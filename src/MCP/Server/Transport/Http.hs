{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Streamable HTTP transport (MCP 2025-06-18). The server keeps per-client
-- state keyed by an @Mcp-Session-Id@ header, threads an 'McpSession' to each
-- handler, and bridges server-initiated notifications onto an SSE stream so
-- clients can receive progress, log, and (in Group 7) sampling/elicitation
-- messages mid-request.
module MCP.Server.Transport.Http
  ( -- * HTTP Transport
    HttpConfig(..)
  , transportRunHttp
  , defaultHttpConfig
  ) where

import           Control.Concurrent          (forkIO, threadDelay)
import           Control.Concurrent.MVar     (newEmptyMVar, putMVar, readMVar,
                                              tryReadMVar)
import           Control.Concurrent.STM      (TQueue, atomically,
                                              isEmptyTQueue, readTQueue, retry,
                                              tryReadTQueue)
import           Control.Exception           (SomeException, try)
import           Control.Monad               (forM_, unless, when)
import           Data.Aeson
import qualified Data.Aeson.KeyMap           as KM
import qualified Data.ByteString.Builder     as BB
import qualified Data.ByteString.Lazy        as BSL
import           Data.IORef                  (IORef, atomicModifyIORef',
                                              newIORef, readIORef)
import qualified Data.Map.Strict             as Map
import           Data.String                 (IsString (fromString))
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as TE
import           Network.HTTP.Types
import qualified Network.Wai                 as Wai
import qualified Network.Wai.Handler.Warp    as Warp
import           System.IO                   (hPutStrLn, stderr)

import           MCP.Server.Handlers
import           MCP.Server.JsonRpc
import           MCP.Server.Session
import           MCP.Server.Types

-- | HTTP transport configuration following MCP 2025-06-18 Streamable HTTP specification
data HttpConfig = HttpConfig
  { httpPort     :: Int      -- ^ Port to listen on
  , httpHost     :: String   -- ^ Host to bind to (default "localhost")
  , httpEndpoint :: String   -- ^ MCP endpoint path (default "/mcp")
  , httpVerbose  :: Bool     -- ^ Enable verbose logging (default False)
  } deriving (Show, Eq)

-- | Default HTTP configuration
defaultHttpConfig :: HttpConfig
defaultHttpConfig = HttpConfig
  { httpPort = 3000
  , httpHost = "localhost"
  , httpEndpoint = "/mcp"
  , httpVerbose = False
  }

-- | Helper for conditional logging
logVerbose :: HttpConfig -> String -> IO ()
logVerbose config msg = when (httpVerbose config) $ hPutStrLn stderr msg

-- | Map of live sessions keyed by their @Mcp-Session-Id@ header value.
type SessionMap = IORef (Map.Map Text SessionState)

-- | Transport-specific implementation for HTTP
transportRunHttp :: HttpConfig -> McpServerInfo -> McpServerHandlers IO -> IO ()
transportRunHttp config serverInfo handlers = do
  sessions <- newIORef Map.empty
  let settings = Warp.setHost (fromString $ httpHost config) $
                 Warp.setPort (httpPort config) $
                 Warp.defaultSettings

  putStrLn $ "Starting MCP HTTP server on " ++ httpHost config ++ ":" ++ show (httpPort config) ++ httpEndpoint config
  Warp.runSettings settings (mcpApplication config serverInfo handlers sessions)

-- | WAI Application for MCP over HTTP
mcpApplication :: HttpConfig -> McpServerInfo -> McpServerHandlers IO -> SessionMap -> Wai.Application
mcpApplication config serverInfo handlers sessions req respond = do
  logVerbose config $ "HTTP " ++ show (Wai.requestMethod req) ++ " " ++ T.unpack (TE.decodeUtf8 $ Wai.rawPathInfo req)

  -- DNS rebinding protection (per spec security best practices): when the
  -- server is bound to a loopback host, reject requests whose Host or Origin
  -- header points elsewhere. This stops attackers from coercing local
  -- browsers into talking to a local-only MCP server via DNS rebinding.
  if not (isAllowedHostHeader config req) || not (isAllowedOriginHeader config req)
    then do
      logVerbose config "Request rejected: invalid Host/Origin for loopback bind"
      respond $ Wai.responseLBS
        status403
        [("Content-Type", "application/json")]
        (encode $ object ["error" .= ("Forbidden: invalid Host or Origin header" :: Text)])
    -- Check if this is our MCP endpoint
    else if TE.decodeUtf8 (Wai.rawPathInfo req) == T.pack (httpEndpoint config)
      then handleMcpRequest config serverInfo handlers sessions req respond
      else respond $ Wai.responseLBS status404 [("Content-Type", "text/plain")] "Not Found"

-- | Names that are always considered "loopback" regardless of configured host.
loopbackHostNames :: [Text]
loopbackHostNames = ["localhost", "127.0.0.1", "[::1]", "::1"]

-- | Validate the Host header against the configured loopback bind. If the
-- server is bound to 0.0.0.0 (all interfaces) we don't enforce; otherwise we
-- require the Host's hostname portion to match the configured host or one of
-- the standard loopback names.
isAllowedHostHeader :: HttpConfig -> Wai.Request -> Bool
isAllowedHostHeader config req
  | httpHost config == "0.0.0.0" = True
  | otherwise = case lookup "Host" (Wai.requestHeaders req) of
      Nothing -> True   -- HTTP/1.0 client may omit Host
      Just h  ->
        let hostName = T.takeWhile (/= ':') (TE.decodeUtf8 h)
        in hostName `elem` loopbackHostNames
           || hostName == T.pack (httpHost config)

-- | Validate the Origin header (if present). Same logic as Host.
isAllowedOriginHeader :: HttpConfig -> Wai.Request -> Bool
isAllowedOriginHeader config req
  | httpHost config == "0.0.0.0" = True
  | otherwise = case lookup "Origin" (Wai.requestHeaders req) of
      Nothing -> True   -- non-browser clients omit Origin
      Just o  ->
        let originText = TE.decodeUtf8 o
            -- Strip scheme then take up to first ':' or '/'
            hostName = T.takeWhile (\c -> c /= ':' && c /= '/')
                     $ T.dropWhile (== '/')
                     $ snd (T.breakOnEnd "//" originText)
        in hostName `elem` loopbackHostNames
           || hostName == T.pack (httpHost config)

-- | Headers to expose on responses for browser-based clients.
corsHeaders :: ResponseHeaders
corsHeaders =
  [ ("Access-Control-Allow-Origin",      "*")
  , ("Access-Control-Allow-Headers",     "Content-Type, MCP-Protocol-Version, Mcp-Session-Id")
  , ("Access-Control-Expose-Headers",    "Mcp-Session-Id")
  ]

-- | Dispatch the inbound HTTP method (GET / POST / DELETE / OPTIONS).
handleMcpRequest :: HttpConfig
                 -> McpServerInfo
                 -> McpServerHandlers IO
                 -> SessionMap
                 -> Wai.Request
                 -> (Wai.Response -> IO Wai.ResponseReceived)
                 -> IO Wai.ResponseReceived
handleMcpRequest config serverInfo handlers sessions req respond =
  case Wai.requestMethod req of
    -- GET requests open an SSE stream the server uses to push notifications
    -- and (Group 7) sampling/elicitation requests. If the Accept header does
    -- not request text/event-stream we fall back to the legacy discovery
    -- payload.
    "GET" -> case lookup "Accept" (Wai.requestHeaders req) of
      Just accept | "text/event-stream" `T.isInfixOf` TE.decodeUtf8 accept ->
        handleSseGet config sessions req respond
      _ -> handleDiscoveryGet config serverInfo req respond

    -- POST requests carry JSON-RPC messages.
    "POST" -> do
      body <- Wai.strictRequestBody req
      logVerbose config $ "Received POST body (" ++ show (BSL.length body) ++ " bytes): " ++ take 200 (show body)
      if isInitializeBody body
        then handleInitializePost config serverInfo handlers sessions body respond
        else case lookup "MCP-Protocol-Version" (Wai.requestHeaders req) of
          Nothing -> do
            logVerbose config "Request rejected: Missing MCP-Protocol-Version header"
            respond $ Wai.responseLBS
              status400
              (("Content-Type", "application/json") : corsHeaders)
              (encode $ object ["error" .= ("Missing required MCP-Protocol-Version header" :: Text)])
          Just _ ->
            handlePostBody config serverInfo handlers sessions req body respond

    -- DELETE tears down the named session.
    "DELETE" -> case lookupSessionIdHeader req of
      Nothing -> respond $ Wai.responseLBS
        status400
        (("Content-Type", "application/json") : corsHeaders)
        (encode $ object ["error" .= ("DELETE requires Mcp-Session-Id" :: Text)])
      Just sid -> do
        atomicModifyIORef' sessions (\m -> (Map.delete sid m, ()))
        respond $ Wai.responseLBS status200 corsHeaders ""

    -- OPTIONS for CORS preflight
    "OPTIONS" -> respond $ Wai.responseLBS
      status200
      (corsHeaders ++
        [ ("Access-Control-Allow-Methods", "GET, POST, DELETE, OPTIONS")
        ])
      ""

    _ -> respond $ Wai.responseLBS
      status405
      (("Content-Type", "text/plain") : ("Allow", "GET, POST, DELETE, OPTIONS") : corsHeaders)
      "Method Not Allowed"

-- | The legacy GET discovery payload — kept for clients that probe the
-- endpoint without opening an SSE stream. The conformance suite does not
-- exercise this path.
handleDiscoveryGet :: HttpConfig -> McpServerInfo -> Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
handleDiscoveryGet config serverInfo _req respond = do
  let discoveryResponse = object
        [ "name"            .= serverName serverInfo
        , "version"         .= serverVersion serverInfo
        , "description"     .= serverInstructions serverInfo
        , "protocolVersion" .= ("2025-06-18" :: Text)
        ]
  logVerbose config "Sending server discovery response"
  respond $ Wai.responseLBS
    status200
    (("Content-Type", "application/json") : corsHeaders)
    (encode discoveryResponse)

-- | Open a Server-Sent Events stream that drains the named session's
-- outbound TQueue. The stream stays open until the client disconnects;
-- an idle keep-alive comment is emitted every 30s so intermediaries don't
-- close us out for inactivity.
handleSseGet :: HttpConfig
             -> SessionMap
             -> Wai.Request
             -> (Wai.Response -> IO Wai.ResponseReceived)
             -> IO Wai.ResponseReceived
handleSseGet config sessions req respond = case lookupSessionIdHeader req of
  Nothing -> respond $ Wai.responseLBS
    status400
    (("Content-Type", "application/json") : corsHeaders)
    (encode $ object ["error" .= ("GET stream requires Mcp-Session-Id" :: Text)])
  Just sid -> do
    sm <- readIORef sessions
    case Map.lookup sid sm of
      Nothing -> respond $ Wai.responseLBS
        status404
        (("Content-Type", "application/json") : corsHeaders)
        (encode $ object ["error" .= ("Unknown session" :: Text)])
      Just st -> respond $ Wai.responseStream
        status200
        (("Content-Type", "text/event-stream") :
         ("Cache-Control", "no-cache, no-transform") :
         ("Connection", "keep-alive") :
         corsHeaders)
        (\write flush -> do
            logVerbose config $ "SSE GET stream opened for session " <> T.unpack sid
            sseDrainLoop write flush (sessionOutbound st))

-- | Drain a session's outbound queue forever, writing each message as a
-- @data:@ SSE event. Polls every 100ms; emits an SSE keep-alive comment if
-- 30 seconds have elapsed without a real message so intermediaries don't
-- close the connection.
sseDrainLoop :: (BB.Builder -> IO ())
             -> IO ()
             -> TQueue JsonRpcMessage
             -> IO ()
sseDrainLoop write flush q = loopWith 0
  where
    pollMicros, keepAliveMicros :: Int
    pollMicros      = 100_000        -- 100 ms
    keepAliveMicros = 30_000_000     -- 30 s

    loopWith idleMicros = do
      mv <- atomically (tryReadTQueue q)
      case mv of
        Just msg -> do
          writeSseEvent write msg
          flush
          loopWith 0
        Nothing -> do
          let idle' = idleMicros + pollMicros
          if idle' >= keepAliveMicros
            then do
              write (BB.byteString ": keep-alive\n\n")
              flush
              threadDelay pollMicros
              loopWith 0
            else do
              threadDelay pollMicros
              loopWith idle'

-- | Format a 'JsonRpcMessage' as an SSE @data:@ event.
writeSseEvent :: (BB.Builder -> IO ()) -> JsonRpcMessage -> IO ()
writeSseEvent write msg = do
  let bytes = encode (encodeJsonRpcMessage msg)
  write (BB.byteString "data: ")
  write (BB.lazyByteString bytes)
  write (BB.byteString "\n\n")

-- | Cheap pre-parse of the JSON body to detect an @initialize@ request.
isInitializeBody :: BSL.ByteString -> Bool
isInitializeBody body = case eitherDecode body of
  Right (Object o) -> case KM.lookup "method" o of
    Just (String "initialize") -> True
    _                          -> False
  _                -> False

-- | The first POST a client makes is @initialize@. We allocate a session
-- here, dispatch the request with a session-bound builder, and return the
-- @Mcp-Session-Id@ in the response so the client can use it on subsequent
-- requests.
handleInitializePost :: HttpConfig
                     -> McpServerInfo
                     -> McpServerHandlers IO
                     -> SessionMap
                     -> BSL.ByteString
                     -> (Wai.Response -> IO Wai.ResponseReceived)
                     -> IO Wai.ResponseReceived
handleInitializePost config serverInfo handlers sessions body respond = do
  st <- newSessionState
  atomicModifyIORef' sessions (\m -> (Map.insert (sessionId st) st m, ()))
  let mkSession mProgVal = mkHttpSession st (mProgVal >>= progressTokenFromValue)
  case eitherDecode body of
    Left err -> do
      hPutStrLn stderr $ "JSON parse error: " ++ err
      respond $ Wai.responseLBS
        status400
        (("Content-Type", "application/json") : corsHeaders)
        (encode $ object ["error" .= ("Invalid JSON" :: Text)])
    Right jsonValue -> do
      case parseJsonRpcMessage jsonValue of
        Left err -> do
          hPutStrLn stderr $ "JSON-RPC parse error: " ++ err
          respond $ Wai.responseLBS
            status400
            (("Content-Type", "application/json") : corsHeaders)
            (encode $ object ["error" .= ("Invalid JSON-RPC" :: Text)])
        Right message -> do
          maybeResponse <- handleMcpMessage serverInfo handlers mkSession message
          case maybeResponse of
            Just responseMsg ->
              respond $ Wai.responseLBS
                status200
                (("Content-Type", "application/json") :
                 ("Mcp-Session-Id", TE.encodeUtf8 (sessionId st)) :
                 corsHeaders)
                (encode $ encodeJsonRpcMessage responseMsg)
            Nothing ->
              respond $ Wai.responseLBS
                status202
                (("Mcp-Session-Id", TE.encodeUtf8 (sessionId st)) : corsHeaders)
                ""

-- | Subsequent (post-initialize) POSTs. The client must include
-- @Mcp-Session-Id@; we look up the session, dispatch the message, and
-- return either a JSON response (for non-streaming requests) or an SSE
-- stream (for tool calls that may emit progress / log notifications).
handlePostBody :: HttpConfig
               -> McpServerInfo
               -> McpServerHandlers IO
               -> SessionMap
               -> Wai.Request
               -> BSL.ByteString
               -> (Wai.Response -> IO Wai.ResponseReceived)
               -> IO Wai.ResponseReceived
handlePostBody config serverInfo handlers sessions req body respond = do
  -- Mcp-Session-Id is optional for compatibility — clients that ignore
  -- the header still flow through with a one-shot anonymous session.
  st <- case lookupSessionIdHeader req of
    Just sid -> do
      sm <- readIORef sessions
      case Map.lookup sid sm of
        Just s  -> pure s
        Nothing -> do
          fresh <- newSessionState
          atomicModifyIORef' sessions (\m -> (Map.insert sid (fresh { sessionId = sid }) m, ()))
          pure fresh { sessionId = sid }
    Nothing -> do
      fresh <- newSessionState
      atomicModifyIORef' sessions (\m -> (Map.insert (sessionId fresh) fresh m, ()))
      pure fresh
  let mkSession mProgVal = mkHttpSession st (mProgVal >>= progressTokenFromValue)

  case eitherDecode body of
    Left err -> do
      hPutStrLn stderr $ "JSON parse error: " ++ err
      respond $ Wai.responseLBS
        status400
        (("Content-Type", "application/json") : corsHeaders)
        (encode $ object ["error" .= ("Invalid JSON" :: Text)])

    Right jsonValue -> case parseJsonRpcMessage jsonValue of
      Left err -> do
        hPutStrLn stderr $ "JSON-RPC parse error: " ++ err
        respond $ Wai.responseLBS
          status400
          (("Content-Type", "application/json") : corsHeaders)
          (encode $ object ["error" .= ("Invalid JSON-RPC" :: Text)])

      Right message -> do
        logVerbose config $ "Processing HTTP message: " ++ show (getMessageSummary message)
        case message of
          -- Tool calls may emit notifications mid-flight. Stream the response
          -- via SSE so notifications and the final result share one stream.
          JsonRpcMessageRequest r | requestMethod r == "tools/call" ->
            streamToolCall config serverInfo handlers st mkSession message respond
          _ -> do
            maybeResponse <- handleMcpMessage serverInfo handlers mkSession message
            case maybeResponse of
              Just responseMsg ->
                respond $ Wai.responseLBS
                  status200
                  (("Content-Type", "application/json") : corsHeaders)
                  (encode $ encodeJsonRpcMessage responseMsg)
              Nothing ->
                respond $ Wai.responseLBS status202 corsHeaders ""

-- | Stream a tool-call response as SSE so progress / log notifications the
-- handler emits land on the same connection as the final response.
streamToolCall :: HttpConfig
               -> McpServerInfo
               -> McpServerHandlers IO
               -> SessionState
               -> (Maybe Value -> McpSession IO)
               -> JsonRpcMessage
               -> (Wai.Response -> IO Wai.ResponseReceived)
               -> IO Wai.ResponseReceived
streamToolCall _config serverInfo handlers st mkSession message respond =
  respond $ Wai.responseStream
    status200
    (("Content-Type", "text/event-stream") :
     ("Cache-Control", "no-cache, no-transform") :
     ("Connection", "keep-alive") :
     corsHeaders)
    (\write flush -> do
        resultVar <- newEmptyMVar
        -- Run the handler in a forked thread so we can interleave SSE
        -- notification flushes with handler progress.
        _ <- forkIO $ do
          r <- handleMcpMessage serverInfo handlers mkSession message
          putMVar resultVar r
        let drain = do
              -- Pull every queued message synchronously, then poll the
              -- result MVar; if the handler is still running, sleep
              -- briefly and loop.
              flushQueue write flush (sessionOutbound st)
              done <- tryReadMVar resultVar
              case done of
                Just resp -> pure resp
                Nothing   -> threadDelay 5_000 >> drain
        finalResp <- drain
        -- Drain anything queued *after* the handler returned but before
        -- we read the MVar.
        flushQueue write flush (sessionOutbound st)
        case finalResp of
          Just msg -> writeSseEvent write msg >> flush
          Nothing  -> pure ()
    )

-- | Pull every message currently sitting in the queue and SSE-emit it.
flushQueue :: (BB.Builder -> IO ()) -> IO () -> TQueue JsonRpcMessage -> IO ()
flushQueue write flush q = do
  let pumpOne = do
        mv <- atomically (tryReadTQueue q)
        case mv of
          Nothing -> pure False
          Just m  -> do
            writeSseEvent write m
            flush
            pure True
      loop = do
        more <- pumpOne
        when more loop
  loop

lookupSessionIdHeader :: Wai.Request -> Maybe Text
lookupSessionIdHeader req =
  TE.decodeUtf8 <$> lookup "Mcp-Session-Id" (Wai.requestHeaders req)
