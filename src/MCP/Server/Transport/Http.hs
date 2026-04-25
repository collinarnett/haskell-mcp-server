{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MCP.Server.Transport.Http
  ( -- * HTTP Transport
    HttpConfig(..)
  , transportRunHttp
  , defaultHttpConfig
  ) where

import           Control.Monad            (when)
import           Data.Aeson
import qualified Data.Aeson.KeyMap        as KM
import qualified Data.ByteString.Lazy     as BSL
import           Data.String              (IsString (fromString))
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import           Network.HTTP.Types
import qualified Network.Wai              as Wai
import qualified Network.Wai.Handler.Warp as Warp
import           System.IO                (hPutStrLn, stderr)

import           MCP.Server.Handlers
import           MCP.Server.JsonRpc
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


-- | Transport-specific implementation for HTTP
transportRunHttp :: HttpConfig -> McpServerInfo -> McpServerHandlers IO -> IO ()
transportRunHttp config serverInfo handlers = do
  let settings = Warp.setHost (fromString $ httpHost config) $
                 Warp.setPort (httpPort config) $
                 Warp.defaultSettings

  putStrLn $ "Starting MCP HTTP server on " ++ httpHost config ++ ":" ++ show (httpPort config) ++ httpEndpoint config
  Warp.runSettings settings (mcpApplication config serverInfo handlers)

-- | WAI Application for MCP over HTTP
mcpApplication :: HttpConfig -> McpServerInfo -> McpServerHandlers IO -> Wai.Application
mcpApplication config serverInfo handlers req respond = do
  -- Log the request
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
      then handleMcpRequest config serverInfo handlers req respond
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

-- | Handle MCP requests according to the Streamable HTTP specification.
--
-- The @MCP-Protocol-Version@ header is REQUIRED on requests AFTER initialize.
-- Per the spec the version is negotiated DURING the @initialize@ exchange,
-- so the client cannot know it ahead of time and the initial POST cannot
-- carry the header. GET (discovery) and OPTIONS (CORS preflight) are also
-- exempt. All other POST requests must carry the header set to the server's
-- supported version (2025-06-18).
handleMcpRequest :: HttpConfig -> McpServerInfo -> McpServerHandlers IO -> Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
handleMcpRequest config serverInfo handlers req respond =
  case Wai.requestMethod req of
    -- GET requests for endpoint discovery
    "GET" -> do
      let discoveryResponse = object
            [ "name" .= serverName serverInfo
            , "version" .= serverVersion serverInfo
            , "description" .= serverInstructions serverInfo
            , "protocolVersion" .= ("2025-06-18" :: Text)
            , "capabilities" .= object
                [ "tools" .= object []
                , "prompts" .= object []
                , "resources" .= object []
                ]
            ]
      logVerbose config $ "Sending server discovery response: " ++ show discoveryResponse
      respond $ Wai.responseLBS
        status200
        [("Content-Type", "application/json"), ("Access-Control-Allow-Origin", "*")]
        (encode discoveryResponse)

    -- POST requests for JSON-RPC messages
    "POST" -> do
      body <- Wai.strictRequestBody req
      logVerbose config $ "Received POST body (" ++ show (BSL.length body) ++ " bytes): " ++ take 200 (show body)
      if isInitializeBody body
        then handleJsonRpcRequest config serverInfo handlers body respond
        else case lookup "MCP-Protocol-Version" (Wai.requestHeaders req) of
          Nothing -> do
            logVerbose config "Request rejected: Missing MCP-Protocol-Version header"
            respond $ Wai.responseLBS
              status400
              [("Content-Type", "application/json")]
              (encode $ object ["error" .= ("Missing required MCP-Protocol-Version header" :: Text)])
          Just _ ->
            -- Accept any negotiated version. The actual compatibility check
            -- happens in the initialize handler, which echoes the client's
            -- version when supported. We do not re-gate here because doing
            -- so would require per-session state to know what was negotiated.
            handleJsonRpcRequest config serverInfo handlers body respond

    -- OPTIONS for CORS preflight
    "OPTIONS" -> respond $ Wai.responseLBS
      status200
      [ ("Access-Control-Allow-Origin", "*")
      , ("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
      , ("Access-Control-Allow-Headers", "Content-Type, MCP-Protocol-Version")
      ]
      ""

    -- Unsupported methods
    _ -> respond $ Wai.responseLBS
      status405
      [("Content-Type", "text/plain"), ("Allow", "GET, POST, OPTIONS")]
      "Method Not Allowed"

-- | Cheap pre-parse of the JSON body to detect an @initialize@ request.
-- Returns 'True' iff the top-level object's @method@ field is the string
-- @\"initialize\"@. Any parse error or non-object body returns 'False'.
isInitializeBody :: BSL.ByteString -> Bool
isInitializeBody body = case eitherDecode body of
  Right (Object o) -> case KM.lookup "method" o of
    Just (String "initialize") -> True
    _                          -> False
  _                -> False

-- | Handle JSON-RPC request from HTTP body
handleJsonRpcRequest :: HttpConfig -> McpServerInfo -> McpServerHandlers IO -> BSL.ByteString -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
handleJsonRpcRequest config serverInfo handlers body respond = do
  case eitherDecode body of
    Left err -> do
      hPutStrLn stderr $ "JSON parse error: " ++ err
      respond $ Wai.responseLBS
        status400
        [("Content-Type", "application/json")]
        (encode $ object ["error" .= ("Invalid JSON" :: Text)])

    Right jsonValue -> handleSingleJsonRpc config serverInfo handlers jsonValue respond

-- | Handle a single JSON-RPC message
handleSingleJsonRpc :: HttpConfig -> McpServerInfo -> McpServerHandlers IO -> Value -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
handleSingleJsonRpc config serverInfo handlers jsonValue respond = do
  case parseJsonRpcMessage jsonValue of
    Left err -> do
      hPutStrLn stderr $ "JSON-RPC parse error: " ++ err
      respond $ Wai.responseLBS
        status400
        [("Content-Type", "application/json")]
        (encode $ object ["error" .= ("Invalid JSON-RPC" :: Text)])

    Right message -> do
      logVerbose config $ "Processing HTTP message: " ++ show (getMessageSummary message)
      maybeResponse <- handleMcpMessage serverInfo handlers message

      case maybeResponse of
        Just responseMsg -> do
          let responseJson = encode $ encodeJsonRpcMessage responseMsg
          logVerbose config $ "Sending HTTP response for: " ++ show (getMessageSummary message)
          respond $ Wai.responseLBS
            status200
            [("Content-Type", "application/json"), ("Access-Control-Allow-Origin", "*")]
            responseJson

        Nothing -> do
          logVerbose config $ "No response needed for: " ++ show (getMessageSummary message)
          -- For notifications, return 200 with empty JSON object (per MCP spec)
          respond $ Wai.responseLBS
            status200
            [("Content-Type", "application/json"), ("Access-Control-Allow-Origin", "*")]
            "{}"
