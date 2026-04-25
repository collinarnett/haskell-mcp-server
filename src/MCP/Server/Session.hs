{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Per-connection state and the 'McpSession' handle handlers use to talk
-- back to the client. Notifications, sampling, and elicitation all flow
-- through this layer; the transport is responsible for instantiating it.
module MCP.Server.Session
  ( -- * Session state (transport-managed)
    SessionState(..)
  , newSessionState
  , freshRequestId

    -- * Building the session handle
  , mkHttpSession
  , mkStdioSession

    -- * Routing incoming server-request responses
  , routeIncomingResponse

    -- * Progress tokens
  , ProgressToken(..)
  , progressTokenFromValue
  , progressTokenToValue
  ) where

import           Control.Concurrent          (forkIO, threadDelay)
import           Control.Concurrent.MVar     (MVar, newEmptyMVar, putMVar,
                                              takeMVar, tryPutMVar)
import           Control.Concurrent.STM      (TQueue, atomically, newTQueueIO,
                                              writeTQueue)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Data.Aeson                  (Value (..))
import qualified Data.Aeson                  as A
import qualified Data.Aeson.KeyMap           as KM
import           Data.IORef                  (IORef, atomicModifyIORef',
                                              newIORef, readIORef)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Time.Clock.POSIX       (getPOSIXTime)
import           GHC.Generics                (Generic)
import           System.IO                   (hPutStrLn, stderr)

import           MCP.Server.JsonRpc
import           MCP.Server.Types            (LogLevel (..), McpSession (..))

-- | The MCP spec lets a progress token be either a string or a number.
data ProgressToken
  = ProgressTokenText Text
  | ProgressTokenNumber Int
  deriving (Show, Eq, Ord, Generic)

progressTokenFromValue :: Value -> Maybe ProgressToken
progressTokenFromValue (String t) = Just (ProgressTokenText t)
progressTokenFromValue (Number n) = Just (ProgressTokenNumber (round n))
progressTokenFromValue _          = Nothing

progressTokenToValue :: ProgressToken -> Value
progressTokenToValue (ProgressTokenText t)   = String t
progressTokenToValue (ProgressTokenNumber n) = A.Number (fromIntegral n)

-- | Mutable state carried for the lifetime of a single client session. The
-- HTTP transport keeps these keyed by @Mcp-Session-Id@; the stdio transport
-- runs with a single implicit session.
data SessionState = SessionState
  { sessionId           :: Text
    -- ^ Server-issued session id. ASCII visible (0x21–0x7E) per spec.
  , sessionLogLevel     :: IORef LogLevel
    -- ^ Threshold set by @logging/setLevel@; messages below it are dropped.
  , sessionSubscribed   :: IORef (Set Text)
    -- ^ URIs the client called @resources/subscribe@ on.
  , sessionOutbound     :: TQueue JsonRpcMessage
    -- ^ Server-to-client messages awaiting delivery on the SSE GET stream.
  , sessionPendingReqs  :: IORef (Map RequestId (MVar (Either Text Value)))
    -- ^ Server-initiated requests (sampling, elicitation) waiting for a
    -- client-supplied response. Keyed by the request id we generated.
    -- 'Right' carries the @result@ Value; 'Left' carries a server-side
    -- error description (transport disconnect, decode failure, etc.).
  , sessionRequestIdGen :: IORef Int
    -- ^ Monotonic counter for generating fresh server request ids.
  }

-- | Build a fresh 'SessionState' with a freshly-generated session id. The
-- session id is built from a monotonic POSIX timestamp; it is unique per
-- caller, opaque, and stays within the spec's @[0x21–0x7E]@ visible-ASCII
-- range. Used for HTTP sessions; the stdio transport does not need one.
newSessionState :: IO SessionState
newSessionState = do
  -- POSIX time in microseconds, base-36-encoded, makes a short visible-ASCII
  -- session id without pulling in a UUID dependency.
  now <- getPOSIXTime
  let micros :: Integer
      micros = floor (now * 1_000_000)
      sid    = "mcp-" <> base36 micros
  SessionState sid
    <$> newIORef LogInfo
    <*> newIORef Set.empty
    <*> newTQueueIO
    <*> newIORef Map.empty
    <*> newIORef 0

base36 :: Integer -> Text
base36 n
  | n <= 0    = "0"
  | otherwise = T.pack (go n "")
  where
    digits = "0123456789abcdefghijklmnopqrstuvwxyz"
    go 0 acc = acc
    go k acc = let (q, r) = k `quotRem` 36
               in go q (digits !! fromInteger r : acc)

-- | Allocate the next request id this session will use for server-initiated
-- requests. We always issue numeric ids so client correlation is unambiguous.
freshRequestId :: SessionState -> IO RequestId
freshRequestId st = do
  n <- atomicModifyIORef' (sessionRequestIdGen st) (\x -> (x + 1, x + 1))
  pure (RequestIdNumber n)

-- | A session bound to live HTTP transport state. Notifications go onto the
-- session's outbound TQueue, drained by the GET-SSE handler.
mkHttpSession :: SessionState -> Maybe ProgressToken -> McpSession IO
mkHttpSession st mProg = McpSession
  { sendProgress = \progress total message ->
      case mProg of
        Nothing  -> pure ()  -- spec: no progressToken => no notification
        Just tok -> enqueueNotification st "notifications/progress" $
          A.object $
            [ "progressToken" A..= progressTokenToValue tok
            , "progress"      A..= progress
            ] ++ maybe [] (\t -> ["total"   A..= t]) total
              ++ maybe [] (\m -> ["message" A..= m]) message
  , sendLog = \level mLogger d -> do
      threshold <- readIORef (sessionLogLevel st)
      if level >= threshold
        then enqueueNotification st "notifications/message" $
          A.object $
            [ "level" A..= level
            , "data"  A..= d
            ] ++ maybe [] (\l -> ["logger" A..= l]) mLogger
        else pure ()
  , sample = serverInitiatedRequest st "sampling/createMessage"
  , elicit = serverInitiatedRequest st "elicitation/create"
  , currentProgressToken = fmap progressTokenToValue mProg
  }

-- | Issue a server-initiated request to the client and block until the
-- client posts back a matching JSON-RPC response. Times out after 60s
-- so a misbehaving (or disconnected) client can't pin a tool handler
-- forever.
serverInitiatedRequest :: SessionState -> Text -> Value -> IO (Either Text Value)
serverInitiatedRequest st method params = do
  reqId <- freshRequestId st
  reply <- newEmptyMVar
  atomicModifyIORef' (sessionPendingReqs st)
    (\m -> (Map.insert reqId reply m, ()))
  let req = JsonRpcRequest
        { requestJsonrpc = "2.0"
        , requestId      = reqId
        , requestMethod  = method
        , requestParams  = Just params
        }
  atomically $ writeTQueue (sessionOutbound st) (JsonRpcMessageRequest req)
  -- Watchdog: if the client never responds, signal a Left.
  _ <- forkIO $ do
    threadDelay 60_000_000   -- 60 seconds
    _ <- tryPutMVar reply (Left "timed out waiting for client response")
    pure ()
  result <- takeMVar reply
  -- Best-effort cleanup of the pending entry; safe even if already gone.
  atomicModifyIORef' (sessionPendingReqs st)
    (\m -> (Map.delete reqId m, ()))
  pure result

-- | Hand a freshly-decoded JSON-RPC response back to whichever in-flight
-- server-initiated request is waiting for it. The HTTP transport calls
-- this when a client POST contains a response body. If the id is unknown
-- (already timed out, never issued, etc.) the message is dropped.
routeIncomingResponse :: SessionState -> JsonRpcResponse -> IO ()
routeIncomingResponse st resp = do
  m <- readIORef (sessionPendingReqs st)
  case Map.lookup (responseId resp) m of
    Nothing  -> pure ()
    Just mv -> do
      let payload = case responseError resp of
            Just err -> Left (errorMessage err)
            Nothing  -> case responseResult resp of
              Just v  -> Right v
              Nothing -> Left "client response had neither result nor error"
      _ <- tryPutMVar mv payload
      pure ()

-- | A session that is safe to construct on the stdio transport but does
-- nothing with notifications and refuses sampling/elicitation. The conformance
-- suite drives those features over HTTP only — stdio retains the historical
-- pure request/response loop. Polymorphic in the handler monad so it can
-- thread through both pure-IO and 'MonadIO'-constrained user code paths.
mkStdioSession :: MonadIO m => McpSession m
mkStdioSession = McpSession
  { sendProgress = \_ _ _ -> pure ()
  , sendLog      = \level _ d -> liftIO $ hPutStrLn stderr $
      "[stdio:" <> show level <> "] " <> show d
  , sample       = \_ -> pure (Left "sampling not supported on stdio transport")
  , elicit       = \_ -> pure (Left "elicitation not supported on stdio transport")
  , currentProgressToken = Nothing
  }

-- | Push a server-to-client notification onto a session's outbound queue.
enqueueNotification :: SessionState -> Text -> Value -> IO ()
enqueueNotification st method paramsV = do
  -- Wrap the params so we always emit a JSON-RPC notification envelope and
  -- never an empty params field — clients are picky about that.
  let notif = JsonRpcNotification
        { notificationJsonrpc = "2.0"
        , notificationMethod  = method
        , notificationParams  = case paramsV of
            Object km | KM.null km -> Nothing
            v                      -> Just v
        }
  atomically $ writeTQueue (sessionOutbound st) (JsonRpcMessageNotification notif)
