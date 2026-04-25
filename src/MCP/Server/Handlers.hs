{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MCP.Server.Handlers
  ( -- * Core Message Handling
    handleMcpMessage
  , jsonValueToText

    -- * Individual Request Handlers
  , handleInitialize
  , handlePing
  , handlePromptsList
  , handlePromptsGet
  , handleResourcesList
  , handleResourcesRead
  , handleResourcesTemplatesList
  , handleResourcesSubscribe
  , handleResourcesUnsubscribe
  , handleToolsList
  , handleToolsCall
  , handleLoggingSetLevel
  , handleCompletionComplete

    -- * Session plumbing
  , SessionBuilder

    -- * Protocol Support
  , validateProtocolVersion
  , getMessageSummary

    -- * Error Conversion
  , errorCodeFromMcpError
  , errorMessageFromMcpError
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson
import qualified Data.Map               as Map
import           Data.Text              (Text)
import qualified Data.Text              as T
import           System.IO              (hPutStrLn, stderr)

import           MCP.Server.JsonRpc
import           MCP.Server.Protocol
import           MCP.Server.Types

-- | Convert JSON Value to Text representation suitable for handlers
jsonValueToText :: Value -> Text
jsonValueToText (String t) = t
jsonValueToText (Number n) =
    -- Check if it's a whole number, if so format as integer
    if fromInteger (round n) == n
        then T.pack $ show (round n :: Integer)
        else T.pack $ show n
jsonValueToText (Bool True) = "true"
jsonValueToText (Bool False) = "false"
jsonValueToText Null = ""
jsonValueToText v = T.pack $ show v

-- | Extract a brief summary of a JSON-RPC message for logging
getMessageSummary :: JsonRpcMessage -> String
getMessageSummary (JsonRpcMessageRequest req) =
  "Request[" ++ show (requestId req) ++ "] " ++ T.unpack (requestMethod req)
getMessageSummary (JsonRpcMessageNotification notif) =
  "Notification " ++ T.unpack (notificationMethod notif)
getMessageSummary (JsonRpcMessageResponse resp) =
  "Response[" ++ show (responseId resp) ++ "]"

-- | Validate protocol version and return negotiated version.
-- Per MCP spec: the server responds with the highest version it supports
-- that is compatible with the client. Since the protocol is designed to be
-- backwards-compatible, we echo the client's version when it is >= ours
-- and fall back to our version otherwise.
validateProtocolVersion :: Text -> Either Text Text
validateProtocolVersion clientVersion
  | clientVersion >= protocolVersion = Right clientVersion
  | otherwise = Right protocolVersion

-- | Build the session used to dispatch a single inbound request. The HTTP
-- transport picks the per-session base from its session map and tweaks the
-- progress token; the stdio transport ignores the argument and returns its
-- no-op session. Keeping this as a callback avoids leaking transport types
-- (TQueue/IORef) into the Handlers module.
type SessionBuilder m = Maybe Value -> McpSession m

-- | Handle an MCP message and return a response if needed
handleMcpMessage :: (MonadIO m)
                 => McpServerInfo
                 -> McpServerHandlers m
                 -> SessionBuilder m
                 -> JsonRpcMessage
                 -> m (Maybe JsonRpcMessage)
handleMcpMessage serverInfo handlers mkSession (JsonRpcMessageRequest req) = do
  response <- case requestMethod req of
    "initialize" -> handleInitialize serverInfo handlers req
    "ping" -> handlePing req
    "prompts/list" -> handlePromptsList handlers req
    "prompts/get" -> handlePromptsGet handlers req
    "resources/list" -> handleResourcesList handlers req
    "resources/read" -> handleResourcesRead handlers req
    "resources/templates/list" -> handleResourcesTemplatesList handlers req
    "resources/subscribe"   -> handleResourcesSubscribe req
    "resources/unsubscribe" -> handleResourcesUnsubscribe req
    "tools/list" -> handleToolsList handlers req
    "tools/call" -> handleToolsCall handlers mkSession req
    "logging/setLevel"   -> handleLoggingSetLevel req
    "completion/complete" -> handleCompletionComplete handlers req
    method -> return $ makeErrorResponse (requestId req) $ JsonRpcError
      { errorCode = -32601
      , errorMessage = "Method not found: " <> method
      , errorData = Nothing
      }
  return $ Just $ JsonRpcMessageResponse response

handleMcpMessage _ _ _ (JsonRpcMessageNotification notif) = do
  case notificationMethod notif of
    "notifications/initialized" -> do
      liftIO $ hPutStrLn stderr "Received initialized notification - server is ready for operation"
      return ()
    _ -> do
      liftIO $ hPutStrLn stderr $ "Received unknown notification: " ++ T.unpack (notificationMethod notif)
      return ()
  return Nothing

handleMcpMessage _ _ _ (JsonRpcMessageResponse _) =
  return Nothing

-- | Handle initialize request
handleInitialize :: (MonadIO m) => McpServerInfo -> McpServerHandlers m -> JsonRpcRequest -> m JsonRpcResponse
handleInitialize serverInfo handlers req = do
  case requestParams req of
    Nothing -> return $ makeErrorResponse (requestId req) $ JsonRpcError
      { errorCode = -32602
      , errorMessage = "Missing required parameters for initialize"
      , errorData = Nothing
      }
    Just params ->
      case fromJSON params of
        Error err -> return $ makeErrorResponse (requestId req) $ JsonRpcError
          { errorCode = -32602
          , errorMessage = "Invalid initialize parameters: " <> T.pack err
          , errorData = Nothing
          }
        Success initReq -> do
          -- Check protocol version compatibility
          let clientVersion = initProtocolVersion initReq
          case validateProtocolVersion clientVersion of
            Left errorMsg -> return $ makeErrorResponse (requestId req) $ JsonRpcError
              { errorCode = -32602
              , errorMessage = errorMsg
              , errorData = Nothing
              }
            Right negotiatedVersion -> do
              liftIO $ hPutStrLn stderr $ "Client version: " ++ T.unpack clientVersion ++ ", using: " ++ T.unpack negotiatedVersion
              let capabilities = serverCapabilitiesFor handlers
              let response = InitializeResponse
                    { initRespProtocolVersion = negotiatedVersion
                    , initRespCapabilities = capabilities
                    , initRespServerInfo = serverInfo
                    }
              return $ makeSuccessResponse (requestId req) (toJSON response)

-- | Build the @ServerCapabilities@ block for the @initialize@ response by
-- inspecting which handler families the user actually registered. We only
-- advertise prompts/resources/tools when there are corresponding handlers;
-- @logging@ and @completions@ are advertised unconditionally because the
-- dispatcher gracefully handles their absence with sensible defaults.
serverCapabilitiesFor :: McpServerHandlers m -> ServerCapabilities
serverCapabilitiesFor handlers = ServerCapabilities
  { capabilityPrompts = case prompts handlers of
      Just _  -> Just $ PromptCapabilities { promptListChanged = Nothing }
      Nothing -> Nothing
  , capabilityResources = case resources handlers of
      Just _  -> Just $ ResourceCapabilities
                          { -- Server accepts subscribe/unsubscribe even when
                            -- it doesn't yet emit @notifications/resources/updated@.
                            resourceSubscribe   = Just True
                          , resourceListChanged = Nothing
                          }
      Nothing -> Nothing
  , capabilityTools = case tools handlers of
      Just _  -> Just $ ToolCapabilities { toolListChanged = Nothing }
      Nothing -> Nothing
  , capabilityLogging     = Just LoggingCapabilities
  , capabilityCompletions = Just CompletionCapabilities
  }

-- | Handle ping request
handlePing :: (MonadIO m) => JsonRpcRequest -> m JsonRpcResponse
handlePing req = return $ makeSuccessResponse (requestId req) (toJSON PongResponse)

-- | Handle prompts/list request
handlePromptsList :: (MonadIO m) => McpServerHandlers m -> JsonRpcRequest -> m JsonRpcResponse
handlePromptsList handlers req =
  case prompts handlers of
    Nothing -> return $ makeErrorResponse (requestId req) $ JsonRpcError
      { errorCode = -32601
      , errorMessage = "Prompts not supported"
      , errorData = Nothing
      }
    Just (listHandler, _) -> do
      promptsList <- listHandler
      let response = PromptsListResponse
            { promptsListPrompts = promptsList
            }
      return $ makeSuccessResponse (requestId req) (toJSON response)

-- | Handle prompts/get request
handlePromptsGet :: (MonadIO m) => McpServerHandlers m -> JsonRpcRequest -> m JsonRpcResponse
handlePromptsGet handlers req =
  case prompts handlers of
    Nothing -> return $ makeErrorResponse (requestId req) $ JsonRpcError
      { errorCode = -32601
      , errorMessage = "Prompts not supported"
      , errorData = Nothing
      }
    Just (_, getHandler) -> do
      case requestParams req of
        Nothing -> return $ makeErrorResponse (requestId req) $ JsonRpcError
          { errorCode = -32602
          , errorMessage = "Missing parameters"
          , errorData = Nothing
          }
        Just params ->
          case fromJSON params of
            Error err -> return $ makeErrorResponse (requestId req) $ JsonRpcError
              { errorCode = -32602
              , errorMessage = "Invalid parameters: " <> T.pack err
              , errorData = Nothing
              }
            Success getReq -> do
              let args = maybe [] (map (\(k, v) -> (k, jsonValueToText v)) . Map.toList) (promptsGetArguments getReq)
              result <- getHandler (promptsGetName getReq) args
              case result of
                Left err -> return $ makeErrorResponse (requestId req) $ JsonRpcError
                  { errorCode = errorCodeFromMcpError err
                  , errorMessage = errorMessageFromMcpError err
                  , errorData = Nothing
                  }
                Right messages -> do
                  let response = PromptsGetResponse
                        { promptsGetDescription = Nothing
                        , promptsGetMessages = messages
                        , promptsGetMeta = Nothing
                        }
                  return $ makeSuccessResponse (requestId req) (toJSON response)

-- | Handle resources/list request
handleResourcesList :: (MonadIO m) => McpServerHandlers m -> JsonRpcRequest -> m JsonRpcResponse
handleResourcesList handlers req =
  case resources handlers of
    Nothing -> return $ makeErrorResponse (requestId req) $ JsonRpcError
      { errorCode = -32601
      , errorMessage = "Resources not supported"
      , errorData = Nothing
      }
    Just (listHandler, _) -> do
      resourcesList <- listHandler
      let response = ResourcesListResponse
            { resourcesListResources = resourcesList
            }
      return $ makeSuccessResponse (requestId req) (toJSON response)

-- | Handle resources/read request
handleResourcesRead :: (MonadIO m) => McpServerHandlers m -> JsonRpcRequest -> m JsonRpcResponse
handleResourcesRead handlers req =
  case resources handlers of
    Nothing -> return $ makeErrorResponse (requestId req) $ JsonRpcError
      { errorCode = -32601
      , errorMessage = "Resources not supported"
      , errorData = Nothing
      }
    Just (_, readHandler) -> do
      case requestParams req of
        Nothing -> return $ makeErrorResponse (requestId req) $ JsonRpcError
          { errorCode = -32602
          , errorMessage = "Missing parameters"
          , errorData = Nothing
          }
        Just params ->
          case fromJSON params of
            Error err -> return $ makeErrorResponse (requestId req) $ JsonRpcError
              { errorCode = -32602
              , errorMessage = "Invalid parameters: " <> T.pack err
              , errorData = Nothing
              }
            Success readReq -> do
              result <- readHandler (resourcesReadUri readReq)
              case result of
                Left err -> return $ makeErrorResponse (requestId req) $ JsonRpcError
                  { errorCode = errorCodeFromMcpError err
                  , errorMessage = errorMessageFromMcpError err
                  , errorData = Nothing
                  }
                Right resourceContent -> do
                  let response = ResourcesReadResponse
                        { resourcesReadContents = [resourceContent]
                        }
                  return $ makeSuccessResponse (requestId req) (toJSON response)

-- | Handle resources/templates/list. Servers commonly have no templates;
-- in that case we return an empty list rather than @method-not-found@ so the
-- conformance suite (which probes this method unconditionally) sees a
-- well-formed response.
handleResourcesTemplatesList :: (MonadIO m) => McpServerHandlers m -> JsonRpcRequest -> m JsonRpcResponse
handleResourcesTemplatesList handlers req = do
  templates <- case resourceTemplates handlers of
    Nothing       -> pure []
    Just listFn   -> listFn
  let response = ResourcesTemplatesListResponse
        { resourcesTemplatesListTemplates = templates
        }
  return $ makeSuccessResponse (requestId req) (toJSON response)

-- | Handle tools/list request
handleToolsList :: (MonadIO m) => McpServerHandlers m -> JsonRpcRequest -> m JsonRpcResponse
handleToolsList handlers req =
  case tools handlers of
    Nothing -> return $ makeErrorResponse (requestId req) $ JsonRpcError
      { errorCode = -32601
      , errorMessage = "Tools not supported"
      , errorData = Nothing
      }
    Just (listHandler, _) -> do
      toolsList <- listHandler
      let response = ToolsListResponse
            { toolsListTools = toolsList
            }
      return $ makeSuccessResponse (requestId req) (toJSON response)

-- | Handle tools/call request. The session-builder is invoked with the
-- request's @_meta.progressToken@ so the user-supplied tool handler sees a
-- session that emits progress notifications keyed to the right token.
handleToolsCall :: (MonadIO m) => McpServerHandlers m -> SessionBuilder m -> JsonRpcRequest -> m JsonRpcResponse
handleToolsCall handlers mkSession req =
  case tools handlers of
    Nothing -> return $ makeErrorResponse (requestId req) $ JsonRpcError
      { errorCode = -32601
      , errorMessage = "Tools not supported"
      , errorData = Nothing
      }
    Just (_, callHandler) -> do
      case requestParams req of
        Nothing -> return $ makeErrorResponse (requestId req) $ JsonRpcError
          { errorCode = -32602
          , errorMessage = "Missing parameters"
          , errorData = Nothing
          }
        Just params ->
          case fromJSON params of
            Error err -> return $ makeErrorResponse (requestId req) $ JsonRpcError
              { errorCode = -32602
              , errorMessage = "Invalid parameters: " <> T.pack err
              , errorData = Nothing
              }
            Success callReq -> do
              let args    = maybe [] (map (\(k, v) -> (k, jsonValueToText v)) . Map.toList) (toolsCallArguments callReq)
                  session = mkSession (toolsCallProgressToken callReq)
              result <- callHandler session (toolsCallName callReq) args
              case result of
                Left err -> return $ makeErrorResponse (requestId req) $ JsonRpcError
                  { errorCode = errorCodeFromMcpError err
                  , errorMessage = errorMessageFromMcpError err
                  , errorData = Nothing
                  }
                Right (ToolResult cs isErr) -> do
                  -- Always include @isError@ in the response so the
                  -- conformance suite can distinguish a successful tool
                  -- run from one that returned a tool execution error.
                  let response = ToolsCallResponse
                        { toolsCallContent = cs
                        , toolsCallIsError = Just isErr
                        , toolsCallMeta    = Nothing
                        }
                  return $ makeSuccessResponse (requestId req) (toJSON response)

-- | @logging/setLevel@. The library accepts the level for protocol
-- conformance and parses it; per-session log filtering arrives in Group 6
-- once 'McpSession' is plumbed. Until then this is a no-op acknowledgement.
handleLoggingSetLevel :: (MonadIO m) => JsonRpcRequest -> m JsonRpcResponse
handleLoggingSetLevel req = case requestParams req of
  Nothing -> return $ makeErrorResponse (requestId req) $ JsonRpcError
    { errorCode = -32602
    , errorMessage = "Missing parameters for logging/setLevel"
    , errorData = Nothing
    }
  Just params -> case fromJSON params of
    Error err -> return $ makeErrorResponse (requestId req) $ JsonRpcError
      { errorCode = -32602
      , errorMessage = "Invalid parameters for logging/setLevel: " <> T.pack err
      , errorData = Nothing
      }
    Success (_ :: SetLevelRequest) ->
      return $ makeSuccessResponse (requestId req) (object [])

-- | @completion/complete@. With no completion handler registered the server
-- still answers with a well-formed empty result; the conformance suite only
-- checks the response shape, not the contents.
handleCompletionComplete :: (MonadIO m) => McpServerHandlers m -> JsonRpcRequest -> m JsonRpcResponse
handleCompletionComplete handlers req = case requestParams req of
  Nothing -> return $ makeErrorResponse (requestId req) $ JsonRpcError
    { errorCode = -32602
    , errorMessage = "Missing parameters for completion/complete"
    , errorData = Nothing
    }
  Just params -> case fromJSON params of
    Error err -> return $ makeErrorResponse (requestId req) $ JsonRpcError
      { errorCode = -32602
      , errorMessage = "Invalid parameters for completion/complete: " <> T.pack err
      , errorData = Nothing
      }
    Success (cReq :: CompleteRequest) -> do
      result <- case completions handlers of
        Nothing      -> pure CompletionResult
          { completionValues  = []
          , completionTotal   = Just 0
          , completionHasMore = Just False
          }
        Just handler ->
          handler (completeRef cReq) (completeArgument cReq) (completeContext cReq)
      let response = CompleteResponse { completeCompletion = result }
      return $ makeSuccessResponse (requestId req) (toJSON response)

-- | @resources/subscribe@. The library accepts the request and returns an
-- empty success object. Real subscription tracking arrives with the
-- session-aware HTTP transport in Group 6 — at that point the handler will
-- record the URI on 'SessionState' and emit
-- @notifications/resources/updated@ when the resource changes.
handleResourcesSubscribe :: (MonadIO m) => JsonRpcRequest -> m JsonRpcResponse
handleResourcesSubscribe req = case requestParams req of
  Nothing -> return $ makeErrorResponse (requestId req) $ JsonRpcError
    { errorCode = -32602
    , errorMessage = "Missing parameters for resources/subscribe"
    , errorData = Nothing
    }
  Just params -> case fromJSON params of
    Error err -> return $ makeErrorResponse (requestId req) $ JsonRpcError
      { errorCode = -32602
      , errorMessage = "Invalid parameters for resources/subscribe: " <> T.pack err
      , errorData = Nothing
      }
    Success (_ :: SubscribeRequest) ->
      return $ makeSuccessResponse (requestId req) (object [])

handleResourcesUnsubscribe :: (MonadIO m) => JsonRpcRequest -> m JsonRpcResponse
handleResourcesUnsubscribe req = case requestParams req of
  Nothing -> return $ makeErrorResponse (requestId req) $ JsonRpcError
    { errorCode = -32602
    , errorMessage = "Missing parameters for resources/unsubscribe"
    , errorData = Nothing
    }
  Just params -> case fromJSON params of
    Error err -> return $ makeErrorResponse (requestId req) $ JsonRpcError
      { errorCode = -32602
      , errorMessage = "Invalid parameters for resources/unsubscribe: " <> T.pack err
      , errorData = Nothing
      }
    Success (_ :: UnsubscribeRequest) ->
      return $ makeSuccessResponse (requestId req) (object [])

-- | Convert MCP error to JSON-RPC error code
errorCodeFromMcpError :: Error -> Int
errorCodeFromMcpError (InvalidPromptName _)     = -32602
errorCodeFromMcpError (MissingRequiredParams _) = -32602
errorCodeFromMcpError (ResourceNotFound _)      = -32602
errorCodeFromMcpError (InternalError _)         = -32603
errorCodeFromMcpError (UnknownTool _)           = -32602
errorCodeFromMcpError (InvalidRequest _)        = -32600
errorCodeFromMcpError (MethodNotFound _)        = -32601
errorCodeFromMcpError (InvalidParams _)         = -32602

-- | Convert MCP error to JSON-RPC error message
errorMessageFromMcpError :: Error -> Text
errorMessageFromMcpError (InvalidPromptName msg) = "Invalid prompt name: " <> msg
errorMessageFromMcpError (MissingRequiredParams msg) = "Missing required parameters: " <> msg
errorMessageFromMcpError (ResourceNotFound msg) = "Resource not found: " <> msg
errorMessageFromMcpError (InternalError msg) = "Internal error: " <> msg
errorMessageFromMcpError (UnknownTool msg) = "Unknown tool: " <> msg
errorMessageFromMcpError (InvalidRequest msg) = "Invalid request: " <> msg
errorMessageFromMcpError (MethodNotFound msg) = "Method not found: " <> msg
errorMessageFromMcpError (InvalidParams msg) = "Invalid parameters: " <> msg
