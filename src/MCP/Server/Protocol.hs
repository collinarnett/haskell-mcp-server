{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module MCP.Server.Protocol
  ( -- * MCP Protocol Messages
    InitializeRequest(..)
  , InitializeResponse(..)
  , InitializedNotification(..)
  , PingRequest(..)
  , PongResponse(..)

    -- * Prompts Protocol
  , PromptsListRequest(..)
  , PromptsListResponse(..)
  , PromptsGetRequest(..)
  , PromptsGetResponse(..)

    -- * Resources Protocol
  , ResourcesListRequest(..)
  , ResourcesListResponse(..)
  , ResourcesReadRequest(..)
  , ResourcesReadResponse(..)
  , ResourcesTemplatesListRequest(..)
  , ResourcesTemplatesListResponse(..)

    -- * Tools Protocol
  , ToolsListRequest(..)
  , ToolsListResponse(..)
  , ToolsCallRequest(..)
  , ToolsCallResponse(..)

    -- * Logging
  , SetLevelRequest(..)

    -- * Completion
  , CompleteRequest(..)
  , CompleteResponse(..)

    -- * Subscriptions
  , SubscribeRequest(..)
  , UnsubscribeRequest(..)

    -- * Common Types
  , ListChangedNotification(..)

    -- * Protocol Functions
  , protocolVersion
  ) where

import           Data.Aeson
import qualified Data.Aeson.KeyMap as AKM
import           Data.Map          (Map)
import           Data.Text         (Text)
import           GHC.Generics      (Generic)
import           MCP.Server.Types

protocolVersion :: Text
protocolVersion = "2025-06-18"


-- | Initialize request
data InitializeRequest = InitializeRequest
  { initProtocolVersion :: Text
  , initCapabilities    :: Value
  , initClientInfo      :: Value
  } deriving (Show, Eq, Generic)

instance FromJSON InitializeRequest where
  parseJSON = withObject "InitializeRequest" $ \o -> InitializeRequest
    <$> o .: "protocolVersion"
    <*> o .: "capabilities"
    <*> o .: "clientInfo"

-- | Initialize response
data InitializeResponse = InitializeResponse
  { initRespProtocolVersion :: Text
  , initRespCapabilities    :: ServerCapabilities
  , initRespServerInfo      :: McpServerInfo
  } deriving (Show, Eq, Generic)

instance ToJSON InitializeResponse where
  toJSON resp = object
    [ "protocolVersion" .= initRespProtocolVersion resp
    , "capabilities" .= initRespCapabilities resp
    , "serverInfo" .= object
        [ "name" .= serverName (initRespServerInfo resp)
        , "version" .= serverVersion (initRespServerInfo resp)
        , "instructions" .= serverInstructions (initRespServerInfo resp)
        ]
    ]

-- | Initialized notification (no parameters)
data InitializedNotification = InitializedNotification
  deriving (Show, Eq, Generic)

instance FromJSON InitializedNotification where
  parseJSON _ = return InitializedNotification

-- | Ping request (no parameters)
data PingRequest = PingRequest
  deriving (Show, Eq, Generic)

instance FromJSON PingRequest where
  parseJSON _ = return PingRequest

-- | Pong response (empty object)
data PongResponse = PongResponse
  deriving (Show, Eq, Generic)

instance ToJSON PongResponse where
  toJSON PongResponse = object []

-- 'PromptMessage' / 'MessageRole' live in "MCP.Server.Types" so the public
-- @MCP.Server@ module can re-export them via its existing
-- @module MCP.Server.Types@ re-export.

-- | Prompts list request
data PromptsListRequest = PromptsListRequest
  deriving (Show, Eq, Generic)

instance FromJSON PromptsListRequest where
  parseJSON _ = return PromptsListRequest

-- | Prompts list response
data PromptsListResponse = PromptsListResponse
  { promptsListPrompts :: [PromptDefinition]
  } deriving (Show, Eq, Generic)

instance ToJSON PromptsListResponse where
  toJSON resp = object
    [ "prompts" .= promptsListPrompts resp
    ]

-- | Prompts get request
data PromptsGetRequest = PromptsGetRequest
  { promptsGetName      :: Text
  , promptsGetArguments :: Maybe (Map Text Value)
  } deriving (Show, Eq, Generic)

instance FromJSON PromptsGetRequest where
  parseJSON = withObject "PromptsGetRequest" $ \o -> PromptsGetRequest
    <$> o .: "name"
    <*> o .:? "arguments"

-- | Prompts get response (2025-06-18 enhanced)
data PromptsGetResponse = PromptsGetResponse
  { promptsGetDescription :: Maybe Text
  , promptsGetMessages    :: [PromptMessage]
  , promptsGetMeta        :: Maybe Value  -- New _meta field for additional metadata
  } deriving (Show, Eq, Generic)

instance ToJSON PromptsGetResponse where
  toJSON resp = object $
    [ "messages" .= promptsGetMessages resp
    ] ++ maybe [] (\d -> ["description" .= d]) (promptsGetDescription resp)
      ++ maybe [] (\m -> ["_meta" .= m]) (promptsGetMeta resp)

-- | Resources list request
data ResourcesListRequest = ResourcesListRequest
  deriving (Show, Eq, Generic)

instance FromJSON ResourcesListRequest where
  parseJSON _ = return ResourcesListRequest

-- | Resources list response
data ResourcesListResponse = ResourcesListResponse
  { resourcesListResources :: [ResourceDefinition]
  } deriving (Show, Eq, Generic)

instance ToJSON ResourcesListResponse where
  toJSON resp = object
    [ "resources" .= resourcesListResources resp
    ]

-- | Resources read request
data ResourcesReadRequest = ResourcesReadRequest
  { resourcesReadUri :: URI
  } deriving (Show, Eq, Generic)

instance FromJSON ResourcesReadRequest where
  parseJSON = withObject "ResourcesReadRequest" $ \o -> do
    uriText <- o .: "uri"
    case parseURI uriText of
      Just uri -> return $ ResourcesReadRequest uri
      Nothing  -> fail "Invalid URI"

-- | Resources read response
data ResourcesReadResponse = ResourcesReadResponse
  { resourcesReadContents :: [ResourceContent]
  } deriving (Show, Eq, Generic)

instance ToJSON ResourcesReadResponse where
  toJSON resp = object
    [ "contents" .= resourcesReadContents resp
    ]

-- | @resources/templates/list@ has the same nullary request shape as the
-- other paginated list requests. We do not implement cursor-based pagination
-- yet — the server returns every template in a single response.
data ResourcesTemplatesListRequest = ResourcesTemplatesListRequest
  deriving (Show, Eq, Generic)

instance FromJSON ResourcesTemplatesListRequest where
  parseJSON _ = return ResourcesTemplatesListRequest

data ResourcesTemplatesListResponse = ResourcesTemplatesListResponse
  { resourcesTemplatesListTemplates :: [ResourceTemplateDefinition]
  } deriving (Show, Eq, Generic)

instance ToJSON ResourcesTemplatesListResponse where
  toJSON resp = object
    [ "resourceTemplates" .= resourcesTemplatesListTemplates resp
    ]

-- | Tools list request
data ToolsListRequest = ToolsListRequest
  deriving (Show, Eq, Generic)

instance FromJSON ToolsListRequest where
  parseJSON _ = return ToolsListRequest

-- | Tools list response
data ToolsListResponse = ToolsListResponse
  { toolsListTools :: [ToolDefinition]
  } deriving (Show, Eq, Generic)

instance ToJSON ToolsListResponse where
  toJSON resp = object
    [ "tools" .= toolsListTools resp
    ]

-- | Tools call request. The optional @_meta.progressToken@ is reified into
-- 'toolsCallProgressToken' so the dispatch layer can wire it through to
-- the tool handler's 'McpSession' without poking at raw JSON.
data ToolsCallRequest = ToolsCallRequest
  { toolsCallName          :: Text
  , toolsCallArguments     :: Maybe (Map Text Value)
  , toolsCallProgressToken :: Maybe Value
  } deriving (Show, Eq, Generic)

instance FromJSON ToolsCallRequest where
  parseJSON = withObject "ToolsCallRequest" $ \o -> do
    name      <- o .: "name"
    arguments <- o .:? "arguments"
    metaObj   <- o .:? "_meta"
    let progressToken = case metaObj of
          Just (Object km) -> case AKM.lookup "progressToken" km of
            Just v -> Just v
            _      -> Nothing
          _ -> Nothing
    pure $ ToolsCallRequest name arguments progressToken

-- | Tools call response (2025-06-18 enhanced)
data ToolsCallResponse = ToolsCallResponse
  { toolsCallContent :: [Content]
  , toolsCallIsError :: Maybe Bool
  , toolsCallMeta :: Maybe Value  -- New _meta field for structured output
  } deriving (Show, Eq, Generic)

instance ToJSON ToolsCallResponse where
  toJSON resp = object $
    [ "content" .= toolsCallContent resp
    ] ++ maybe [] (\e -> ["isError" .= e]) (toolsCallIsError resp)
      ++ maybe [] (\m -> ["_meta" .= m]) (toolsCallMeta resp)

-- | @logging/setLevel@ — stores a per-session log threshold.
data SetLevelRequest = SetLevelRequest
  { setLevelLevel :: LogLevel
  } deriving (Show, Eq, Generic)

instance FromJSON SetLevelRequest where
  parseJSON = withObject "SetLevelRequest" $ \o -> SetLevelRequest <$> o .: "level"

-- | @completion/complete@ request.
data CompleteRequest = CompleteRequest
  { completeRef      :: CompletionReference
  , completeArgument :: CompletionArgument
  , completeContext  :: Maybe Value
  } deriving (Show, Eq, Generic)

instance FromJSON CompleteRequest where
  parseJSON = withObject "CompleteRequest" $ \o -> CompleteRequest
    <$> o .:  "ref"
    <*> o .:  "argument"
    <*> o .:? "context"

data CompleteResponse = CompleteResponse
  { completeCompletion :: CompletionResult
  } deriving (Show, Eq, Generic)

instance ToJSON CompleteResponse where
  toJSON r = object [ "completion" .= completeCompletion r ]

-- | @resources/subscribe@ — server is asked to monitor a URI for updates.
data SubscribeRequest = SubscribeRequest
  { subscribeUri :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON SubscribeRequest where
  parseJSON = withObject "SubscribeRequest" $ \o -> SubscribeRequest <$> o .: "uri"

-- | @resources/unsubscribe@ — paired with 'SubscribeRequest'.
data UnsubscribeRequest = UnsubscribeRequest
  { unsubscribeUri :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON UnsubscribeRequest where
  parseJSON = withObject "UnsubscribeRequest" $ \o -> UnsubscribeRequest <$> o .: "uri"

-- | List changed notification
data ListChangedNotification = ListChangedNotification
  deriving (Show, Eq, Generic)

instance ToJSON ListChangedNotification where
  toJSON ListChangedNotification = object []
