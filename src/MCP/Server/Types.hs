{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module MCP.Server.Types
  ( -- * Content Types
    Content(..)
  , ContentImageData(..)
  , ContentAudioData(..)
  , ResourceLinkData(..)
  , ResourceContent(..)

    -- * Prompt Messages
  , PromptMessage(..)
  , MessageRole(..)
  , userMessage
  , assistantMessage

    -- * Tool Results
  , ToolResult(..)
  , toolText
  , toolContent
  , toolError

    -- * Logging
  , LogLevel(..)
  , logLevelText
  , parseLogLevel

    -- * Completion
  , CompletionReference(..)
  , CompletionArgument(..)
  , CompletionResult(..)
  , CompletionHandler

    -- * URI Utilities
  , parseURI
  , URI

    -- * Error Types
  , Error(..)

    -- * Definition Types
  , PromptDefinition(..)
  , ResourceDefinition(..)
  , ResourceTemplateDefinition(..)
  , ToolDefinition(..)
  , ArgumentDefinition(..)
  , InputSchemaDefinition(..)
  , InputSchemaDefinitionProperty(..)

    -- * Server Types
  , McpServerInfo(..)
  , McpServerHandlers(..)
  , ServerCapabilities(..)
  , PromptCapabilities(..)
  , ResourceCapabilities(..)
  , ToolCapabilities(..)
  , LoggingCapabilities(..)
  , CompletionCapabilities(..)

    -- * Request/Response Types
  , PromptListHandler
  , PromptGetHandler
  , ResourceListHandler
  , ResourceReadHandler
  , ResourceTemplateListHandler
  , ToolListHandler
  , ToolCallHandler

    -- * Basic Types
  , PromptName
  , ToolName
  , ArgumentName
  , ArgumentValue
  ) where

import           Data.Aeson
import           Data.Aeson.Key   (fromText)
import           Data.Aeson.Types (Parser)
import           Data.Maybe       (catMaybes)
import           Data.Text        (Text)
import qualified Data.Text        as T
import           GHC.Generics     (Generic)
import           Network.URI      (URI, parseURI)

type PromptName = Text
type ToolName = Text
type ArgumentName = Text
type ArgumentValue = Text

-- | Content block returned in tool results, prompt messages, and other places
-- where the MCP spec defines a polymorphic content union
-- (text, image, audio, resource_link, embedded resource).
data Content
  = ContentText Text
  | ContentImage ContentImageData
  | ContentAudio ContentAudioData
  | ContentResourceLink ResourceLinkData
  | ContentEmbeddedResource ResourceContent
  deriving (Show, Eq, Generic)

instance ToJSON Content where
  toJSON (ContentText text) = object
    [ "type" .= ("text" :: Text)
    , "text" .= text
    ]
  toJSON (ContentImage img) = object
    [ "type" .= ("image" :: Text)
    , "data" .= contentImageData img
    , "mimeType" .= contentImageMimeType img
    ]
  toJSON (ContentAudio aud) = object
    [ "type" .= ("audio" :: Text)
    , "data" .= contentAudioData aud
    , "mimeType" .= contentAudioMimeType aud
    ]
  toJSON (ContentResourceLink link) = object $
    [ "type" .= ("resource_link" :: Text)
    , "uri" .= show (resourceLinkUri link)
    , "name" .= resourceLinkName link
    ] ++ maybe [] (\d -> ["description" .= d]) (resourceLinkDescription link)
      ++ maybe [] (\m -> ["mimeType" .= m]) (resourceLinkMimeType link)
  toJSON (ContentEmbeddedResource res) = object
    [ "type" .= ("resource" :: Text)
    , "resource" .= res
    ]

instance FromJSON Content where
  parseJSON = withObject "Content" $ \o -> do
    contentType <- o .: "type" :: Parser Text
    case contentType of
      "text" -> ContentText <$> o .: "text"
      "image" -> do
        imgData <- o .: "data"
        mimeType <- o .: "mimeType"
        return $ ContentImage $ ContentImageData imgData mimeType
      "audio" -> do
        audData <- o .: "data"
        mimeType <- o .: "mimeType"
        return $ ContentAudio $ ContentAudioData audData mimeType
      "resource_link" -> do
        uriText <- o .: "uri"
        case parseURI uriText of
          Nothing  -> fail $ "Invalid resource_link uri: " <> uriText
          Just uri -> ContentResourceLink <$>
            ( ResourceLinkData uri
                <$> o .: "name"
                <*> o .:? "description"
                <*> o .:? "mimeType" )
      "resource" -> ContentEmbeddedResource <$> o .: "resource"
      _ -> fail $ "Unknown content type: " ++ T.unpack contentType

data ContentImageData = ContentImageData
  { contentImageData     :: Text  -- ^ base64-encoded image bytes
  , contentImageMimeType :: Text
  } deriving (Show, Eq, Generic)

data ContentAudioData = ContentAudioData
  { contentAudioData     :: Text  -- ^ base64-encoded audio bytes
  , contentAudioMimeType :: Text
  } deriving (Show, Eq, Generic)

-- | A resource_link content block — points at a resource without inlining its
-- contents. The MCP spec requires @uri@ and @name@; @description@ and
-- @mimeType@ are optional.
data ResourceLinkData = ResourceLinkData
  { resourceLinkUri         :: URI
  , resourceLinkName        :: Text
  , resourceLinkDescription :: Maybe Text
  , resourceLinkMimeType    :: Maybe Text
  } deriving (Show, Eq, Generic)

-- | Role of a prompt message turn.
data MessageRole = RoleUser | RoleAssistant
  deriving (Show, Eq, Generic)

instance ToJSON MessageRole where
  toJSON RoleUser      = "user"
  toJSON RoleAssistant = "assistant"

-- | A single message in a prompt response — pairs a role with a content block.
-- Multiple messages in a row let a prompt author stage a multi-turn
-- conversation including embedded resources or images.
data PromptMessage = PromptMessage
  { promptMessageRole    :: MessageRole
  , promptMessageContent :: Content
  } deriving (Show, Eq, Generic)

instance ToJSON PromptMessage where
  toJSON msg = object
    [ "role"    .= promptMessageRole msg
    , "content" .= promptMessageContent msg
    ]

-- | Convenience constructor for a user-role message.
userMessage :: Content -> PromptMessage
userMessage = PromptMessage RoleUser

-- | Convenience constructor for an assistant-role message.
assistantMessage :: Content -> PromptMessage
assistantMessage = PromptMessage RoleAssistant

-- | The full result of a @tools/call@ handler — a list of content blocks
-- plus a flag distinguishing a tool execution error from successful
-- output. The error case is the spec-mandated way for a tool to report
-- an in-band failure (e.g. \"file not found\") without rejecting the
-- protocol-level @tools/call@ as malformed.
data ToolResult = ToolResult
  { toolResultContent :: [Content]
  , toolResultIsError :: Bool
  } deriving (Show, Eq, Generic)

-- | Build a successful 'ToolResult' from a single text body.
toolText :: Text -> ToolResult
toolText t = ToolResult [ContentText t] False

-- | Build a successful 'ToolResult' from an arbitrary list of content blocks.
toolContent :: [Content] -> ToolResult
toolContent cs = ToolResult cs False

-- | Build a tool execution error wrapping a single text body. The protocol
-- response carries @isError: true@ — distinct from a JSON-RPC error which
-- the dispatcher uses to reject malformed requests.
toolError :: Text -> ToolResult
toolError t = ToolResult [ContentText t] True

-- | RFC 5424 syslog-style severity levels used by @logging/setLevel@ and
-- @notifications/message@.
data LogLevel
  = LogDebug
  | LogInfo
  | LogNotice
  | LogWarning
  | LogError
  | LogCritical
  | LogAlert
  | LogEmergency
  deriving (Show, Eq, Ord, Generic)

logLevelText :: LogLevel -> Text
logLevelText LogDebug     = "debug"
logLevelText LogInfo      = "info"
logLevelText LogNotice    = "notice"
logLevelText LogWarning   = "warning"
logLevelText LogError     = "error"
logLevelText LogCritical  = "critical"
logLevelText LogAlert     = "alert"
logLevelText LogEmergency = "emergency"

parseLogLevel :: Text -> Maybe LogLevel
parseLogLevel t = case t of
  "debug"     -> Just LogDebug
  "info"      -> Just LogInfo
  "notice"    -> Just LogNotice
  "warning"   -> Just LogWarning
  "error"     -> Just LogError
  "critical"  -> Just LogCritical
  "alert"     -> Just LogAlert
  "emergency" -> Just LogEmergency
  _           -> Nothing

instance ToJSON LogLevel where
  toJSON = toJSON . logLevelText

instance FromJSON LogLevel where
  parseJSON = withText "LogLevel" $ \t -> case parseLogLevel t of
    Just l  -> pure l
    Nothing -> fail $ "Unknown log level: " <> T.unpack t

-- | A completion reference picks the prompt argument or resource template
-- argument the client wants to complete.
data CompletionReference
  = RefPrompt   { refPromptName :: Text }
  | RefResource { refResourceUri :: Text }
  deriving (Show, Eq, Generic)

instance FromJSON CompletionReference where
  parseJSON = withObject "CompletionReference" $ \o -> do
    refType <- o .: "type" :: Parser Text
    case refType of
      "ref/prompt"   -> RefPrompt   <$> o .: "name"
      "ref/resource" -> RefResource <$> o .: "uri"
      other -> fail $ "Unknown completion ref type: " <> T.unpack other

data CompletionArgument = CompletionArgument
  { completionArgName  :: Text
  , completionArgValue :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON CompletionArgument where
  parseJSON = withObject "CompletionArgument" $ \o -> CompletionArgument
    <$> o .: "name"
    <*> o .: "value"

data CompletionResult = CompletionResult
  { completionValues  :: [Text]
  , completionTotal   :: Maybe Int
  , completionHasMore :: Maybe Bool
  } deriving (Show, Eq, Generic)

instance ToJSON CompletionResult where
  toJSON r = object $
    [ "values" .= completionValues r ]
    ++ maybe [] (\t -> ["total"   .= t]) (completionTotal r)
    ++ maybe [] (\h -> ["hasMore" .= h]) (completionHasMore r)

-- | Optional handler for @completion/complete@. The third argument carries
-- any optional context the client included (typically previously-resolved
-- argument values).
type CompletionHandler m =
     CompletionReference
  -> CompletionArgument
  -> Maybe Value
  -> m CompletionResult

-- | Resource content compliant with MCP specification
-- Must include uri and mimeType, with either text or blob data
data ResourceContent
  = ResourceText
      { resourceUri :: URI
      , resourceMimeType :: Text
      , resourceText :: Text
      }
  | ResourceBlob
      { resourceUri :: URI
      , resourceMimeType :: Text
      , resourceBlob :: Text  -- base64 encoded
      }
  deriving (Show, Eq, Generic)

instance ToJSON ResourceContent where
  toJSON (ResourceText uri mimeType text) = object
    [ "uri" .= show uri
    , "mimeType" .= mimeType
    , "text" .= text
    ]
  toJSON (ResourceBlob uri mimeType blob) = object
    [ "uri" .= show uri
    , "mimeType" .= mimeType
    , "blob" .= blob
    ]

instance FromJSON ResourceContent where
  parseJSON = withObject "ResourceContent" $ \o -> do
    uriText <- o .: "uri"
    mimeType <- o .: "mimeType"
    case parseURI uriText of
      Nothing -> fail "Invalid URI"
      Just uri -> do
        maybeText <- o .:? "text"
        maybeBlob <- o .:? "blob"
        case (maybeText, maybeBlob) of
          (Just text, Nothing) -> return $ ResourceText uri mimeType text
          (Nothing, Just blob) -> return $ ResourceBlob uri mimeType blob
          _ -> fail "ResourceContent must have either 'text' or 'blob' field"

-- | MCP protocol errors
data Error
  = InvalidPromptName Text
  | MissingRequiredParams Text
  | ResourceNotFound Text
  | InternalError Text
  | UnknownTool Text
  | InvalidRequest Text
  | MethodNotFound Text
  | InvalidParams Text
  deriving (Show, Eq, Generic)

instance ToJSON Error where
  toJSON err = object
    [ "code" .= errorCode err
    , "message" .= errorMessage err
    ]
    where
      errorCode :: Error -> Int
      errorCode (InvalidPromptName _)     = -32602
      errorCode (MissingRequiredParams _) = -32602
      errorCode (ResourceNotFound _)      = -32602
      errorCode (InternalError _)         = -32603
      errorCode (UnknownTool _)           = -32602
      errorCode (InvalidRequest _)        = -32600
      errorCode (MethodNotFound _)        = -32601
      errorCode (InvalidParams _)         = -32602

      errorMessage :: Error -> Text
      errorMessage (InvalidPromptName msg) = "Invalid prompt name: " <> msg
      errorMessage (MissingRequiredParams msg) = "Missing required parameters: " <> msg
      errorMessage (ResourceNotFound msg) = "Resource not found: " <> msg
      errorMessage (InternalError msg) = "Internal error: " <> msg
      errorMessage (UnknownTool msg) = "Unknown tool: " <> msg
      errorMessage (InvalidRequest msg) = "Invalid request: " <> msg
      errorMessage (MethodNotFound msg) = "Method not found: " <> msg
      errorMessage (InvalidParams msg) = "Invalid parameters: " <> msg

-- | Prompt definition (2025-06-18 enhanced)
data PromptDefinition = PromptDefinition
  { promptDefinitionName        :: Text
  , promptDefinitionDescription :: Text
  , promptDefinitionArguments   :: [ArgumentDefinition]
  , promptDefinitionTitle       :: Maybe Text  -- New title field for human-friendly display
  } deriving (Show, Eq, Generic)

instance ToJSON PromptDefinition where
  toJSON def = object $
    [ "name" .= promptDefinitionName def
    , "description" .= promptDefinitionDescription def
    , "arguments" .= promptDefinitionArguments def
    ] ++ maybe [] (\t -> ["title" .= t]) (promptDefinitionTitle def)

-- | Resource definition (2025-06-18 enhanced)
data ResourceDefinition = ResourceDefinition
  { resourceDefinitionURI         :: Text
  , resourceDefinitionName        :: Text
  , resourceDefinitionDescription :: Maybe Text
  , resourceDefinitionMimeType    :: Maybe Text
  , resourceDefinitionTitle       :: Maybe Text  -- New title field for human-friendly display
  } deriving (Show, Eq, Generic)

instance ToJSON ResourceDefinition where
  toJSON def = object $
    [ "uri" .= resourceDefinitionURI def
    , "name" .= resourceDefinitionName def
    ] ++
    maybe [] (\d -> ["description" .= d]) (resourceDefinitionDescription def) ++
    maybe [] (\m -> ["mimeType" .= m]) (resourceDefinitionMimeType def) ++
    maybe [] (\t -> ["title" .= t]) (resourceDefinitionTitle def)

-- | RFC 6570 URI template a server advertises so clients can synthesize
-- resource URIs (e.g. @test://template/{id}/data@). Templated reads still
-- flow through the regular @resources/read@ handler.
data ResourceTemplateDefinition = ResourceTemplateDefinition
  { resourceTemplateUriTemplate :: Text
  , resourceTemplateName        :: Text
  , resourceTemplateDescription :: Maybe Text
  , resourceTemplateMimeType    :: Maybe Text
  , resourceTemplateTitle       :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON ResourceTemplateDefinition where
  toJSON def = object $
    [ "uriTemplate" .= resourceTemplateUriTemplate def
    , "name"        .= resourceTemplateName def
    ] ++
    maybe [] (\d -> ["description" .= d]) (resourceTemplateDescription def) ++
    maybe [] (\m -> ["mimeType" .= m])    (resourceTemplateMimeType def) ++
    maybe [] (\t -> ["title" .= t])       (resourceTemplateTitle def)

-- | Tool definition (2025-06-18 enhanced)
data ToolDefinition = ToolDefinition
  { toolDefinitionName        :: Text
  , toolDefinitionDescription :: Text
  , toolDefinitionInputSchema :: InputSchemaDefinition
  , toolDefinitionTitle       :: Maybe Text  -- New title field for human-friendly display
  } deriving (Show, Eq, Generic)

instance ToJSON ToolDefinition where
  toJSON def = object $
    [ "name" .= toolDefinitionName def
    , "description" .= toolDefinitionDescription def
    , "inputSchema" .= toolDefinitionInputSchema def
    ] ++ maybe [] (\t -> ["title" .= t]) (toolDefinitionTitle def)

-- | Argument definition for prompts
data ArgumentDefinition = ArgumentDefinition
  { argumentDefinitionName        :: Text
  , argumentDefinitionDescription :: Text
  , argumentDefinitionRequired    :: Bool
  } deriving (Show, Eq, Generic)

instance ToJSON ArgumentDefinition where
  toJSON def = object
    [ "name" .= argumentDefinitionName def
    , "description" .= argumentDefinitionDescription def
    , "required" .= argumentDefinitionRequired def
    ]

-- | Input schema definition for tools
data InputSchemaDefinition = InputSchemaDefinitionObject
  { properties :: [(Text, InputSchemaDefinitionProperty)]
  , required   :: [Text]
  } deriving (Show, Eq, Generic)

instance ToJSON InputSchemaDefinition where
  toJSON (InputSchemaDefinitionObject props req) = object
    [ "type" .= ("object" :: Text)
    , "properties" .= object (map (\(k, v) -> fromText k .= v) props)
    , "required" .= req
    ]

data InputSchemaDefinitionProperty = InputSchemaDefinitionProperty
  { propertyType        :: Text
  , propertyDescription :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON InputSchemaDefinitionProperty where
  toJSON prop = object
    [ "type" .= propertyType prop
    , "description" .= propertyDescription prop
    ]

-- | Server information
data McpServerInfo = McpServerInfo
  { serverName         :: Text
  , serverVersion      :: Text
  , serverInstructions :: Text
  } deriving (Show, Eq, Generic)

-- | Individual capability objects
data PromptCapabilities = PromptCapabilities
  { promptListChanged :: Maybe Bool
  } deriving (Show, Eq, Generic)

instance ToJSON PromptCapabilities where
  toJSON caps = object $ catMaybes
    [ fmap ("listChanged" .=) (promptListChanged caps)
    ]

data ResourceCapabilities = ResourceCapabilities
  { resourceSubscribe   :: Maybe Bool
  , resourceListChanged :: Maybe Bool
  } deriving (Show, Eq, Generic)

instance ToJSON ResourceCapabilities where
  toJSON caps = object $ catMaybes
    [ fmap ("subscribe" .=) (resourceSubscribe caps)
    , fmap ("listChanged" .=) (resourceListChanged caps)
    ]

data ToolCapabilities = ToolCapabilities
  { toolListChanged :: Maybe Bool
  } deriving (Show, Eq, Generic)

instance ToJSON ToolCapabilities where
  toJSON caps = object $ catMaybes
    [ fmap ("listChanged" .=) (toolListChanged caps)
    ]

data LoggingCapabilities = LoggingCapabilities
  { -- No specific sub-capabilities for logging yet
  } deriving (Show, Eq, Generic)

instance ToJSON LoggingCapabilities where
  toJSON _ = object []

data CompletionCapabilities = CompletionCapabilities
  { -- No specific sub-capabilities defined for completions in 2025-06-18
  } deriving (Show, Eq, Generic)

instance ToJSON CompletionCapabilities where
  toJSON _ = object []

-- | Server capabilities
data ServerCapabilities = ServerCapabilities
  { capabilityPrompts     :: Maybe PromptCapabilities
  , capabilityResources   :: Maybe ResourceCapabilities
  , capabilityTools       :: Maybe ToolCapabilities
  , capabilityLogging     :: Maybe LoggingCapabilities
  , capabilityCompletions :: Maybe CompletionCapabilities
  } deriving (Show, Eq, Generic)

instance ToJSON ServerCapabilities where
  toJSON caps = object $ catMaybes
    [ fmap ("prompts" .=) (capabilityPrompts caps)
    , fmap ("resources" .=) (capabilityResources caps)
    , fmap ("tools" .=) (capabilityTools caps)
    , fmap ("logging" .=) (capabilityLogging caps)
    , fmap ("completions" .=) (capabilityCompletions caps)
    ]


-- | Handler type definitions
type PromptListHandler m = m [PromptDefinition]
type PromptGetHandler m = PromptName -> [(ArgumentName, ArgumentValue)] -> m (Either Error [PromptMessage])

type ResourceListHandler m = m [ResourceDefinition]
type ResourceReadHandler m = URI -> m (Either Error ResourceContent)
type ResourceTemplateListHandler m = m [ResourceTemplateDefinition]

type ToolListHandler m = m [ToolDefinition]
type ToolCallHandler m = ToolName -> [(ArgumentName, ArgumentValue)] -> m (Either Error ToolResult)

-- | Server handlers — every family is optional. The @resources@ pair handles
-- @resources/list@ and @resources/read@; @resourceTemplates@ stands alone
-- and just feeds @resources/templates/list@ — templated reads dispatch
-- through the same 'ResourceReadHandler', with the URI fully substituted.
data McpServerHandlers m = McpServerHandlers
  { prompts           :: Maybe (PromptListHandler m, PromptGetHandler m)
  , resources         :: Maybe (ResourceListHandler m, ResourceReadHandler m)
  , resourceTemplates :: Maybe (ResourceTemplateListHandler m)
  , tools             :: Maybe (ToolListHandler m, ToolCallHandler m)
  , completions       :: Maybe (CompletionHandler m)
  }
