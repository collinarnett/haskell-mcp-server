{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main where

import           Control.Concurrent (threadDelay)
import           Data.Aeson         (FromJSON, ToJSON (..), Value, fromJSON)
import qualified Data.Aeson         as A
import           Data.List          (dropWhileEnd)
import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as Map
import qualified Data.Text          as T
import           GHC.Generics       (Generic)
import           MCP.Server
import           MCP.Server.Derive
import           System.Environment (getArgs)
import           System.Exit        (exitFailure)
import           System.IO          (hPutStrLn, hSetEncoding, stderr, stdout,
                                     utf8)
import           Text.Read          (readMaybe)
import           Types

-- | Tiny 1×1 transparent PNG used as the test image for prompts and tools that
-- need an image content block. Inlined as base64 to keep the example self-
-- contained.
tinyPngBase64 :: T.Text
tinyPngBase64 =
    "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNkYAAAAAYAAjCB0C8AAAAASUVORK5CYII="

-- | Parse a URI literal known to be valid at write time. The arguments are
-- compile-time constants embedded in the source; a failure here means the
-- literal itself is malformed (i.e. a typo in this file), not invalid user
-- input. This is the only place in the example where we centralize the
-- "cannot happen at runtime" invariant — every dynamic URI is parsed via
-- 'parseURI' with a real 'Maybe' branch.
mustParseURI :: String -> URI
mustParseURI s = case parseURI s of
  Just u  -> u
  Nothing -> error ("invariant: hardcoded URI literal does not parse: " <> s)

-- Pre-parsed conformance URIs. Lifted to top-level constants so each handler
-- doesn't re-run 'parseURI' on every call.
embeddedResourceUri, mixedContentResourceUri, fallbackEmbeddedUri :: URI
embeddedResourceUri     = mustParseURI "test://embedded-resource"
mixedContentResourceUri = mustParseURI "test://mixed-content-resource"
fallbackEmbeddedUri     = mustParseURI "test://invalid"

-- High-level handler functions

handlePrompt :: MyPrompt -> IO [PromptMessage]
handlePrompt (Recipe idea) =
    pure [userMessage (ContentText ("Recipe prompt for " <> idea <> ": Start by gathering fresh ingredients..."))]
handlePrompt (Shopping description) =
    pure [userMessage (ContentText ("Shopping prompt for " <> description <> ": Create a detailed shopping list..."))]
handlePrompt TestSimplePrompt =
    pure [userMessage (ContentText "This is a simple prompt for testing.")]
handlePrompt (TestPromptWithArguments a1 a2) =
    pure [userMessage (ContentText ("Prompt with arguments: arg1='" <> a1 <> "', arg2='" <> a2 <> "'"))]
handlePrompt (TestPromptWithEmbeddedResource uriText) =
    -- Conformance passes an arbitrary URI string; if it does not round-trip
    -- through Network.URI we substitute a known-valid sentinel so we still
    -- produce a spec-shaped message rather than crashing the server.
    let uri = maybe fallbackEmbeddedUri id (parseURI (T.unpack uriText))
    in pure
      [ userMessage (ContentEmbeddedResource (ResourceText uri "text/plain" "Embedded resource content for testing."))
      , userMessage (ContentText "Please process the embedded resource above.")
      ]
handlePrompt TestPromptWithImage =
    pure
      [ userMessage (ContentImage (ContentImageData tinyPngBase64 "image/png"))
      , userMessage (ContentText "Please analyze the image above.")
      ]

handleResource :: URI -> MyResource -> IO ResourceContent
handleResource uri ProductCategories =
    pure $ ResourceText uri "text/plain" "Fresh Produce, Dairy, Bakery, Meat & Seafood, Frozen Foods"
handleResource uri SaleItems =
    pure $ ResourceText uri "text/plain" "Organic Apples $2.99/lb, Free Range Eggs $4.50/dozen, Artisan Bread $3.25/loaf"
handleResource uri HeadlineBannerAd =
    pure $ ResourceText uri "text/plain" "🛒 Weekly Special: 20% off all organic produce! 🥕🥬🍎"

-- | Resource list/read pair the conformance suite probes. We register the
-- demo resources from 'MyResource' alongside three @test://@ fixtures and
-- a templated URI. URI matching is hand-rolled because the TH derive only
-- supports nullary constructors and one URI scheme.
conformanceResources :: (ResourceListHandler IO, ResourceReadHandler IO)
conformanceResources = (listH, readH)
  where
    demoResource name uriText desc =
      ResourceDefinition
        { resourceDefinitionURI         = uriText
        , resourceDefinitionName        = name
        , resourceDefinitionDescription = Just desc
        , resourceDefinitionMimeType    = Just "text/plain"
        , resourceDefinitionTitle       = Nothing
        }
    listH = pure
      [ demoResource "product_categories"  "resource://product_categories"  "Product categories"
      , demoResource "sale_items"          "resource://sale_items"          "Items currently on sale"
      , demoResource "headline_banner_ad"  "resource://headline_banner_ad"  "Headline banner advertisement"
      , ResourceDefinition
          { resourceDefinitionURI         = "test://static-text"
          , resourceDefinitionName        = "static-text"
          , resourceDefinitionDescription = Just "Static text resource for conformance"
          , resourceDefinitionMimeType    = Just "text/plain"
          , resourceDefinitionTitle       = Nothing
          }
      , ResourceDefinition
          { resourceDefinitionURI         = "test://static-binary"
          , resourceDefinitionName        = "static-binary"
          , resourceDefinitionDescription = Just "Static binary resource for conformance"
          , resourceDefinitionMimeType    = Just "image/png"
          , resourceDefinitionTitle       = Nothing
          }
      ]
    readH uri = case T.pack (show uri) of
      "test://static-text"   ->
          pure $ Right $ ResourceText uri "text/plain" "This is the content of the static text resource."
      "test://static-binary" ->
          pure $ Right $ ResourceBlob uri "image/png" tinyPngBase64
      uriText
        | Just rest    <- T.stripPrefix "test://template/" uriText
        , Just idText  <- T.stripSuffix "/data" rest
        , not (T.null idText)
        ->
          let body =
                "{\"id\":\"" <> idText
                <> "\",\"templateTest\":true,\"data\":\"Data for ID: "
                <> idText <> "\"}"
          in pure $ Right $ ResourceText uri "application/json" body
      _ -> case parseURI (T.unpack (T.pack (show uri))) of
        Just _  -> handleDerivedResource uri
        Nothing -> pure $ Left $ ResourceNotFound (T.pack (show uri))

    -- Fall through to the derived MyResource handler so the demo URIs
    -- (resource://product_categories, etc.) keep working alongside the
    -- conformance fixtures.
    handleDerivedResource uri = do
      let (_, derivedRead) = derivedResources
      derivedRead uri

    derivedResources :: (ResourceListHandler IO, ResourceReadHandler IO)
    derivedResources = $(deriveResourceHandler ''MyResource 'handleResource)

conformanceResourceTemplates :: ResourceTemplateListHandler IO
conformanceResourceTemplates = pure
  [ ResourceTemplateDefinition
      { resourceTemplateUriTemplate = "test://template/{id}/data"
      , resourceTemplateName        = "template-data"
      , resourceTemplateDescription = Just "Templated data resource for conformance"
      , resourceTemplateMimeType    = Just "application/json"
      , resourceTemplateTitle       = Nothing
      }
  ]

-- | Tiny silent WAV (44-byte header + 2 zero samples) used for the audio
-- conformance fixture. WAV header values are little-endian.
tinyWavBase64 :: T.Text
tinyWavBase64 = "UklGRigAAABXQVZFZm10IBAAAAABAAEARKwAAIhYAQACABAAZGF0YQQAAAAAAAAA"

handleTool :: MyTool -> McpSession IO -> IO ToolResult
handleTool (SearchForProduct q category) _ =
    pure $ toolText $ case category of
        Nothing  -> "Search results for '" <> q <> "': Found 15 products across all categories"
        Just cat -> "Search results for '" <> q <> "' in " <> cat <> ": Found 8 products"
handleTool (AddToCart sku) _ =
    pure $ toolText $ "Added item " <> sku <> " to your cart. Cart total: 3 items"
handleTool Checkout _ =
    pure $ toolText "Checkout completed! Order #12345 confirmed. Thank you for shopping with us!"
handleTool (ComplexTool field1 field2 field3 field4 field5) _ =
    pure $ toolText $
        "Complex tool called with: " <> field1 <> ", " <> field2 <>
        maybe "" (", " <>) field3 <> ", " <> field4 <>
        maybe "" (", " <>) field5
handleTool TestSimpleText _ =
    pure $ toolText "This is a simple text response for testing."
handleTool TestImageContent _ =
    pure $ toolContent [ContentImage (ContentImageData tinyPngBase64 "image/png")]
handleTool TestAudioContent _ =
    pure $ toolContent [ContentAudio (ContentAudioData tinyWavBase64 "audio/wav")]
handleTool TestEmbeddedResource _ =
    pure $ toolContent
      [ ContentEmbeddedResource $ ResourceText embeddedResourceUri "text/plain" "This is an embedded resource content."
      ]
handleTool TestMultipleContentTypes _ =
    pure $ toolContent
      [ ContentText "Multiple content types test:"
      , ContentImage (ContentImageData tinyPngBase64 "image/png")
      , ContentEmbeddedResource $ ResourceText mixedContentResourceUri "application/json" "{\"test\":\"data\",\"value\":123}"
      ]
handleTool TestErrorHandling _ =
    pure $ toolError "This tool intentionally returns an error for testing"
handleTool TestToolWithProgress sess = do
    -- Spec: each notification carries the same @progressToken@ as the
    -- inbound request — sendProgress no-ops when the request had none.
    sendProgress sess 0   (Just 100) Nothing
    threadDelay 5_000
    sendProgress sess 50  (Just 100) Nothing
    threadDelay 5_000
    sendProgress sess 100 (Just 100) Nothing
    pure $ toolText "Progress test complete."
handleTool TestToolWithLogging sess = do
    sendLog sess LogInfo Nothing (toJSON ("Tool execution started" :: T.Text))
    threadDelay 5_000
    sendLog sess LogInfo Nothing (toJSON ("Tool processing data"   :: T.Text))
    threadDelay 5_000
    sendLog sess LogInfo Nothing (toJSON ("Tool execution completed":: T.Text))
    pure $ toolText "Logging test complete."
handleTool (TestSampling promptText) sess = do
    -- Issue a sampling/createMessage request to the client. The conformance
    -- suite plays the role of the LLM and always responds with
    -- role=assistant, content={type:text, text:"…"}.
    let params = SamplingParams
          { messages  = [userMessage (ContentText promptText)]
          , maxTokens = 100
          }
    eRes <- sample sess (toJSON params)
    case eRes of
      Left err -> pure $ toolError ("Sampling failed: " <> err)
      Right v  -> case fromJSON v of
        A.Success resp ->
          pure $ toolText ("LLM response: " <> samplingResponseText resp)
        A.Error e ->
          pure $ toolError ("Could not decode sampling response: " <> T.pack e)
handleTool (TestElicitation msg) sess = do
    let params = ElicitParams
          { message         = msg
          , requestedSchema = ElicitedSchema
              { type_      = "object"
              , properties = Map.fromList
                  [ ("username", PropertySchema "string" "User's response")
                  , ("email",    PropertySchema "string" "User's email address")
                  ]
              , required   = ["username", "email"]
              }
          }
    eRes <- elicit sess (toJSON params)
    case eRes of
      Left err -> pure $ toolError ("Elicitation failed: " <> err)
      Right v  -> case fromJSON v :: A.Result ElicitResponse of
        A.Success resp ->
          pure $ toolText $ case (resp.action, resp.content) of
            ("accept", Just c) -> "User accepted: " <> T.pack (show c)
            ("decline", _)     -> "User declined the elicitation."
            ("cancel",  _)     -> "User cancelled the elicitation."
            (other, _)         -> "Unknown elicitation action: " <> other
        A.Error e ->
          pure $ toolError ("Could not decode elicitation response: " <> T.pack e)

-- Sampling / elicitation wire types
-- ---------------------------------
-- These mirror the @sampling/createMessage@ and @elicitation/create@
-- params and result shapes (MCP 2025-06-18). We use 'Generic' deriving so
-- aeson handles encoding / decoding — no hand-rolled @object [...]@ trees.
-- Field names are chosen to match the wire keys directly; trailing
-- underscores are stripped via 'jsonDropTrailingUnderscore' so reserved
-- words like @type@ can stay as @type_@ in Haskell.

jsonDropTrailingUnderscore :: A.Options
jsonDropTrailingUnderscore = A.defaultOptions
  { A.fieldLabelModifier = dropWhileEnd (== '_') }

-- | @sampling/createMessage@ params. We reuse the library's 'PromptMessage'
-- because the wire shape (@{role, content: ContentBlock}@) is identical.
data SamplingParams = SamplingParams
  { messages  :: [PromptMessage]
  , maxTokens :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON SamplingParams

-- | The single content block returned by the client in a sampling response.
-- Only the @text@ shape is used by the conformance suite.
data SamplingResponseContent = SamplingResponseContent
  { type_ :: T.Text
  , text  :: T.Text
  } deriving (Show, Eq, Generic)

instance FromJSON SamplingResponseContent where
  parseJSON = A.genericParseJSON jsonDropTrailingUnderscore

-- | Whole @sampling/createMessage@ result; we only surface the inner text.
data SamplingResponse = SamplingResponse
  { role       :: T.Text
  , content    :: SamplingResponseContent
  , model      :: T.Text
  , stopReason :: Maybe T.Text
  } deriving (Show, Eq, Generic)

instance FromJSON SamplingResponse

samplingResponseText :: SamplingResponse -> T.Text
samplingResponseText resp = resp.content.text

-- | A property declaration in an elicitation @requestedSchema@.
data PropertySchema = PropertySchema
  { type_       :: T.Text
  , description :: T.Text
  } deriving (Show, Eq, Generic)

instance ToJSON PropertySchema where
  toJSON = A.genericToJSON jsonDropTrailingUnderscore

-- | @requestedSchema@ for elicitation. Always object-typed in our example.
data ElicitedSchema = ElicitedSchema
  { type_      :: T.Text
  , properties :: Map T.Text PropertySchema
  , required   :: [T.Text]
  } deriving (Show, Eq, Generic)

instance ToJSON ElicitedSchema where
  toJSON = A.genericToJSON jsonDropTrailingUnderscore

-- | @elicitation/create@ params.
data ElicitParams = ElicitParams
  { message         :: T.Text
  , requestedSchema :: ElicitedSchema
  } deriving (Show, Eq, Generic)

instance ToJSON ElicitParams

-- | @elicitation/create@ result. @content@ is present only when
-- @action == "accept"@; we keep it as a generic 'Value' since its shape
-- depends on the requested schema.
data ElicitResponse = ElicitResponse
  { action  :: T.Text
  , content :: Maybe Value
  } deriving (Show, Eq, Generic)

instance FromJSON ElicitResponse

main :: IO ()
main = do
    -- Set UTF-8 encoding to handle Unicode characters properly
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    cliArgs <- getArgs
    -- Derive the handlers using Template Haskell where appropriate; resources
    -- are wired manually so the demo URIs and the conformance fixtures coexist.
    let promptsH = $(derivePromptHandler ''MyPrompt 'handlePrompt)
        toolsH   = $(deriveToolHandler ''MyTool 'handleTool)
        info = McpServerInfo
            { serverName = "Complete Example MCP Server"
            , serverVersion = "0.3.0"
            , serverInstructions = "An example MCP server that handles prompts, resources, and tools."
            }
        handlers = McpServerHandlers
            { prompts           = Just promptsH
            , resources         = Just conformanceResources
            , resourceTemplates = Just conformanceResourceTemplates
            , tools             = Just toolsH
            , completions       = Nothing
            }
    case cliArgs of
        ["--http", portStr] | Just port <- readMaybe portStr ->
            runMcpServerHttpWithConfig
                HttpConfig
                    { httpPort = port
                    , httpHost = "localhost"
                    , httpEndpoint = "/mcp"
                    , httpVerbose = False
                    }
                info handlers
        ["--stdio"] -> runMcpServerStdio info handlers
        []          -> runMcpServerStdio info handlers
        _ -> do
          hPutStrLn stderr "Usage: complete-example [--stdio | --http <port>]"
          exitFailure
