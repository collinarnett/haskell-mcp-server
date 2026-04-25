{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Control.Concurrent (threadDelay)
import           Data.Aeson         (toJSON)
import qualified Data.Text          as T
import           MCP.Server
import           MCP.Server.Derive
import           System.Environment (getArgs)
import           System.IO          (hSetEncoding, stderr, stdout, utf8)
import           Text.Read          (readMaybe)
import           Types

-- | Tiny 1×1 transparent PNG used as the test image for prompts and tools that
-- need an image content block. Inlined as base64 to keep the example self-
-- contained.
tinyPngBase64 :: T.Text
tinyPngBase64 =
    "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNkYAAAAAYAAjCB0C8AAAAASUVORK5CYII="

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
handlePrompt (TestPromptWithEmbeddedResource uriText) = do
    -- Conformance passes an arbitrary URI string; if it does not round-trip
    -- through Network.URI we fall back to a sentinel so we still produce a
    -- spec-shaped message rather than crashing the server.
    let uri = case parseURI (T.unpack uriText) of
          Just u  -> u
          Nothing -> case parseURI "test://invalid" of
            Just u  -> u
            Nothing -> error "tinyUri: parseURI failed for sentinel"
    pure
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
handleTool TestEmbeddedResource _ = do
    let uri = case parseURI "test://embedded-resource" of
          Just u  -> u
          Nothing -> error "invariant: test://embedded-resource is a valid URI"
    pure $ toolContent
      [ ContentEmbeddedResource $ ResourceText uri "text/plain" "This is an embedded resource content."
      ]
handleTool TestMultipleContentTypes _ = do
    let uri = case parseURI "test://mixed-content-resource" of
          Just u  -> u
          Nothing -> error "invariant: test://mixed-content-resource is a valid URI"
    pure $ toolContent
      [ ContentText "Multiple content types test:"
      , ContentImage (ContentImageData tinyPngBase64 "image/png")
      , ContentEmbeddedResource $ ResourceText uri "application/json" "{\"test\":\"data\",\"value\":123}"
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
        _ -> error "Usage: complete-example [--stdio | --http <port>]"
