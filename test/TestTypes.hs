{-# LANGUAGE OverloadedStrings #-}

module TestTypes where

import           Data.Text  (Text)
import qualified Data.Text  as T
import           MCP.Server (Content(..), McpSession, PromptMessage,
                             ResourceContent(..), ToolResult, toolText,
                             userMessage)
import           Network.URI (URI)

-- Test data types for end-to-end testing
data TestPrompt
    = SimplePrompt { message :: Text }
    | ComplexPrompt { title :: Text, priority :: Int, urgent :: Bool }
    | OptionalPrompt { required :: Text, optional :: Maybe Int }
    deriving (Show, Eq)

data TestResource
    = ConfigFile
    | DatabaseConnection
    | UserProfile
    deriving (Show, Eq)

data TestTool
    = Echo { text :: Text }
    | Calculate { operation :: Text, x :: Int, y :: Int }
    | Toggle { flag :: Bool }
    | Search { query :: Text, limit :: Maybe Int, caseSensitive :: Maybe Bool }
    deriving (Show, Eq)

-- Test separate parameter types approach (should fail with current implementation)
data GetValueParams = GetValueParams { _gvpKey :: Text }
    deriving (Show, Eq)
data SetValueParams = SetValueParams { _svpKey :: Text, _svpValue :: Text }
    deriving (Show, Eq)

data SeparateParamsTool
    = GetValue GetValueParams
    | SetValue SetValueParams
    deriving (Show, Eq)

-- Test recursive parameter types
data InnerParams = InnerParams { _ipName :: Text, _ipAge :: Int }
    deriving (Show, Eq)
data MiddleParams = MiddleParams InnerParams
    deriving (Show, Eq)
data RecursiveTool = ProcessData MiddleParams
    deriving (Show, Eq)

-- Handler functions
handleTestPrompt :: TestPrompt -> IO [PromptMessage]
handleTestPrompt (SimplePrompt msg) =
    pure [userMessage (ContentText ("Simple prompt: " <> msg))]
handleTestPrompt (ComplexPrompt title prio urgent) =
    pure [userMessage (ContentText ("Complex prompt: " <> title <> " (priority=" <> T.pack (show prio) <> ", urgent=" <> T.pack (show urgent) <> ")"))]
handleTestPrompt (OptionalPrompt req opt) =
    pure [userMessage (ContentText ("Optional prompt: " <> req <> maybe "" ((" optional=" <>) . T.pack . show) opt))]

handleTestResource :: URI -> TestResource -> IO ResourceContent
handleTestResource uri ConfigFile =
    pure $ ResourceText uri "text/plain" "Config file contents: debug=true, timeout=30"
handleTestResource uri DatabaseConnection =
    pure $ ResourceText uri "text/plain" "Database at localhost:5432"
handleTestResource uri UserProfile =
    pure $ ResourceText uri "text/plain" "User profile for ID 123"

handleTestTool :: TestTool -> McpSession IO -> IO ToolResult
handleTestTool (Echo text) _ =
    pure $ toolText $ "Echo: " <> text
handleTestTool (Calculate op x y) _ =
    let result = case op of
            "add" -> x + y
            "multiply" -> x * y
            "subtract" -> x - y
            _ -> 0
    in pure $ toolText $ T.pack (show result)
handleTestTool (Toggle flag) _ =
    pure $ toolText $ "Flag is now: " <> T.pack (show (not flag))
handleTestTool (Search query limit caseSens) _ =
    pure $ toolText $ "Search results for '" <> query <> "'" <>
        maybe "" ((" (limit=" <>) . (<> ")") . T.pack . show) limit <>
        maybe "" ((" (case-sensitive=" <>) . (<> ")") . T.pack . show) caseSens

-- Handler for separate params tool
handleSeparateParamsTool :: SeparateParamsTool -> McpSession IO -> IO ToolResult
handleSeparateParamsTool (GetValue (GetValueParams key)) _ =
    pure $ toolText $ "Getting value for key: " <> key
handleSeparateParamsTool (SetValue (SetValueParams key value)) _ =
    pure $ toolText $ "Setting " <> key <> " = " <> value

-- Handler for recursive tool
handleRecursiveTool :: RecursiveTool -> McpSession IO -> IO ToolResult
handleRecursiveTool (ProcessData (MiddleParams (InnerParams name age))) _ =
    pure $ toolText $ "Processing data for " <> name <> " (age " <> T.pack (show age) <> ")"

-- Type covering all parseable field types for exhaustive parsing tests
data AllTypesTool
    = RequiredFields
        { rfText :: Text
        , rfInt :: Int
        , rfInteger :: Integer
        , rfDouble :: Double
        , rfFloat :: Float
        , rfBool :: Bool
        }
    | OptionalFields
        { ofText :: Maybe Text
        , ofInt :: Maybe Int
        , ofInteger :: Maybe Integer
        , ofDouble :: Maybe Double
        , ofFloat :: Maybe Float
        , ofBool :: Maybe Bool
        }
    deriving (Show, Eq)

handleAllTypesTool :: AllTypesTool -> McpSession IO -> IO ToolResult
handleAllTypesTool (RequiredFields t i ig d f b) _ =
    pure $ toolText $ T.intercalate ", "
        [ "text=" <> t
        , "int=" <> T.pack (show i)
        , "integer=" <> T.pack (show ig)
        , "double=" <> T.pack (show d)
        , "float=" <> T.pack (show f)
        , "bool=" <> T.pack (show b)
        ]
handleAllTypesTool (OptionalFields t i ig d f b) _ =
    pure $ toolText $ T.intercalate ", "
        [ "text=" <> maybe "Nothing" id t
        , "int=" <> maybe "Nothing" (T.pack . show) i
        , "integer=" <> maybe "Nothing" (T.pack . show) ig
        , "double=" <> maybe "Nothing" (T.pack . show) d
        , "float=" <> maybe "Nothing" (T.pack . show) f
        , "bool=" <> maybe "Nothing" (T.pack . show) b
        ]

-- Test descriptions for custom description functionality
testDescriptions :: [(String, String)]
testDescriptions =
    [ ("Echo", "Echoes the input text back to the user")
    , ("Calculate", "Performs mathematical calculations")
    , ("text", "The text to echo back")
    , ("operation", "The mathematical operation to perform")
    , ("x", "The first number")
    , ("y", "The second number")
    ]

-- Test descriptions for separate parameter types
separateParamsDescriptions :: [(String, String)]
separateParamsDescriptions =
    [ ("GetValue", "Retrieves a value from the key-value store")
    , ("SetValue", "Sets a value in the key-value store")
    , ("_gvpKey", "The key to retrieve the value for")
    , ("_svpKey", "The key to set the value for")
    , ("_svpValue", "The value to store")
    , ("ProcessData", "Processes user data with age validation")
    , ("_ipName", "The person's full name")
    , ("_ipAge", "The person's age in years")
    ]
