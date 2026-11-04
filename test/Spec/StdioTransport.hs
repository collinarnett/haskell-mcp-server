{-# LANGUAGE OverloadedStrings #-}

module Spec.StdioTransport (spec) where

import Control.Exception (bracket)
import Data.Aeson (Value (..), decodeStrict, encode, object, (.=))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import GHC.IO.Encoding (getLocaleEncoding, mkTextEncoding, setLocaleEncoding)
import MCP.Server.Transport.Stdio (transportRunHandles)
import MCP.Server.Types
import System.IO (hClose)
import System.Process (createPipe)
import Test.Hspec

-- | A server started with an ASCII locale, as MCP clients often start
-- them, died reading the first request that held a non-ASCII character:
-- the input was decoded with the locale instead of UTF-8.
spec :: Spec
spec = describe "Stdio transport" $
  it "reads and answers UTF-8 whatever the locale" $
    withAsciiLocale $ do
      reply <- exchange (callEcho "café ☕ 日本")
      echoed reply `shouldBe` Just "café ☕ 日本"

withAsciiLocale :: IO a -> IO a
withAsciiLocale action = do
  ascii <- mkTextEncoding "ASCII"
  bracket getLocaleEncoding setLocaleEncoding (\_ -> setLocaleEncoding ascii >> action)

-- | Runs the transport over pipes until the request is answered and the
-- input ends, returning the first line written.
exchange :: Value -> IO BS.ByteString
exchange request = do
  (inputRead, inputWrite) <- createPipe
  (outputRead, outputWrite) <- createPipe
  BS.hPut inputWrite (LBS.toStrict (encode request) <> "\n")
  hClose inputWrite
  transportRunHandles inputRead outputWrite serverInfo handlers
  hClose outputWrite
  BS.hGetLine outputRead

callEcho :: Text -> Value
callEcho text =
  object
    [ "jsonrpc" .= ("2.0" :: Text)
    , "id" .= (1 :: Int)
    , "method" .= ("tools/call" :: Text)
    , "params" .= object ["name" .= ("echo" :: Text), "arguments" .= object ["text" .= text]]
    ]

echoed :: BS.ByteString -> Maybe Text
echoed line = do
  Object reply <- decodeStrict line
  Object result <- KeyMap.lookup "result" reply
  Array content <- KeyMap.lookup "content" result
  [Object first] <- pure (foldr (:) [] content)
  String text <- KeyMap.lookup "text" first
  pure text

serverInfo :: McpServerInfo
serverInfo = McpServerInfo "echo" "1" ""

handlers :: McpServerHandlers IO
handlers =
  McpServerHandlers
    { prompts = Nothing
    , resources = Nothing
    , resourceTemplates = Nothing
    , tools = Just (pure [], \_ _ args -> pure (Right (ToolResult [ContentText (maybe "" id (lookup "text" args))] False)))
    , completions = Nothing
    }
