{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spec.JSONConversion (spec) where

import Data.Aeson (Value (..), decode, toJSON)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Data.Text.Encoding as TE
import MCP.Server.Protocol (InitializeResponse (..))
import Data.Text (Text)
import qualified Data.Text as T
import MCP.Server
import Test.Hspec
import Test.QuickCheck
import Text.Read (readMaybe)

-- Arbitrary instance for Text (needed for QuickCheck)
instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary

spec :: Spec
spec = describe "JSON Type Conversion" $ do
  describe "Property-based tests" $ do
    it "round-trips integers correctly" $ property prop_intRoundTrip
    it "round-trips booleans correctly" $ property prop_boolRoundTrip
    it "round-trips text correctly" $ property prop_textRoundTrip

  describe "Manual conversion tests" $ do
    it "converts integers manually" testIntConversion
    it "converts booleans manually" testBoolConversion

  structuredArgumentSpec
  initializeSpec

-- Fixed property-based tests (no more 'error' usage)
prop_intRoundTrip :: Int -> Bool
prop_intRoundTrip i =
    let jsonVal = toJSON i
        textVal = jsonValueToText jsonVal
        parsed = readMaybe (T.unpack textVal)
    in parsed == Just i

prop_boolRoundTrip :: Bool -> Bool
prop_boolRoundTrip b =
    let jsonVal = toJSON b
        textVal = jsonValueToText jsonVal
        parsed = case T.toLower textVal of
                   "true" -> Just True
                   "false" -> Just False
                   _ -> Nothing
    in parsed == Just b

prop_textRoundTrip :: Text -> Bool
prop_textRoundTrip t =
    let jsonVal = toJSON t
        textVal = jsonValueToText jsonVal
    in textVal == t

-- Manual test functions converted to Hspec style
testIntConversion :: IO ()
testIntConversion = do
    let testCases = [0, 42, -17, 999999] :: [Int]
    results <- mapM testSingleInt testCases
    all id results `shouldBe` True
  where
    testSingleInt i = do
        let jsonVal = toJSON i
            textVal = jsonValueToText jsonVal
        case readMaybe (T.unpack textVal) of
            Just parsed -> return (parsed == i)
            Nothing -> do
                expectationFailure $ "Failed to parse Int from: " <> T.unpack textVal
                return False

testBoolConversion :: IO ()
testBoolConversion = do
    let testCases = [True, False]
    results <- mapM testSingleBool testCases
    all id results `shouldBe` True
  where
    testSingleBool b = do
        let jsonVal = toJSON b
            textVal = jsonValueToText jsonVal
            expected = if b then "true" else "false"
        case T.toLower textVal of
            result | result == expected -> return True
            result -> do
                expectationFailure $ "Expected " <> T.unpack expected <> " but got " <> T.unpack result
                return False

-- | Arrays and objects once reached handlers as Haskell's 'show' of the
-- aeson value (@Array [String "a"]@), which no handler could decode.
structuredArgumentSpec :: Spec
structuredArgumentSpec = describe "Structured arguments" $ do
  it "hands an array to the handler as JSON that decodes to it" $
    property $ \(items :: [Text]) ->
      decode (LBS.fromStrict (TE.encodeUtf8 (jsonValueToText (toJSON items)))) == Just items
  it "hands an object to the handler as JSON that decodes to it" $
    property $ \(entries :: [(Text, Int)]) ->
      let fields = Map.fromList entries
       in decode (LBS.fromStrict (TE.encodeUtf8 (jsonValueToText (toJSON fields)))) == Just fields

-- | Clients read the server's instructions from the top level of the
-- initialize result; nested in serverInfo they were never seen.
initializeSpec :: Spec
initializeSpec = describe "Initialize response" $
  it "carries the instructions at the top level of the result" $ do
    let response =
          InitializeResponse
            { initRespProtocolVersion = "2025-06-18"
            , initRespCapabilities = ServerCapabilities Nothing Nothing Nothing Nothing Nothing
            , initRespServerInfo = McpServerInfo "test" "1" "Read the manual."
            }
    case toJSON response of
      Object result -> do
        KeyMap.lookup "instructions" result `shouldBe` Just (String "Read the manual.")
        (KeyMap.lookup "serverInfo" result >>= \case
            Object info -> KeyMap.lookup "instructions" info
            _ -> Nothing) `shouldBe` Nothing
      other -> expectationFailure ("not an object: " <> show other)
