{-# LANGUAGE OverloadedStrings #-}

-- Verifies the JSON wire shape for each 'Content' constructor matches
-- the MCP 2025-06-18 spec exactly. The conformance suite asserts these
-- shapes byte-for-byte, so any drift here surfaces as conformance failures.
module Spec.ContentEncoding (spec) where

import           Data.Aeson           (Value (..), object, toJSON, (.=))
import qualified Data.Aeson.Key       as Key
import qualified Data.Aeson.KeyMap    as KM
import           MCP.Server
import           Network.URI          (URI, parseURI)
import           Test.Hspec

mustParseURI :: String -> URI
mustParseURI s = case parseURI s of
  Just u  -> u
  Nothing -> error $ "test setup: invalid URI " <> s

spec :: Spec
spec = describe "Content JSON wire shapes" $ do

  it "encodes ContentText as {type:text, text:...}" $
    toJSON (ContentText "hello") `shouldBe` object
      [ "type" .= ("text" :: String)
      , "text" .= ("hello" :: String)
      ]

  it "encodes ContentImage as {type:image, data, mimeType}" $
    toJSON (ContentImage (ContentImageData "AAAA" "image/png")) `shouldBe` object
      [ "type"     .= ("image" :: String)
      , "data"     .= ("AAAA" :: String)
      , "mimeType" .= ("image/png" :: String)
      ]

  it "encodes ContentAudio as {type:audio, data, mimeType}" $
    toJSON (ContentAudio (ContentAudioData "BBBB" "audio/wav")) `shouldBe` object
      [ "type"     .= ("audio" :: String)
      , "data"     .= ("BBBB" :: String)
      , "mimeType" .= ("audio/wav" :: String)
      ]

  it "encodes ContentResourceLink as {type:resource_link, uri, name, mimeType?}" $ do
    let uri = mustParseURI "test://foo"
        link = ResourceLinkData uri "foo" Nothing (Just "text/plain")
    toJSON (ContentResourceLink link) `shouldBe` object
      [ "type"     .= ("resource_link" :: String)
      , "uri"      .= ("test://foo" :: String)
      , "name"     .= ("foo" :: String)
      , "mimeType" .= ("text/plain" :: String)
      ]

  it "encodes ContentEmbeddedResource (text variant) as {type:resource, resource:{uri, mimeType, text}}" $ do
    let uri = mustParseURI "test://embed"
        rc  = ResourceText uri "text/plain" "embedded body"
    case toJSON (ContentEmbeddedResource rc) of
      Object obj -> do
        KM.lookup (Key.fromString "type") obj `shouldBe` Just (String "resource")
        case KM.lookup (Key.fromString "resource") obj of
          Just (Object inner) -> do
            KM.lookup (Key.fromString "uri") inner      `shouldBe` Just (String "test://embed")
            KM.lookup (Key.fromString "mimeType") inner `shouldBe` Just (String "text/plain")
            KM.lookup (Key.fromString "text") inner     `shouldBe` Just (String "embedded body")
          other -> expectationFailure $ "expected nested object for `resource`, got: " <> show other
      other -> expectationFailure $ "expected outer object, got: " <> show other

  it "encodes ContentEmbeddedResource (blob variant) as {type:resource, resource:{uri, mimeType, blob}}" $ do
    let uri = mustParseURI "test://blob"
        rc  = ResourceBlob uri "image/png" "BASE64"
    case toJSON (ContentEmbeddedResource rc) of
      Object obj -> case KM.lookup (Key.fromString "resource") obj of
        Just (Object inner) ->
          KM.lookup (Key.fromString "blob") inner `shouldBe` Just (String "BASE64")
        other -> expectationFailure $ "expected nested object for `resource`, got: " <> show other
      other -> expectationFailure $ "expected outer object, got: " <> show other
