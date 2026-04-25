{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Data.Text (Text)

-- High-level data type definitions from SPEC.md

data MyPrompt
    = Recipe { idea :: Text }
    | Shopping { description :: Text }
    -- Conformance fixtures (test_simple_prompt, test_prompt_with_arguments, …
    -- snake-cased from constructor names by 'derivePromptHandler').
    | TestSimplePrompt
    | TestPromptWithArguments { arg1 :: Text, arg2 :: Text }
    | TestPromptWithEmbeddedResource { resourceUri :: Text }
    | TestPromptWithImage
    deriving (Show, Eq)

data MyResource
    = ProductCategories
    | SaleItems
    | HeadlineBannerAd
    deriving (Show, Eq)

data MyTool
    = SearchForProduct { q :: Text, category :: Maybe Text }
    | AddToCart { sku :: Text }
    | Checkout
    | ComplexTool { field1 :: Text, field2 :: Text, field3 :: Maybe Text, field4 :: Text, field5 :: Maybe Text }
    -- Conformance fixtures (test_simple_text, test_image_content, …)
    | TestSimpleText
    | TestImageContent
    | TestAudioContent
    | TestEmbeddedResource
    | TestMultipleContentTypes
    | TestErrorHandling
    | TestToolWithProgress
    | TestToolWithLogging
    | TestSampling { prompt :: Text }
    | TestElicitation { message :: Text }
    deriving (Show, Eq)
