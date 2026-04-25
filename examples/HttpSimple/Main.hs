{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Data.IORef
import           MCP.Server
import           MCP.Server.Derive
import           System.IO         (hPutStrLn, stderr)
import           Types

main :: IO ()
main = do
    hPutStrLn stderr "Starting HTTP Simple MCP Server..."

    -- Create a simple in-memory store
    store <- newIORef []

    let handleTool :: SimpleTool -> McpSession IO -> IO ToolResult
        handleTool (GetValue k) _ = do
            pairs <- readIORef store
            case lookup k pairs of
                Nothing -> pure $ toolText $ "Key '" <> k <> "' not found"
                Just v  -> pure $ toolText v
        handleTool (SetValue k v) _ = do
            pairs <- readIORef store
            let newPairs = (k, v) : filter ((/= k) . fst) pairs
            writeIORef store newPairs
            pure $ toolText $ "Set '" <> k <> "' to '" <> v <> "'"

    -- Derive the tool handlers using Template Haskell with descriptions
    let tools = $(deriveToolHandlerWithDescription ''SimpleTool 'handleTool simpleDescriptions)
     in runMcpServerHttp
        McpServerInfo
            { serverName = "HTTP Simple Key-Value MCP Server"
            , serverVersion = "1.0.0"
            , serverInstructions = "A simple HTTP key-value store with GetValue and SetValue tools"
            }
        McpServerHandlers
            { prompts           = Nothing
            , resources         = Nothing
            , resourceTemplates = Nothing
            , tools             = Just tools
            , completions       = Nothing
            }
