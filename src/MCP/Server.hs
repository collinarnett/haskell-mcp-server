{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MCP.Server
  ( -- * Server Runtime
    runMcpServerStdio
  , runMcpServerHttp
  , runMcpServerHttpWithConfig

    -- * Transport Configuration
  , HttpConfig(..)

    -- * Utility Functions
  , jsonValueToText

    -- * Re-exports
  , module MCP.Server.Types
  ) where

import           MCP.Server.Handlers    (jsonValueToText)
import           MCP.Server.Transport.Stdio (transportRunStdio)
import           MCP.Server.Transport.Http (HttpConfig(..), transportRunHttp, defaultHttpConfig)
import           MCP.Server.Types

-- | Run an MCP server using STDIO transport
runMcpServerStdio :: McpServerInfo -> McpServerHandlers IO -> IO ()
runMcpServerStdio serverInfo handlers = transportRunStdio serverInfo handlers

-- | Run an MCP server using HTTP transport with default configuration
runMcpServerHttp :: McpServerInfo -> McpServerHandlers IO -> IO ()
runMcpServerHttp serverInfo handlers = transportRunHttp defaultHttpConfig serverInfo handlers

-- | Run an MCP server using HTTP transport with custom configuration
runMcpServerHttpWithConfig :: HttpConfig -> McpServerInfo -> McpServerHandlers IO -> IO ()
runMcpServerHttpWithConfig config serverInfo handlers = transportRunHttp config serverInfo handlers
