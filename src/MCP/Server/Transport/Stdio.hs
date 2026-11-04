{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MCP.Server.Transport.Stdio
  ( -- * STDIO Transport
    transportRunStdio
  , transportRunHandles
  ) where

import           Control.Monad          (unless, when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson
import qualified Data.ByteString.Lazy   as BSL
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import qualified Data.Text.IO           as TIO
import           System.IO              (Handle, hFlush, hIsEOF, hSetEncoding,
                                         stderr, stdin, stdout, utf8)

import           MCP.Server.Handlers
import           MCP.Server.JsonRpc
import           MCP.Server.Session (mkStdioSession)
import           MCP.Server.Types

transportRunStdio :: (MonadIO m) => McpServerInfo -> McpServerHandlers m -> m ()
transportRunStdio = transportRunHandles stdin stdout

-- | Serves newline-delimited JSON-RPC read from the first handle, writing
-- responses to the second, until the input ends. Both carry UTF-8, as the
-- spec requires, whatever the process locale: clients often start servers
-- with an environment whose locale is ASCII.
transportRunHandles :: (MonadIO m) => Handle -> Handle -> McpServerInfo -> McpServerHandlers m -> m ()
transportRunHandles input output serverInfo handlers = do
  liftIO $ do
    hSetEncoding stderr utf8
    hSetEncoding input utf8
    hSetEncoding output utf8
  loop
  where
    loop = do
      eof <- liftIO $ hIsEOF input
      unless eof $ do
        line <- liftIO (TIO.hGetLine input)
        when (not $ T.null $ T.strip line) $ do
          liftIO $ TIO.hPutStrLn stderr $ "Received request: " <> line
          case eitherDecode (BSL.fromStrict $ TE.encodeUtf8 line) of
            Left err -> liftIO $ TIO.hPutStrLn stderr $ "Parse error: " <> T.pack err
            Right jsonValue ->
              case parseJsonRpcMessage jsonValue of
                Left err -> liftIO $ TIO.hPutStrLn stderr $ "JSON-RPC parse error: " <> T.pack err
                Right message -> do
                  liftIO $ TIO.hPutStrLn stderr $ "Processing message: " <> T.pack (show (getMessageSummary message))
                  -- Stdio uses a single no-op session: progress/log are
                  -- discarded and sample/elicit error out cleanly.
                  let mkSession _ = mkStdioSession
                  response <- handleMcpMessage serverInfo handlers mkSession message
                  case response of
                    Just responseMsg -> do
                      liftIO $ TIO.hPutStrLn stderr $ "Sending response for: " <> T.pack (show (getMessageSummary message))
                      let responseText = TE.decodeUtf8 $ BSL.toStrict $ encode $ encodeJsonRpcMessage responseMsg
                      liftIO $ TIO.hPutStrLn output responseText
                      liftIO $ hFlush output
                    Nothing -> liftIO $ TIO.hPutStrLn stderr $ "No response needed for: " <> T.pack (show (getMessageSummary message))
        loop
