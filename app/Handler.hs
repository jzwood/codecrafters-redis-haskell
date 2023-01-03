{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler where

import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Data.ByteString.Char8 (ByteString, any, append, cons, pack, unpack)
import Data.Char (isDigit, toUpper)
import Data.Maybe (fromMaybe)
import Parse (RAST (..), toResp)
import Store
import Text.Read (readMaybe)

upper :: ByteString -> ByteString
upper = pack . fmap toUpper . unpack

res :: RAST -> IO ByteString
res = return . toResp

microsecondsPerMilliseconds = 1_000

toInt :: ByteString -> Maybe Int
toInt bs = if isInt then Just int else Nothing
  where
    str = unpack bs
    isInt = all isDigit str
    int = Prelude.read str :: Int

-- HANDLER
handle :: Store -> RAST -> IO ByteString
handle store (Array (BulkString cmd : cmds)) =
    case upper cmd of
        "PING" -> handlePing cmds
        "ECHO" -> handleEcho cmds
        "GET" -> handleGet store cmds
        "SET" -> handleSet store cmds
        err -> error (unpack err)

handlePing :: [RAST] -> IO ByteString
handlePing [] = handlePing [SimpleString "PONG"]
handlePing [pong@(SimpleString _)] = res pong
handlePing _ = res $ Error "(error) ERR wrong number of arguments for 'ping' command"

handleEcho :: [RAST] -> IO ByteString
handleEcho [echo@(BulkString _)] = res echo
handleEcho _ = res $ Error "(error) ERR wrong number of arguments for 'echo' command"

handleGet :: Store -> [RAST] -> IO ByteString
handleGet store [BulkString key] = do
    maybeVal <- Store.read store key
    res $ maybe NullBulkString SimpleString maybeVal

handleSet :: Store -> [RAST] -> IO ByteString
handleSet store [BulkString key, BulkString value] = do
    _ <- Store.write store key value
    res (SimpleString "OK")
handleSet store [BulkString key, BulkString value, BulkString opt1, BulkString opt2] =
    case (upper opt1, toInt opt2) of
        ("PX", Just milliseconds) -> do
            _ <- forkIO $ do
                threadDelay $ microsecondsPerMilliseconds * milliseconds
                evict store key
            handleSet store [BulkString key, BulkString value]
        _ -> res $ Error "(error) ERR  syntax"
handleSet _ _ = res $ Error "(error) ERR  syntax"
