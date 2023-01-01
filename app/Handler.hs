{-# LANGUAGE OverloadedStrings #-}

module Handler where

import Data.ByteString.Char8 (ByteString, any, append, cons, pack, unpack)
import Data.Char (toUpper)
import Data.Maybe (fromMaybe)
import Parse (RAST (..), toResp)
import Store

upper :: ByteString -> ByteString
upper = pack . fmap toUpper . unpack

res :: RAST -> IO ByteString
res = return . toResp

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
handlePing [] = handlePing [BulkString "PONG"]
handlePing [pong@(BulkString _)] = res pong
handlePing _ = res $ Error "(error) ERR wrong number of arguments for 'ping' command"

handleEcho :: [RAST] -> IO ByteString
handleEcho [echo@(BulkString _)] = res echo
handleEcho _ = res $ Error "(error) ERR wrong number of arguments for 'echo' command"

handleGet :: Store -> [RAST] -> IO ByteString
handleGet store [BulkString key] = do
    maybeVal <- Store.read store key
    res $ maybe (Error "nil") SimpleString maybeVal

handleSet :: Store -> [RAST] -> IO ByteString
handleSet store [BulkString key, BulkString value] = do
    _ <- Store.write store key value
    res (SimpleString "OK")

--handleSet :: Store -> [RAST] -> IO ByteString
--handleSet store [BulkString key, BulkString value, BulkString opt1, BulkString opt2] = do
----case upper opt1 of
----"PX" ->
--_ <- Store.write store key value
--res (SimpleString "OK")
