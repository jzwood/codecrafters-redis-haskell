{-# LANGUAGE OverloadedStrings #-}

module Handler where

import Data.Maybe (fromMaybe)
import Control.Monad.STM (atomically)
import Data.ByteString.Char8 (ByteString, any, append, cons, pack, unpack)
import Parse (RAST(..))
import Store
import Data.Char (toUpper)

upper :: ByteString -> ByteString
upper = pack . fmap toUpper . unpack

encodeBulkString :: ByteString -> ByteString
encodeBulkString bs = cons '$' $ pack ((show . length . unpack $ bs) ++ "\r\n") `append` bs `append` pack "\r\n"

encodeSimpleString :: ByteString -> ByteString
encodeSimpleString ss = cons '+' $ ss `append` pack "\r\n"

-- HANDLER
handle :: Store -> RAST -> IO ByteString
handle store (SimpleString ss) = return $ encodeSimpleString ss
handle store (BulkString bs) = return $ encodeBulkString bs
handle store (Array [BulkString cmd]) =
    case upper cmd of
        "PING" -> handle store (SimpleString "PONG")
        err -> error (unpack err)
handle store (Array [BulkString cmd, BulkString arg1]) =
    case upper cmd of
        "ECHO" -> handle store (BulkString arg1)
        "PING" -> handle store (BulkString arg1)
        "GET" -> do
            maybeVal <- Store.read store arg1
            handle store (SimpleString $ fromMaybe "" maybeVal)
        err -> error (unpack err)
handle store (Array [BulkString req, BulkString arg1, BulkString arg2]) =
    case upper req of
        "SET" -> do
            _ <- atomically $ Store.write store arg1 arg2
            handle store (SimpleString "OK")
        err -> error (unpack err)
handle store (Array arr) = error $ show arr
