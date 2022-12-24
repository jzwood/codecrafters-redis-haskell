{-# LANGUAGE OverloadedStrings #-}

module Handler where

import Data.ByteString.Char8 (ByteString, any, append, cons, pack, unpack)
import Parse (RAST(..))
import Data.Char (toUpper)



upper :: ByteString -> ByteString
upper = pack . fmap toUpper . unpack

-- HANDLER
handle :: RAST -> ByteString
handle (SimpleString bs) = error (unpack bs)
handle (BulkString bs) = error (unpack bs)
handle (Array [BulkString req, BulkString resp]) =
    case upper req of
        "ECHO" -> resp
        "PING" -> resp
        err -> error (unpack err)
handle (Array [BulkString cmd]) =
    case upper cmd of
        "PING" -> "PONG"
        err -> error (unpack err)
handle (Array arr) = error $ show arr
