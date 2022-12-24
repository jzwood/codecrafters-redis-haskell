{-# LANGUAGE OverloadedStrings #-}

module Handler where

import Data.ByteString.Char8 (ByteString, any, append, cons, pack, unpack)
import Parse (RAST(..))
import Store
import Data.Char (toUpper)

--type Store = TVar (Map ByteString ByteString)

--newStore :: IO Store
--newStore = newTVar Map.empty

--read :: Store -> ByteString -> IO ByteString
--read key = undefined -- readTVarIO :: TVar a -> IO a

--write :: Store -> ByteString -> IO Bool

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
handle store (Array [BulkString req, rast]) =
    case upper req of
        "ECHO" -> handle store rast
        "PING" -> handle store rast
        --"SET" -> handle store rast -- TODO
        err -> error (unpack err)
handle store (Array [BulkString req]) =
    case upper req of
        "PING" -> return "PONG"
        "GET"  -> do
            maybeResp <- Store.read store req
            return ""
        --read :: Store -> ByteString -> IO (Maybe ByteString)
        err -> error (unpack err)
handle store (Array arr) = error $ show arr
