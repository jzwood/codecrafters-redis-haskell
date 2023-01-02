{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 (Parser, char8, count, decimal, endOfLine, parseOnly, take, takeTill)
import Data.ByteString.Char8 (ByteString, any, append, cons, pack, unpack)
import Prelude hiding (any, take)

data RAST
    = SimpleString ByteString
    | Error ByteString
    | -- | Integers [Integer]
      BulkString ByteString
    | NullBulkString
    | Array [RAST]
    deriving (Show)

class Resp rast where
    toResp :: rast -> ByteString

instance Resp RAST where
    toResp (SimpleString ss) = cons '+' $ ss `append` pack "\r\n"
    toResp (BulkString bs) = cons '$' $ pack ((show . length . unpack $ bs) ++ "\r\n") `append` bs `append` pack "\r\n"
    toResp NullBulkString = "$-1\r\n"
    toResp (Error es) = cons '-' $ es `append` pack "\r\n"

-- UTILS
isEOL :: Char -> Bool
isEOL '\n' = True
isEOL '\r' = True
isEOL _ = False

respSafe :: ByteString -> Parser ByteString
respSafe bs = if any isEOL bs then fail (unpack bs) else return bs

-- COMBINATORS
parseRAST :: Parser RAST
parseRAST =
    parseSimpleString
        <|> parseBulkString
        <|> parseArrays

parseSimpleString :: Parser RAST
parseSimpleString =
    SimpleString <$> (char8 '+' *> takeTill isEOL <* endOfLine)

parseBulkString :: Parser RAST
parseBulkString = BulkString <$> (char8 '$' *> decimal <* endOfLine >>= take >>= respSafe) <* endOfLine

parseArrays :: Parser RAST
parseArrays = Array <$> (char8 '*' *> decimal <* endOfLine >>= flip count parseRAST)

runParser :: ByteString -> Either String RAST
runParser = parseOnly parseRAST
