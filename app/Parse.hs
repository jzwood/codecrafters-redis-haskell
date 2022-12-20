{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Prelude hiding (take, any)
import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (ByteString, any, unpack)
import Data.Word

data RAST
    = SimpleString ByteString
    | -- | Errors String
      -- | Integers [Integer]
      BulkString ByteString
    | Array Integer [RAST]
    deriving (Show)

--data RedisArray = RedisArray Integer [RAST] deriving (Show)
--newtype RedisSimpleString = RedisSimpleString ByteString deriving (Show)

--newtype SimpleStrings = SimpleStrings [String]
--"+OK\r\n"

isEOL :: Char -> Bool
isEOL '\n' = True
isEOL '\r' = True
isEOL _ = False

parseRAST :: Parser RAST
parseRAST = parseSimpleString <|> parseArrays

parseSimpleString :: Parser RAST
parseSimpleString =
    SimpleString
        <$> (char8 '+' *> takeTill isEOL <* endOfLine)

parseArrays :: Parser RAST
parseArrays = Array <$> (char8 '*' *> decimal) <*> many' parseRAST

respSafe :: ByteString -> Parser ByteString
respSafe cs = if any isEOL cs then fail (unpack cs) else return cs

parseBulkString :: Parser RAST
parseBulkString = BulkString <$> (char8 '$' *> decimal <* endOfLine >>= take >>= respSafe) <* endOfLine

testParse :: IO ()
testParse = do
    --let res = parse parseSimpleString "+hello\r\n more content"
    let res = parse parseBulkString "$5\r\nhelllo\r\n"
    print res
