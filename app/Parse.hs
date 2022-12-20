{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.ByteString
import Data.Word

data RAST
    = SimpleString ByteString
    | -- | Errors String
      -- | Integers [Integer]
      BulkString Integer ByteString
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

parseBulkString :: Parser RAST
parseBulkString = BulkString <$> (char8 '$' *> decimal) <*> takeTill isEOL <* endOfLine

testParse :: IO ()
testParse = do
    let res = parse parseSimpleString "+hello\r\n more content"
    print res
