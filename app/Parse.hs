{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.ByteString
import Data.Word


data RAST = RSS RedisSimpleString
               -- | Errors String
               -- | Integers [Integer]
               -- | BulkStrings [String]
               | RA RedisArray
               deriving (Show)

data RedisArray = RedisArray Integer [RAST] deriving (Show)
newtype RedisSimpleString = RedisSimpleString ByteString deriving (Show)

--newtype SimpleStrings = SimpleStrings [String]
--"+OK\r\n"

isEOL :: Char -> Bool
isEOL '\n' = True
isEOL '\r' = True
isEOL _ = False

parseRAST :: Parser RAST
parseRAST = (RSS <$> parseSimpleString) <|> (RA <$> parseArrays)

parseSimpleString :: Parser RedisSimpleString
parseSimpleString = RedisSimpleString <$> (char8 '+' *> takeTill isEOL <* endOfLine)

parseArrays :: Parser RedisArray
parseArrays = RedisArray <$> (char8 '*' *> decimal) <*> many' parseRAST


testParse :: IO ()
testParse = do
    let res = parse parseSimpleString "+hello\r\n more content"
    print res
