{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 (Parser, char8, count, decimal, endOfLine, parseOnly, take, takeTill)
import Data.ByteString.Char8 (ByteString, any, append, cons, pack, unpack)
import Prelude hiding (any, take)

data RAST
    = SimpleString ByteString
    | -- | Errors String
      -- | Integers [Integer]
      BulkString ByteString
    | Array [RAST]
    deriving (Show)

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
