{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (ByteString, any, pack, unpack, append, cons)
import Data.Char
import Data.Word
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

upper :: ByteString -> ByteString
upper = pack . fmap toUpper . unpack

encodeByteString :: ByteString -> ByteString
encodeByteString bs = cons '$' $ pack ((show . length . unpack $ bs) ++ "\r\n") `append` bs `append` pack "\r\n"

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

handle :: RAST -> ByteString
handle (SimpleString bs) = error (unpack bs)
handle (BulkString bs) = error (unpack bs)
handle (Array [BulkString req, BulkString resp]) =
    case upper req of
        "ECHO" -> resp
        err -> error (unpack err)

runParser :: ByteString -> Either String ByteString
runParser input = handle <$> parseOnly parseRAST input

testParse :: IO ()
testParse = do
    --let res = parse parseSimpleString "+hello\r\n more content"
    --let res = parse parseBulkString "$5\r\nhelllo\r\n"
    let res = runParser "*2\r\n$4\r\nECHO\r\n$4\r\nPONG\r\n"
    --let res = parse $4\r\nECHO\r\n
    print $ res
