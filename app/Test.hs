{-# LANGUAGE OverloadedStrings #-}

module Test where

import Data.Attoparsec.ByteString.Char8 (parseOnly)
import Parse (parseRAST, runParser)

-- TEST
res1 = runParser "*2\r\n$4\r\nECHO\r\n$4\r\nPONG\r\n"
res2 = parseOnly parseRAST "*2\r\n$4\r\nECHO\r\n$4\r\nPONG\r\n"
res3 = parseOnly parseRAST "*3\r\n$4\r\nECHO\r\n$4\r\nPONG\r\n+HIII\r\n"
