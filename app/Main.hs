{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (forever)
import Network.Socket
    ( SocketType(..)
    , Family(..)
    , SocketOption(..)
    , SockAddr(..)
    , Socket
    , socket
    , listen
    , bind
    , close
    , accept
    , setSocketOption
    , defaultProtocol
    )
import Network.Socket.ByteString

handleSocket :: Socket -> IO ()
handleSocket sock = forever $ do
    (conn, address) <- accept sock
    _ <- send conn "+PONG\r\n"
    return ()

main :: IO ()
main = do
    putStrLn "Logs from your program will appear here"

    sock <- socket AF_INET Stream defaultProtocol
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 6379 0)
    listen sock 5
    handleSocket sock
    print "BYEEEE"
    return ()
