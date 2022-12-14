{-# LANGUAGE OverloadedStrings #-}
module Main where

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

respond :: Socket -> IO ()
respond conn = do
    resp <- recv conn 1024
    send conn "+PONG\r\n"
    respond conn

main :: IO ()
main = do
    putStrLn "Logs from your program will appear here"

    sock <- socket AF_INET Stream defaultProtocol
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 6379 0)
    listen sock 5
    (conn, address) <- accept sock
    respond conn
    print "BYEEEE"
    return ()
