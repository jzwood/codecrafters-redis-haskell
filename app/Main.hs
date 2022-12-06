{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Socket
    ( SocketType(..)
    , Family(..)
    , SocketOption(..)
    , SockAddr(..)
    , socket
    , listen
    , bind
    , accept
    , sendBuf
    , setSocketOption
    , defaultProtocol
    )
import Network.Socket.ByteString

main :: IO ()
main = do
    putStrLn "Logs from your program will appear here"

    sock <- socket AF_INET Stream defaultProtocol
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 6379 0)
    listen sock 5
    (socket, address) <- accept sock
    (msg, sa) <- recvFrom socket 2
    print (socket, "+PONG", sa)
    _ <- sendAllTo socket "+PONG" sa
    print "BYEEEE"
    return ()
