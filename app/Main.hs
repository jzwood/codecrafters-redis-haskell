{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
  ( forkIO
  , ThreadId
  )
import Network.Socket
    ( SocketType(..)
    , ShutdownCmd(..)
    , Family(..)
    , SocketOption(..)
    , SockAddr(..)
    , Socket
    , shutdown
    , socket
    , listen
    , bind
    , close
    , accept
    , setSocketOption
    , defaultProtocol
    )
import Network.Socket.ByteString

-- trigger test
handle :: Socket -> IO ()
handle conn = do
    resp <- recv conn 1024
    send conn "+PONG\r\n"
    --shutdown conn ShutdownBoth
    _ <- forkIO $ handle conn
    return ()

loop :: Socket -> IO ()
loop sock = do
  (conn, address) <- accept sock
  print ("B", conn, address)
  handle conn
  loop sock

main :: IO ()
main = do
    putStrLn "Logs from your program will appear here"

    sock <- socket AF_INET Stream defaultProtocol
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 6379 0)
    listen sock 5
    loop sock
    print "BYEEEE"
    return ()
