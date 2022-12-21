{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (
    ThreadId,
    forkIO,
 )
import Network.Socket (
    Family (..),
    ShutdownCmd (..),
    SockAddr (..),
    Socket,
    SocketOption (..),
    SocketType (..),
    accept,
    bind,
    close,
    defaultProtocol,
    listen,
    setSocketOption,
    shutdown,
    socket,
 )
import Network.Socket.ByteString
import qualified Parse as P

handle :: Socket -> IO ()
handle conn = do
    resp <- recv conn 1024
    print $ P.runParser resp
    case P.runParser resp of
      Right res -> do
        send conn (P.encodeSimpleString res)
        _ <- forkIO $ handle conn
        return ()
      Left _ -> return ()

loop :: Socket -> IO ()
loop sock = do
    (conn, address) <- accept sock
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
