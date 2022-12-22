{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forever)
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
    case P.runParser resp of
      Right res -> do
        send conn (P.encodeSimpleString res)
        _ <- forkIO $ handle conn
        return ()
      Left _ -> close conn  -- maybe this breaks things but it seems to make them better

main :: IO ()
main = do
    putStrLn "Logs from your program will appear here"

    sock <- socket AF_INET Stream defaultProtocol
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 6379 0)
    listen sock 5
    forever $ do
        (conn, address) <- accept sock
        forkIO $ handle conn
