{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (ThreadId, forkIO)
import Control.Monad (forever)
import Data.Function
import Data.Functor
import qualified Handler
import Network.Socket (
    Family (..),
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
    socket,
 )
import Network.Socket.ByteString (recv, send)
import Parse (runParser)
import Store (Store, newStore, read, write)

handle :: Socket -> Store -> IO ()
handle conn store = do
    req <- recv conn 1024
    case runParser req of
        Right rast -> do
            res <- Handler.handle store rast
            send conn res
            _ <- forkIO $ handle conn store
            return ()
        Left _ -> close conn

main :: IO ()
main = do
    putStrLn "Logs from your program will appear here"

    sock <- socket AF_INET Stream defaultProtocol
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 6379 0)
    listen sock 5
    store <- newStore
    forever $ do
        (conn, address) <- accept sock
        forkIO $ handle conn store
