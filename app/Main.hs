{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Functor
import Data.Function
import Control.Concurrent (ThreadId, forkIO)
import Control.Monad (forever)
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
import Parse (encodeSimpleString, runParser)
import Store (Store, newStore, read, write)
import qualified Handler

--type Store = TVar (Map ByteString ByteString)

--newStore :: IO Store
--newStore = newTVar Map.empty

--read :: Store -> ByteString -> IO ByteString
--read key = undefined -- readTVarIO :: TVar a -> IO a

--write :: Store -> ByteString -> IO Bool

handle :: Socket -> Store -> IO ()
handle conn store = do
    req <- recv conn 1024
    case fmap Handler.handle (runParser req) of
        Right res -> do
            send conn res
            _ <- forkIO $ handle conn store
            return ()
        Left _ -> close conn -- maybe this breaks things but it seems to make them better

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
