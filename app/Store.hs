{-# LANGUAGE TupleSections #-}

module Store where

import Data.ByteString.Char8 (ByteString, any, append, cons, pack, unpack)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (filter, lookup)

type Map = [(ByteString, ByteString)]
type Store = IORef Map

newStore :: IO Store
newStore = newIORef []

debug :: Store -> IO ()
debug store = do
    map <- readIORef store
    print map
    return ()

read :: Store -> ByteString -> IO (Maybe ByteString)
read store key = do
    map <- readIORef store
    return $ lookup key map

remove :: ByteString -> Map -> Map
remove key = filter ((/= key) . fst)

insert :: ByteString -> ByteString -> Map -> Map
insert key val map = (key, val) : remove key map

write :: Store -> ByteString -> ByteString -> IO ()
write store key val = atomicModifyIORef' store $ (,()) . insert key val

evict :: Store -> ByteString -> IO ()
evict store key = atomicModifyIORef' store $ (,()) . remove key
