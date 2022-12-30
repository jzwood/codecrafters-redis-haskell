module Store where

import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.ByteString.Char8 (ByteString, any, append, cons, pack, unpack)
import Data.List (lookup, filter)


type Map = [(ByteString, ByteString)]
type Store = IORef Map

newStore :: IO Store
newStore = newIORef []

read :: Store -> ByteString -> IO (Maybe ByteString)
read store key = do
    map <- readIORef store
    return $ lookup key map

insert :: ByteString -> ByteString -> Map -> Map
insert key val map = (key, val) : filter ((==key) . fst) map

write :: Store -> ByteString -> ByteString -> IO ()
write store key val = modifyIORef' store $ insert key val
