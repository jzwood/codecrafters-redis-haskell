module Store where

import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, modifyTVar, readTVarIO)
import Data.ByteString.Char8 (ByteString, any, append, cons, pack, unpack)
import Data.Map (Map)
import qualified Data.Map as Map

type Store = TVar (Map ByteString ByteString)

newStore :: IO Store
newStore = newTVarIO Map.empty

read :: Store -> ByteString -> IO (Maybe ByteString)
read store key = do
    map <- readTVarIO store
    return $ Map.lookup key map

write :: Store -> ByteString -> ByteString -> STM ()
write store key val = modifyTVar store $ Map.insert key val
