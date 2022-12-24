module Store where

import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TVar (TVar, newTVarIO)
import Data.ByteString.Char8 (ByteString, any, append, cons, pack, unpack)
import Data.Map (Map)
import qualified Data.Map as Map

type Store = TVar (Map ByteString ByteString)

newStore :: IO Store
newStore = newTVarIO Map.empty

read :: Store -> ByteString -> IO ByteString
read key = undefined -- readTVarIO :: TVar a -> IO a

write :: Store -> ByteString -> IO Bool
write = undefined
