{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Database (
       Database,
       Key, Value,
       createDB,
       get, set,
       rcdata,
  ) where

import Control.Distributed.Process
import Control.Distributed.Process.Closure

import qualified Data.Map as Map
import Data.Map (Map)
import Control.Concurrent.STM
import Data.Typeable
import Data.Binary (Binary)
import GHC.Generics

type Key   = String
type Value = String

type Database = ProcessId

data PMessage = Get Key ProcessId
              | Put Key Value
              | QueryResult (Maybe Value)
              deriving (Typeable, Generic)

instance Binary PMessage

processMessage :: Map Key Value -> Process ()
processMessage mem = do
  pm <- expect
  case pm of
    Get k pid -> do
      send pid (QueryResult (Map.lookup k mem))
      processMessage mem
    Put k v -> do
      let mem' = Map.insert k v mem
      processMessage mem'
    _ -> processMessage mem

remotable ['processMessage]

createDB :: [NodeId] -> Process Database
createDB nodes = do
  nid <- getSelfNode
  spawn nid ($(mkClosure 'processMessage) (Map.empty :: Map Key Value))

set :: Database -> Key -> Value -> Process ()
set db k v = send db (Put k v)

get :: Database -> Key -> Process (Maybe Value)
get db k = do
  pid <- getSelfPid
  send db (Get k pid)
  pm <- expect
  case pm of
    QueryResult mv -> pure mv
    _ -> say "Fuck!" >> pure Nothing

rcdata :: RemoteTable -> RemoteTable
rcdata = __remoteTable
  -- For the exercise, change this to include your
  -- remote metadata, e.g. rcdata = Database.__remoteTable
