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

import Worker

import Control.Monad (forM, forM_)
import Data.Char (ord)
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Concurrent.STM
import Data.Typeable
import Data.Binary (Binary)
import GHC.Generics

set :: Database -> Key -> Value -> Process ()
set db k v = send db (Put k v)

get :: Database -> Key -> Process (Maybe Value)
get db k = do
  (sendP, recvP) <- newChan
  send db (Get k sendP)
  receiveChan recvP

getWorker :: [ProcessId] -> Char -> ProcessId
getWorker workers c = workers !! ((ord c) `mod` (length workers))

type Key   = String
type Value = String

type Database = ProcessId

data PMessage = Get Key (SendPort (Maybe Value))
              | Put Key Value
              deriving (Typeable, Generic)

instance Binary PMessage

processMessage :: Map Key Value -> Process ()
processMessage mem = do
  pm <- expect
  case pm of
    Get k sendP -> do
      liftIO $ putStrLn $ "me pediram " ++ k
      sendChan sendP (Map.lookup k mem)
      processMessage mem
    Put k v -> do
      liftIO $ putStrLn $ "vô gravá " ++ v
      let mem' = Map.insert k v mem
      processMessage mem'

foreman :: [ProcessId] -> Process ()
foreman [] = say "não tem trabaiadô"
foreman workers = do
  forM_ workers monitor
  go workers
  where
    go :: [ProcessId] -> Process ()
    go workers = do
      receiveWait
        [ match $ \pm -> do
            case pm of
              Get k sendP -> do
                case k of
                  "" -> say "empty key"
                  c:cs -> send (getWorker workers c) pm
              Put k v ->
                case k of
                  "" -> say "empty key"
                  c:cs -> send (getWorker workers c) pm
            foreman workers
        , match $ \(ProcessMonitorNotification _ref deadpid reason) -> do
            say $ "pid " ++ show deadpid ++ " died. reason: " ++ show reason
            foreman (filter (/= deadpid) workers)
        ]

remotable ['foreman, 'processMessage]

createDB :: [NodeId] -> Process Database
createDB nodes = do
  say $ "nós: " ++ show nodes
  myNid <- getSelfNode
  pids <- forM nodes $ \nid -> spawn nid ($(mkClosure 'processMessage) (Map.empty :: Map Key Value))
  spawn myNid ($(mkClosure 'foreman) pids)

rcdata :: RemoteTable -> RemoteTable
rcdata = Database.__remoteTable
  -- For the exercise, change this to include your
  -- remote metadata, e.g. rcdata = Database.__remoteTable
