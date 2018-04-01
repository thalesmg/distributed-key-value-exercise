{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Worker where

import Control.Distributed.Process
import Control.Distributed.Process.Closure

import qualified Data.Map as Map
import Data.Map (Map)
import Control.Concurrent.STM
import Data.Typeable
import Data.Binary (Binary)
import GHC.Generics
