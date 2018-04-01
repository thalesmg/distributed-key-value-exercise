{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Distributed.Process
import Control.Monad.IO.Class
import Control.Monad
import Data.List
import System.IO

import DistribUtils

import Database  (Database, createDB, get, set, rcdata)

main :: IO ()
main = distribMain master rcdata

master :: [NodeId] -> Process ()
master peers = do
  say "começando"
  db <- createDB peers

  say "lendo"
  f <- liftIO $ readFile "src/Database.hs"
  let ws = words f

  zipWithM_ (set db) ws (tail ws)

  say "boom"

  get db "module" >>= liftIO . print
  get db "xxxx"   >>= liftIO . print

  forever $ do
    l <- liftIO $ do
      putStr "command: "
      hFlush stdout
      getLine
    case l of
      (stripPrefix "get " -> Just k) -> do
        r <- get db k
        liftIO $ putStrLn ("response: " ++ show r)
      ((fmap words <$> stripPrefix "put ") -> Just [k, v]) -> set db k v
      _ -> liftIO $ putStrLn $ "invalid command: " ++ l

  return ()
