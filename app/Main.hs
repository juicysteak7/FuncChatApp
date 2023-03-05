module Main (main) where

import System.Environment (getArgs)
import Server (mainServer)
import Client (mainClient)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["server"] -> mainServer
    ["client"] -> mainClient
    _ -> putStrLn "Invalid command. Usage: stack run <server/client>"