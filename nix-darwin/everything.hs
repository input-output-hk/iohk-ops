module Main where

import           Data.Monoid
import           NukeNix
import           PrepareDarwin
import           System.Environment

main :: IO ()
main = do
  let
    --decide "process" = process
    decide "prepare"  = prepare
    decide "nuke-nix" = nuke
    decide cmd        = putStrLn $ "command not found: " <> cmd
  progName <- getProgName
  decide progName
