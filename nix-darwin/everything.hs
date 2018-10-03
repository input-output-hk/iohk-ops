{-# LANGUAGE LambdaCase #-}
module Main where

import           Data.Monoid
import           NukeNix
import           PrepareDarwin
import           System.Environment

main :: IO ()
main = getProgName >>= \case
  "prepare" -> prepare
  "nuke-nix" -> nuke
  _ -> putStrLn "invalid command ran"
