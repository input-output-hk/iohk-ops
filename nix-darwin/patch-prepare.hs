{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid               ((<>))
import           Data.Text
import           Filesystem.Path.CurrentOS (encodeString)
import           RewriteLibs               (chain)
import           System.Environment        (getEnv)

main :: IO ()
main = do
  path  <- getEnv "out"
  _ <- chain (path <> "/bin") [ pack path <> "/bin/prepare", pack path <> "/bin/nuke-nix" ]
  putStrLn "done"
