module CommandLineUtils
  ( filePathOption
  , directoryOption
  ) where

-- TODO: DEVOPS-1066 clean up command line option parsing and fix various issues.

import Prelude hiding (FilePath)
import Options.Applicative
import qualified Filesystem.Path.CurrentOS        as Path
import Filesystem.Path (FilePath)
import Data.Semigroup ((<>))

filePathOption :: Mod OptionFields FilePath -> Parser FilePath
filePathOption = completeFileOption "file"

directoryOption :: Mod OptionFields FilePath -> Parser FilePath
directoryOption = completeFileOption "directory"

-- | Make an option modifier which as a bash completion action.
-- Actions are listed in:
-- https://www.gnu.org/software/bash/manual/html_node/Programmable-Completion-Builtins.html#Programmable-Completion-Builtins
completeFileOption :: String -> Mod OptionFields FilePath -> Parser FilePath
completeFileOption act mods = option (Path.decodeString <$> str) (mods <> comp)
  where comp = completer (bashCompleter act)
