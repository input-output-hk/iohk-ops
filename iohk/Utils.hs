{-# LANGUAGE DeriveGeneric, FlexibleContexts, GADTs, LambdaCase, OverloadedStrings, RecordWildCards, StandaloneDeriving, TupleSections, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -Wno-name-shadowing -Wno-missing-signatures -Wno-type-defaults #-}

module Utils where

import qualified Data.Aeson                    as AE
import qualified Data.Aeson.Types              as AE
import qualified Data.Char                     as C
import qualified Data.Text                     as T
import           Data.Text                        (Text)
import           GHC.Generics              hiding (from, to)
import           Turtle


-- * Flags & enumerables
--
every :: (Bounded a, Enum a) => [a]
every = enumFromTo minBound maxBound


-- | Sum to track assurance
data Confirmation = Confirm | Ask Text
  deriving (Eq, Read, Show)

confirmOrTerminate :: Confirmation -> IO ()
confirmOrTerminate  Confirm       = pure ()
confirmOrTerminate (Ask question) = do
  echo $ unsafeTextToLine question <> "  Enter 'yes' to proceed:"
  reply <- readline
  unless (reply == Just "yes") $ do
    echo "User declined to proceed, exiting."
    exit $ ExitFailure 1


-- * Utils
showT :: Show a => a -> Text
showT = T.pack . show

readT :: Read a => Text -> a
readT = read . T.unpack

lowerShowT :: Show a => a -> Text
lowerShowT = T.toLower . T.pack . show

errorT :: Text -> a
errorT = error . T.unpack

-- Currently unused, but that's mere episode of the used/unused/used/unused event train.
-- Let's keep it, because it's too painful to reinvent every time we need it.
jsonLowerStrip :: (Generic a, AE.GToJSON AE.Zero (Rep a)) => Int -> a -> AE.Value
jsonLowerStrip n = AE.genericToJSON $ AE.defaultOptions { AE.fieldLabelModifier = map C.toLower . drop n }
