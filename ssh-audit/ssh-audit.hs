{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

module Main where

import           Control.Monad
import           Data.Maybe
import           Data.Monoid

import           Control.Monad.IO.Class              (MonadIO (liftIO))

import           System.Environment                  (getEnv)

import qualified "cryptonite" Crypto.Hash            as Hash
import qualified "cryptonite" Crypto.Hash.Algorithms as Hash
import qualified Data.ByteArray                      as ByteArray

import           Data.ByteString                     (ByteString)
import qualified Data.ByteString                     as BS
import qualified Data.ByteString.Char8               as BSC

import qualified Data.ByteString.Base64              as Base64

import           Data.HashMap.Strict                 (HashMap)
import qualified Data.HashMap.Strict                 as HM

import           Data.Text                           (Text)
import qualified Data.Text                           as Text
import qualified Data.Text.Encoding                  as Text
import qualified Data.Text.IO                        as Text

import qualified Pipes                               as Pipes
import qualified Pipes.Core                          as Pipes
import qualified Pipes.Safe                          as Pipes

import qualified Safe

import qualified Systemd.Journal                     as Journal

data PubKey
  = PubKey
    { pubKeyAttrName   :: !Text
    , pubKeyKeyType    :: !Text
    , pubKeySubKeyHash :: !Text
    , pubKeyUser       :: !Text
    }
  deriving (Eq, Show)

hashSHA256 :: ByteString -> ByteString
hashSHA256 bs = ByteArray.convert (Hash.hash bs :: Hash.Digest Hash.SHA256)

hashPubKey :: ByteString -> Either String ByteString
hashPubKey pubkey = Base64.encode . hashSHA256 <$> Base64.decode pubkey

parseLine :: ByteString -> Either String PubKey
parseLine line = do
  (attrName, keyType, pubkey, rawUser) <- do
    case BSC.words line of
      (a:k:p:ru) -> pure (a, k, p, ru)
      other      -> Left $ "unable to parse " <> show other
  let user = BSC.unwords rawUser
  let stripEquals str = fromMaybe str (Text.stripSuffix "=" str)
  let decodeError e = mconcat
                      ["base64 decode error on key ", show attrName, ": ", e]
  hash <- either (Left . decodeError) pure (hashPubKey pubkey)
  pure $ PubKey { pubKeyAttrName   = Text.decodeUtf8 attrName
                , pubKeyKeyType    = Text.decodeUtf8 keyType
                , pubKeySubKeyHash = stripEquals (Text.decodeUtf8 hash)
                , pubKeyUser       = Text.decodeUtf8 user
                }

pipe :: (Pipes.MonadSafe m) => Pipes.Producer' Journal.JournalEntry m ()
pipe = Journal.openJournal
       [Journal.SystemOnly]
       (Journal.FromEnd Journal.Forwards)
       (Just (Journal.Match systemdUnitField "sshd.service"))
       Nothing
  where
    systemdUnitField = Journal.mkJournalField "_SYSTEMD_UNIT"

myeffect :: (MonadIO m)
         => HashMap Text PubKey
         -> Journal.JournalEntry
         -> Pipes.Effect (Pipes.SafeT m) ()
myeffect keyMap je = do
  let messageField = Journal.mkJournalField "MESSAGE"
  case HM.lookup messageField (Journal.journalEntryFields je) of
    Just msg -> do
      maybe (pure ()) id $ do
        lastword <- Safe.lastMay $ Text.words $ Text.decodeUtf8 msg
        ["SHA256", hash] <- pure (Text.splitOn ":" lastword)
        pure $ liftIO $ do
          let msg = case HM.lookup hash keyMap of
                      Just pubkey -> mconcat
                                     [ "attribute: ", pubKeyAttrName pubkey, " "
                                     , "user: ",      pubKeyUser pubkey ]
                      Nothing     -> "ALERT, unknown ssh keypair"
          Text.putStrLn ("ssh-audit: " <> msg)
    Nothing -> pure ()

main :: IO ()
main = do
  inputFile <- getEnv "keys"
  rawKeys <- BSC.readFile inputFile
  keyMap <- HM.fromList <$> forM (BSC.lines rawKeys) (\line -> do
    value <- either fail pure $ parseLine line
    pure (pubKeySubKeyHash value, value))
  Pipes.runSafeT (Pipes.runEffect (Pipes.for pipe (myeffect keyMap)))
