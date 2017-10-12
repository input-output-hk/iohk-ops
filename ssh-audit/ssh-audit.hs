{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Crypto.Hash as Hash
import Crypto.Hash.Algorithms
import Data.ByteArray
import Data.ByteString.Base64
import Data.ByteString hiding (putStrLn)
import Data.Monoid
import qualified Data.ByteString.Char8 as BSC
import System.Environment
import Data.Text
import Data.Text.Encoding
import Data.Maybe
import Systemd.Journal
import Pipes.Core
import Pipes.Safe
import Pipes
import qualified Data.HashMap.Strict as HashMap
import Safe
import Control.Monad
import qualified Data.Text.IO as Text

data PubKey = PubKey { attrName :: Text, keyType :: Text, pubKeyHash :: Text, user :: Text } deriving Show
--data PubKeys = PubKeys (HashMap Text PubKey)

hashSHA256 :: ByteString -> ByteString
hashSHA256 bs = convert (Hash.hash bs :: Hash.Digest Crypto.Hash.Algorithms.SHA256)

hashPubKey :: ByteString -> Either String ByteString
hashPubKey pubkey = encode . hashSHA256 <$> decode pubkey

parseLine :: ByteString -> Either String PubKey
parseLine line = do
  case BSC.words line of
    (attrName':keyType':pubkey:rawUser) -> do
      let
        user' = BSC.unwords rawUser
        stripEquals str = Data.Maybe.fromMaybe str (Data.Text.stripSuffix "=" str)
      case hashPubKey pubkey of
        Left errorMsg -> Left $ "base64 decode error on key " <> (show attrName') <> ": " <> errorMsg
        Right hash -> Right $ PubKey (decodeUtf8 attrName') (decodeUtf8 keyType') (stripEquals $ decodeUtf8 hash) (decodeUtf8 user')
    other -> do
      Left $ "unable to parse " <> (show other)

pipe :: (MonadSafe m) => Producer' JournalEntry m ()
pipe = openJournal [ SystemOnly ] (FromEnd Forwards) (Just (Match (mkJournalField "_SYSTEMD_UNIT") "sshd.service")) Nothing

myeffect :: (MonadIO m) => HashMap.HashMap Text PubKey -> JournalEntry -> Effect (SafeT m) ()
myeffect keyMap je = do
  let
    fields = journalEntryFields je
    msg = HashMap.lookup (mkJournalField "MESSAGE") fields
  case msg of
    Just msg' -> do
      let
        reportUser hash = do
          case HashMap.lookup hash keyMap of
            Just pubkey -> Text.putStrLn $ "ssh-audit: attribute: " <> (attrName pubkey) <> " user: " <> (user pubkey)
            Nothing -> putStrLn $ "ssh-audit: ALERT, unknown ssh keypair"
        maybeHash :: Maybe Text
        maybeHash = do
          case lastMay $ Data.Text.words $ decodeUtf8 msg' of
            Just lastword -> do
              ["SHA256", hash ] <- pure (Data.Text.splitOn ":" lastword)
              Just hash
            Nothing -> Nothing
      maybe (pure ()) (liftIO . reportUser) maybeHash
    Nothing -> return ()

main :: IO ()
main = do
  inputFile <- getEnv "keys"
  rawKeys <- BSC.readFile inputFile
  keyMap <- HashMap.fromList <$> forM (BSC.lines rawKeys) (\line -> do
    value <- either fail pure $ parseLine line
    pure (pubKeyHash value, value))
  (runSafeT (runEffect (Pipes.for pipe (myeffect keyMap))))
