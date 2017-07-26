#!/usr/bin/env runhaskell

{-# LANGUAGE DataKinds, GADTs, OverloadedStrings, StandaloneDeriving, UnicodeSyntax #-}

import Control.Monad (forM_)
import Data.Maybe
import Data.Monoid ((<>))
import Filesystem.Path.CurrentOS (encodeString)
import Prelude hiding (FilePath)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Turtle


-- * URLs
data URL =
  URL
  { proto ∷ T.Text
  , host  ∷ T.Text
  , key   ∷ T.Text
  } deriving (Show)

ppURL∷ URL → T.Text
ppURL (URL pro ho ke) = pro <> "://" <> ho <> "/" <> ke


-- * Buckets
data Bucket     = Bucket { fromBucket ∷ T.Text } deriving (Show)

default'bucket = Bucket "daedalus-travis"

daedalus'versions'key :: Text
daedalus'versions'key = "daedalus-latest-version.json"


-- * Build/release logic
--
data OS = Linux | OSX | Win64

data RSpec a where
  R_Linux :: BuildId               -> RSpec Linux
  R_OSX   :: BuildId               -> RSpec OSX
  R_Win64 :: BuildId -> AppveyorId -> RSpec Win64
deriving instance Show (RSpec a)

data AppveyorId = AppveyorId                  T.Text   deriving (Show)
data BuildId    = BuildId    { fromBuildId :: T.Text } deriving (Show)

buildURL ∷ RSpec a → URL
buildURL (R_Linux (BuildId bid)) =
  URL "https" "s3.eu-central-1.amazonaws.com" $
  error "Sorry, no Linux support just yet."
buildURL (R_OSX   (BuildId bid)) =
  URL "https" "s3.eu-central-1.amazonaws.com" $
  mconcat ["daedalus-travis/Daedalus-installer-", bid, ".pkg"]
buildURL (R_Win64 (BuildId bid) (AppveyorId avid)) =
  URL "https" "ci.appveyor.com" $
  mconcat ["api/buildjobs/", avid, "/artifacts/installers/daedalus-win64-", bid,"-installer.exe"]


-- * Parametrisation & Main
data Command =
  S3 S3Command Bucket
  deriving (Show)

data S3Command
  = CheckDaedalusReleaseURLs
  | SetDaedalusReleaseBuild Bool (RSpec OSX) (RSpec Win64)
  deriving (Show)

parserBuildId :: ArgName -> Parser BuildId
parserBuildId metavar =
  (BuildId    <$> argText metavar       "Daedalus build id.  Example: '0.3.1526'")

parserAppveyorId :: Parser AppveyorId
parserAppveyorId =
  (AppveyorId <$> argText "APPVEYOR-ID" "AppVeyor build subpath.  Example: 'iw5m0firsneia7k2'")

parser ∷ Parser Command
parser =
  subcommand "s3" "Control the S3-related AWS-ities."
  (S3 <$> (subcommand "set-daedalus-release-build" "Set the S3 daedalus-<OS>-latest.<EXT> redirect to a particular version."
            (SetDaedalusReleaseBuild
             <$> (fromMaybe False <$> (optional
                                       (switch "debug" 'd' "Dump internal state, and crucially -- DON'T reach AWS.")))
             <*> (R_OSX   <$> parserBuildId "OSX-BUILD-ID")
             <*> (R_Win64 <$> parserBuildId "WIN64-BUILD-ID" <*> parserAppveyorId))
           <|>
           subcommand "check-daedalus-release-urls" "Check the Daedalus release URLs." (pure CheckDaedalusReleaseURLs))
      <*> (fromMaybe default'bucket
           <$> optional (Bucket <$> (optText "bucket" 'b' (pure $ Turtle.HelpMessage $ "The S3 bucket to operate on.  Defaults to '" <> fromBucket default'bucket <> "'.")))))

main ∷ IO ()
main = do
  top ← options "Helper CLI around IOHK AWS functionality" parser
  case top of
    S3 cmd bucket → runS3 cmd bucket



osxLive, w64Live ∷ Bucket → URL
osxLive (Bucket b) = URL "http" (b <> ".s3-website.eu-central-1.amazonaws.com") "daedalus-osx-latest.pkg"
w64Live (Bucket b) = URL "http" (b <> ".s3-website.eu-central-1.amazonaws.com") "daedalus-win64-latest.exe"

checkURL ∷ Text → URL → IO ()
checkURL desc url = do
  echo ""
  printf ("============== Checking if "%s%" URL is live:") desc
  echo ""
  shells ("wget --tries=1 --spider " <> ppURL url) empty

-- Note Appveyor URL checking broken
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Sadly, neither 'curl' nor 'wget --spider' seem to be able to test AppVeyor URLs
-- for liveness.  Which sounds sort of amazing, indeed.
---
runS3 ∷ S3Command → Bucket → IO ()
runS3 CheckDaedalusReleaseURLs bucket = do
  checkURL "OSX release" (osxLive bucket)
  -- checkURL "W64 release" w64Live -- See Note Appveyor URL checking broken

  echo "URLs live:"
  echo ""
  printf ("  OS X:   "%s%"\n") (ppURL $ osxLive bucket)
  printf ("  Win64:  "%s%"\n") (ppURL $ w64Live bucket)

runS3 (SetDaedalusReleaseBuild debug'mode rsOSX@(R_OSX osx'bid) rsWin64@(R_Win64 w64'bid avId)) bucket = do
  let (   osxurl@(URL osxpr osxcn osxpath)
        , w64url@(URL w64pr w64cn w64path)) =
        (buildURL rsOSX, buildURL rsWin64)

  echo $ unsafeTextToLine $ mconcat [ "Setting Daedalus release redirects to: "]
  echo $ unsafeTextToLine $ mconcat [ "  OS X:   ", ppURL osxurl]
  echo $ unsafeTextToLine $ mconcat [ "  Win64:  ", ppURL w64url]
  checkURL "OSX build" osxurl
  -- checkURL "W64 build" w64url -- See Note Appveyor URL checking broken

  echo "Both URLs live, proceeding to update latest release build references to:"
  echo $ unsafeTextToLine $ mconcat [ "  OS X:  ", ppURL osxurl]
  echo $ unsafeTextToLine $ mconcat [ "  Win64: ", ppURL w64url]
  echo ""

  with   (mktempfile "/tmp" "awsbucketcfg.json")   $ \bucket'cfg → do
    with (mktempfile "/tmp" "awsversionfile.json") $ \version'file → do
      writeTextFile version'file $
        (    "{ "
          <> "\"linux\": \"" <> ""                  <> "\","
          <> "\"macos\": \"" <> fromBuildId osx'bid <> "\","
          <> "\"win64\": \"" <> fromBuildId w64'bid <> "\""
          <> " }" )
      writeTextFile bucket'cfg . T.unlines $
        [ "{",
          "    \"IndexDocument\": {",
          "        \"Suffix\":                   \"index.html\"",
          "    },",
          "    \"RoutingRules\": [",
          "        {",
          "            \"Condition\": {",
          "                \"KeyPrefixEquals\":  \"daedalus-osx-latest.pkg\"",
          "            },",
          "            \"Redirect\": {",
          "                \"HttpRedirectCode\": \"303\",",
          ["                \"Protocol\":         \"", osxpr,  "\","] & mconcat,
          ["                \"HostName\":         \"", osxcn,  "\","] & mconcat,
          ["                \"ReplaceKeyWith\":   \"", osxpath, "\""] & mconcat,
          "            }",
          "        },",
          "        {",
          "            \"Condition\": {",
          "                \"KeyPrefixEquals\":  \"daedalus-win64-latest.exe\"",
          "            },",
          "            \"Redirect\": {",
          "                \"HttpRedirectCode\": \"303\",",
          ["                \"Protocol\":         \"", w64pr,  "\","] & mconcat,
          ["                \"HostName\":         \"", w64cn,  "\","] & mconcat,
          ["                \"ReplaceKeyWith\":   \"", w64path, "\""] & mconcat,
          "            }",
          "        }",
          "    ]",
          "}"]

      if debug'mode
      then do
        shells ("cat " <> format fp bucket'cfg)   empty
        shells ("cat " <> format fp version'file) empty
        echo "" >> echo ""
        echo "URLs we were supposed to touch:"
      else do
        shells ("aws s3api put-bucket-website"
                <> " --bucket "                       <> fromBucket bucket
                <> " --website-configuration file://" <> format fp bucket'cfg)
          empty
        shells ("aws s3api put-object"
                <> " --bucket "                       <> fromBucket bucket
                <> " --key "                          <> daedalus'versions'key
                <> " --acl "                          <> "public-read"
                <> " --body "                         <> (format fp version'file))
          empty
        echo "Done.  Following URLs should be live within a minute:"
      echo ""
      printf ("  OS X:   "%s%"\n") (ppURL $ osxLive bucket)
      printf ("  Win64:  "%s%"\n") (ppURL $ w64Live bucket)

  pure ()
