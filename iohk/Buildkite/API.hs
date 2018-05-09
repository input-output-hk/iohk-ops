{-# LANGUAGE DeriveGeneric, RecordWildCards, OverloadedStrings #-}

-- | Module for accessing selected endpoints of Buildkite API.

-- fixme: need to have better error handling and return Eithers from endpoint functions.

module Buildkite.API
  ( listArtifactsForBuild
  , listArtifactsForJob
  , getArtifact
  , getArtifact'
  , getArtifactURL
  , getBuild
  , getBuildsForPipeline
  , getLatestBuildForPipeline
  , getJobLog
  , ID(..)
  , Artifact(..)
  , Build(..)
  , Job(..)
  , APIToken(..)
  , Paging(..)
  ) where

import           Network.HTTP.Simple
import           Network.HTTP.Conduit (redirectCount)
import           Data.Text (Text)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as S8
import           GHC.Generics
import           Data.Aeson
import           Data.Aeson.Types (Options(..), camelTo2)
import           Network.URI (URI(..), parseURI)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Monoid
import           Data.String
import           Crypto.Hash
import           Control.Error
import qualified Data.ByteArray.Encoding as B (Base(..), convertToBase)

----------------------------------------------------------------------------
-- types

-- | Buildkite usually uses UUIDs to identify records.
newtype ID = ID { unID :: Text }
  deriving (Generic, Show, Eq)

-- | These tokens are associated with an access scope and can be
-- generated at https://buildkite.com/user/api-access-tokens
newtype APIToken = APIToken Text

instance IsString APIToken where
  fromString = APIToken . T.pack

instance IsString ID where
  fromString = ID . T.pack

-- | Build artifact record from Buildkite, i.e. a file.
-- The spelling is theirs not mine.
data Artifact = Artifact
  { artifactId           :: ID
  , artifactJobId        :: ID
  , artifactURL          :: URI
  , artifactDownloadURL  :: URI
  , artifactState        :: Text
  , artifactPath         :: Text
  , artifactDirname      :: Text
  , artifactFilename     :: Text
  , artifactMimeType     :: Text
  , artifactFileSize     :: Int
  , artifactGlobPath     :: Text
  , artifactOriginalPath :: Text
  , artifactSha1sum      :: Text
  } deriving (Generic, Show, Eq)

data Build = Build
  { buildId          :: ID
  , buildUrl         :: URI  -- "https://api.buildkite.com/v2/organizations/my-great-org/pipelines/my-pipeline/builds/1",
  , buildWebUrl      :: URI  -- "https://buildkite.com/my-great-org/my-pipeline/builds/1",
  , buildNumber      :: Int   -- 1,
  , buildState       :: Text  -- "passed",
  , buildBlocked     :: Bool  -- false,
  , buildMessage     :: Text  -- "Bumping to version 0.2-beta.6",
  , buildCommit      :: Text  -- "abcd0b72a1e580e90712cdd9eb26d3fb41cd09c8",
  , buildBranch      :: Text  -- "master",
  , buildEnv         :: Value -- { },
  , buildSource      :: Text  -- "webhook",
  , buildJobs        :: [Job]
  , buildCreatedAt   :: Text  -- "2015-05-09T21:05:59.874Z",
  , buildScheduledAt :: Maybe Text  -- "2015-05-09T21:05:59.874Z",
  , buildStartedAt   :: Maybe Text  -- "2015-05-09T21:05:59.874Z",
  , buildFinishedAt  :: Maybe Text  -- "2015-05-09T21:05:59.874Z",
  , buildMetaData    :: Value -- { },
  -- no need to deserialize these structure, so disable for now
  -- , buildCreator     :: User
  -- , buildPipeline    :: Pipeline
  } deriving (Generic, Show, Eq)

data User = User
  { userId        :: ID
  , userName      :: Text -- "Keith Pitt",
  , userEmail     :: Text --  "keith@buildkite.com",
  , userAvatarUrl :: Text -- "https://www.gravatar.com/avatar/e14f55d3f939977cecbf51b64ff6f861",
  , userCreatedAt :: Text -- "2015-05-22T12:36:45.309Z"
  } deriving (Generic, Show, Eq)

data Pipeline = Pipeline
  { pipelineId                              :: ID
  , pipelineUrl                             :: Text       -- "https://api.buildkite.com/v2/organizations/my-great-org/pipelines/my-pipeline",
  , pipelineWebUrl                          :: Text       -- "https://buildkite.com/my-great-org/my-pipeline",
  , pipelineName                            :: Text       -- "great-pipeline",
  , pipelineSlug                            :: Text       -- "great-pipeline",
  , pipelineRepository                      :: Text       -- "git@github.com:my-great-org/my-pipeline",
  , pipelineProvider                        :: Buildprovider
  , pipelineSkipQueuedBranchBuilds          :: Bool       -- false,
  , pipelineSkipQueuedBranchBuildsFilter    :: Maybe Text -- null,
  , pipelineCancelRunningBranchBuilds       :: Bool       -- false,
  , pipelineCancelRunningBranchBuildsFilter :: Maybe Text -- null,
  , pipelineBuildsUrl                       :: Text       -- "https://api.buildkite.com/v2/organizations/my-great-org/pipelines/my-pipeline/builds",
  , pipelineBadgeUrl                        :: Text       -- "https://badge.buildkite.com/58b3da999635d0ad2daae5f784e56d264343eb02526f129bfb.svg",
  , pipelineCreatedAt                       :: Text       -- "2015-05-09T21:05:59.874Z",
  , pipelineScheduledBuildsCount            :: Int        -- 0,
  , pipelineRunningBuildsCount              :: Int        -- 0,
  , pipelineScheduledJobsCount              :: Int        -- 0,
  , pipelineRunningJobsCount                :: Int        -- 0,
  , pipelineWaitingJobsCount                :: Int        -- 0
  } deriving (Generic, Show, Eq)

data Buildprovider = Buildprovider
  { buildproviderId         :: Text -- "github",
  , buildproviderWebhookUrl :: Text
  } deriving (Generic, Show, Eq)

data Job = Job
  { jobId              :: ID          -- "b63254c0-3271-4a98-8270-7cfbd6c2f14e",
  , jobType            :: Text        -- "script",
  , jobName            :: Text        -- "scripts/build.sh",
  , jobAgentQueryRules :: [Text]      -- ["*"],
  , jobState           :: Text        -- "passed",
  , jobWebUrl          :: Text        -- "https://buildkite.com/my-great-org/my-pipeline/builds/1#b63254c0-3271-4a98-8270-7cfbd6c2f14e",
  , jobLogUrl          :: Text        -- "https://api.buildkite.com/v2/organizations/my-great-org/pipelines/my-pipeline/builds/1/jobs/b63254c0-3271-4a98-8270-7cfbd6c2f14e/log",
  , jobRawLogUrl       :: Text        -- "https://api.buildkite.com/v2/organizations/my-great-org/pipelines/my-pipeline/builds/1/jobs/b63254c0-3271-4a98-8270-7cfbd6c2f14e/log.txt",
  , jobCommand         :: Text        -- "scripts/build.sh",
  , jobExitStatus      :: Maybe Int   -- 0,
  , jobCreatedAt       :: Text        -- "2015-05-09T21:05:59.874Z",
  , jobScheduledAt     :: Maybe Text  -- "2015-05-09T21:05:59.874Z",
  , jobStartedAt       :: Maybe Text  -- "2015-05-09T21:05:59.874Z",
  , jobFinishedAt      :: Maybe Text  -- "2015-05-09T21:05:59.874Z"
  , jobArtifactPaths   :: Maybe Text  -- "",
  -- no need to deserialize this structure, so disable for now
  -- , jobAgent           :: Agent
  } deriving (Generic, Show, Eq)

data Agent = Agent
  { agentId   :: ID
  , agentUrl  :: Text
  , agentName :: Text
  } deriving (Generic, Show, Eq)

data Paging = Paging { pageNum :: Int
                     , pageSize :: Int
                     } deriving (Show, Eq)

data ArtifactDownload = ArtifactDownload { unArtifactDownload :: Text }

defaultPaging :: Paging
defaultPaging = Paging 1 30

----------------------------------------------------------------------------
-- api calls

-- | Returns list of artifact records for all jobs of the given build.
-- Maximum 30 artifacts. (?page=1&per_page=30)
-- https://buildkite.com/docs/rest-api/artifacts#list-artifacts-for-a-build
listArtifactsForBuild :: APIToken      -- ^ token generated in buildkite settings
                      -> Text          -- ^ organization slug, e.g. input-output-hk
                      -> Text          -- ^ pipeline slug, e.g. iohk-ops
                      -> Int           -- ^ build number
                      -> IO [Artifact] -- ^ artifact data records belonging to the job
listArtifactsForBuild t org pipeline buildNum = getResponseBody <$> httpJSON req
  where
    req = makeRequest t path
    path = buildPath org pipeline buildNum ++ [ "artifacts" ]

-- https://buildkite.com/docs/rest-api/artifacts#list-artifacts-for-a-job
listArtifactsForJob :: APIToken      -- ^ token generated in buildkite settings
                    -> Text          -- ^ organization slug, e.g. input-output-hk
                    -> Text          -- ^ pipeline slug, e.g. iohk-ops
                    -> Int           -- ^ build number
                    -> ID            -- ^ job id
                    -> IO [Artifact] -- ^ artifact data records belonging to the job
listArtifactsForJob t org pipeline buildNum jobId = getResponseBody <$> httpJSON req
  where
    req = makeRequest t path
    path = buildPath org pipeline buildNum ++ [ "jobs", unID jobId, "artifacts" ]

-- | Downloads an artifact file and checks its hash.
-- The entire file is read into memory.
-- https://buildkite.com/docs/rest-api/artifacts#download-an-artifact
getArtifact :: APIToken       -- ^ token generated in buildkite settings
            -> Text          -- ^ organization slug, e.g. input-output-hk
            -> Text          -- ^ pipeline slug, e.g. iohk-ops
            -> Int           -- ^ build number
            -> Artifact      -- ^ Artifact record
            -> IO ByteString -- ^ Contents of the artifact file
getArtifact t org pipeline buildNum art@Artifact{..} = do
  file <- L8.toStrict <$> getArtifact' t org pipeline buildNum art
  case checkHash artifactSha1sum file of
    Just hash -> error $ mconcat [ "Bad hash: expected "
                                 , T.unpack artifactSha1sum
                                 , ", got "
                                 , T.unpack hash ]
    Nothing -> return file

-- | Downloads an artifact file, streamed as a lazy bytestring.
-- https://buildkite.com/docs/rest-api/artifacts#download-an-artifact
getArtifact' :: APIToken
             -> Text
             -> Text
             -> Int
             -> Artifact
             -> IO L8.ByteString
getArtifact' t org pipeline buildNum art = do
  url <- getArtifactURL t org pipeline buildNum art
  req <- parseRequest (T.unpack url)
  getResponseBody <$> httpLBS req

-- | Returns the URL for downloading an artifact.
-- Although the returned URL might be in our own public S3 bucket,
-- this can't be assumed, and it may only be valid for 60 seconds.
-- https://buildkite.com/docs/rest-api/artifacts#download-an-artifact
getArtifactURL :: APIToken       -- ^ token generated in buildkite settings
               -> Text           -- ^ organization slug, e.g. input-output-hk
               -> Text           -- ^ pipeline slug, e.g. iohk-ops
               -> Int            -- ^ build number
               -> Artifact       -- ^ Artifact record
               -> IO Text        -- ^ Temporary URL to download artifact from
getArtifactURL t org pipeline buildNum Artifact{..} =
  unArtifactDownload . getResponseBody <$> httpJSON req
  where
    req = makeRequest t path
    path = buildPath org pipeline buildNum ++
           [ "jobs", unID artifactJobId, "artifacts", unID artifactId, "download" ]

-- | Gets a build using the pipeline build number.
-- https://buildkite.com/docs/rest-api/builds#get-a-build
getBuild :: APIToken         -- ^ token generated in buildkite settings
         -> Text             -- ^ organization slug, e.g. input-output-hk
         -> Text             -- ^ pipeline slug, e.g. iohk-ops
         -> Int              -- ^ Build number
         -> IO Build         -- ^ Info about the build
getBuild t org pipeline num = getResponseBody <$> httpJSON req
  where
    req = makeRequest t (buildPath org pipeline num)

-- | Lists builds for a pipeline.
-- https://buildkite.com/docs/rest-api/builds#list-builds-for-a-pipeline
getBuildsForPipeline :: APIToken        -- ^ token generated in buildkite settings
                     -> Text            -- ^ organization slug, e.g. input-output-hk
                     -> Text            -- ^ pipeline slug, e.g. iohk-ops
                     -> [(Text, Text)]  -- ^ Search query
                     -> Paging          -- ^ Which page to fetch
                     -> IO [Build]      -- ^ Page full of builds
getBuildsForPipeline t org pipeline search page = getResponseBody <$> httpJSON req
  where
    req = addSearchParams search $ setPaging page $ makeRequest t path
    path = [ "organizations", org, "pipelines", pipeline, "builds" ]

addSearchParams :: [(Text, Text)] -> Request -> Request
addSearchParams ps req = setRequestQueryString q' req
  where
    q' = getRequestQueryString req ++ q
    q = [(T.encodeUtf8 f, Just (T.encodeUtf8 v)) | (f, v) <- ps]

-- | Gets the latest successful build for a pipeline. Works under the
-- assumption that the API returns builds in date-descending order.
getLatestBuildForPipeline :: APIToken      -- ^ token generated in buildkite settings
                          -> Text          -- ^ organization slug, e.g. input-output-hk
                          -> Text          -- ^ pipeline slug, e.g. iohk-ops
                          -> IO (Maybe Build)
getLatestBuildForPipeline t org pipeline =
  headMay <$> getBuildsForPipeline t org pipeline q (Paging 1 1)
  where q = [("state", "passed")]

-- | Gets the full text of the job's build log.
getJobLog :: APIToken -> Job -> IO L8.ByteString
getJobLog t j = do
  req <- parseRequest . T.unpack . jobRawLogUrl $ j
  getResponseBody <$> httpLBS (addUserAgentHeader . addAuthHeader t $ req)

----------------------------------------------------------------------------
-- util

makeRequest :: APIToken -> [Text] -> Request
makeRequest token path = setRequestPath ("/v2/" <> T.encodeUtf8 (T.intercalate "/" path))
                         $ setRequestHost "api.buildkite.com"
                         $ setRequestSecure True
                         $ setRequestPort 443
                         $ addAuthHeader token
                         $ setRedirectCount 0
                         $ addUserAgentHeader
                         $ defaultRequest

buildPath :: Text -> Text -> Int -> [Text]
buildPath org pipeline num = [ "organizations", org, "pipelines", pipeline
                             , "builds" , T.pack (show num) ]

addUserAgentHeader :: Request -> Request
addUserAgentHeader = setRequestHeader "User-Agent" ["https://github.com/input-output-hk/iohk-ops"]

addAuthHeader :: APIToken -> Request -> Request
addAuthHeader (APIToken token) = setRequestHeader "Authorization"
  ["Bearer " <> T.encodeUtf8 token]

setRedirectCount :: Int -> Request -> Request
setRedirectCount n req = req { redirectCount = n }

setPaging :: Paging -> Request -> Request
setPaging (Paging num count) = setRequestQueryString [q "page" num, q "per_page" count]
  where
    q field val = (field, Just . S8.pack . show $ val)

-- | Compare hashes, return something if they are unequal
checkHash :: Text -> ByteString -> Maybe Text
checkHash h b | calc == T.encodeUtf8 h = Nothing
              | otherwise = Just (T.decodeUtf8 calc)
  where
    calc = b16 (hash b :: Digest SHA1)
    b16 = B.convertToBase B.Base16

-- | Decode UTF-8 response into strict text
getResponseText :: Response L8.ByteString -> Text
getResponseText = T.decodeUtf8 . L8.toStrict . getResponseBody

----------------------------------------------------------------------------
-- deserialization from api

instance FromJSON Artifact where
  parseJSON = genericParseJSON (parseOptions "artifact")

instance FromJSON Build where
  parseJSON = genericParseJSON (parseOptions "build")

instance FromJSON Job where
  parseJSON = genericParseJSON (parseOptions "job")

instance FromJSON Agent where
  parseJSON = genericParseJSON (parseOptions "agent")

instance FromJSON User where
  parseJSON = genericParseJSON (parseOptions "user")

instance FromJSON Pipeline where
  parseJSON = genericParseJSON (parseOptions "pipeline")

instance FromJSON Buildprovider where
  parseJSON = genericParseJSON (parseOptions "buildprovider")

instance FromJSON ID where
  parseJSON v = ID <$> parseJSON v

instance FromJSON ArtifactDownload where
  parseJSON = withObject "ArtifactDownload" $ \o ->
    ArtifactDownload <$> o .: "url"

instance FromJSON URI where
  parseJSON = withText "URI" $ \u -> case parseURI (T.unpack u) of
    Just uri -> return uri
    Nothing -> fail "URI could not be parsed"

parseOptions :: String -> Options
parseOptions prefix = defaultOptions { fieldLabelModifier = dropPrefix . camelTo2 '_' }
  where dropPrefix = drop (length prefix + 1)
