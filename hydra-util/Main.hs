{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}

module Main (main) where

import qualified Database.PostgreSQL.Simple as DB
import           Control.Monad    (foldM, forM_)
import qualified Data.Set as Set
import qualified Turtle.Shell as Turtle
import qualified Turtle as Turtle
import           Database.PostgreSQL.Simple.FromRow (fromRow, field)
import           Diagrams.Backend.SVG               (SVG(SVG), Options(SVGOptions))
import           Diagrams.Prelude                   (Diagram, Point, V2, square, (#), fc, red, green, scaleY, scaleX, none, lw, fontSizeL, text, rotate, (@@), deg, Point(P), atop, hrule, lc, position, (<>), r2, renderDia, mkSizeSpec2D, SizeSpec, QDiagram, Any, Renderable, Path)
import           Formatting                         (format, (%), shown)
import qualified Formatting as Formatting
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import           Servant                            (MimeRender(mimeRender), Accept(contentType), (:>), Capture, Get, Server, Handler, Proxy(Proxy), Application, serve, (:<|>)( (:<|>) ), JSON)
import qualified Graphics.Svg as SVG
import           Network.HTTP.Media                 ((//), (/:))
import           Network.Wai.Handler.Warp           (run)
import           Control.Monad.IO.Class             (liftIO)
import           Data.Text                          (Text)
import           Data.Aeson                         (ToJSON(toJSON), genericToJSON, defaultOptions)
import           GHC.Generics                       (Generic)
import qualified Diagrams.TwoD.Text                 as DText

type DrvPath = Text
newtype EvalID = EvalID { fromEvalID :: Int }

type AllDrvs = Set.Set DrvPath

data Times = Times
    { _buildnr :: Int
    , _stepnr :: Int
    , _starttime :: Int
    , _stoptime :: Int
    , _machine :: String
    , _system :: String
    , _drvpath :: DrvPath
    } deriving (Show, Generic)

instance ToJSON Times where
  toJSON = genericToJSON defaultOptions

instance DB.FromRow Times where
  fromRow = Times <$> field <*> field <*> field <*> field <*> field <*> field <*> field


getDrvClosureFromEval :: DB.Connection -> EvalID -> IO AllDrvs
getDrvClosureFromEval conn evalid = do
  let
    sql :: DB.Query
    sql = "select drvpath from builds where id in (select build from jobsetevalmembers where eval = ?)"
  initialDrvs <- DB.fold conn sql (DB.Only $ fromEvalID evalid) mempty (\input (DB.Only drvpath) -> pure $ Set.insert drvpath input)
  let
    nixStoreQR :: Text -> Turtle.Shell Turtle.Line
    nixStoreQR drvpath' = Turtle.inproc "nix-store" ["-qR", drvpath'] mempty
  let
    step :: AllDrvs -> Turtle.Line -> AllDrvs
    step input line = Set.insert (Turtle.lineToText line) input
    folder :: AllDrvs -> DrvPath -> IO AllDrvs
    folder input drvpath' = do
      Turtle.fold (nixStoreQR drvpath') (Turtle.Fold step input id)
  foldM folder initialDrvs initialDrvs

getEvalTime :: DB.Connection -> EvalID -> IO Int
getEvalTime conn evalid = do
  [DB.Only x] <- DB.query conn "SELECT timestamp FROM jobsetevals WHERE id = ?" (DB.Only $ fromEvalID evalid)
  return x

main :: IO ()
main = httpServer

getStepTimes :: DB.Connection -> AllDrvs -> Int -> Int -> IO [ Times ]
getStepTimes conn drvs cutoff mintime = do
  DB.query conn "SELECT build,stepnr,starttime,stoptime,machine,system,drvpath FROM buildsteps WHERE drvpath IN ? AND status = 0 AND system IS NOT NULL AND starttime > ? AND (stoptime - starttime) > ? ORDER BY starttime" ((DB.In (Set.toList drvs)), cutoff, mintime)


type UtilAPI = "graph" :> Capture "evalid" Int :> Get '[ServantSVG] SVG.Element
          :<|> "graph" :> Capture "evalid" Int :> "rawjson" :> Get '[JSON] ([Times], Int)

instance MimeRender ServantSVG SVG.Element where
  mimeRender _ val = LT.encodeUtf8 $ SVG.prettyText val

data ServantSVG

instance Accept ServantSVG where
  contentType _ = "image" // "svg+xml" /: ("charset", "utf-8")

server1 :: DB.Connection -> Server UtilAPI
server1 conn = (renderGraph conn) :<|> (getRawTimes conn)

getRawTimes :: DB.Connection -> Int -> Handler ([Times], Int)
getRawTimes conn evalid' = do
  let
    -- TODO, make a FromHttpApiData instance
    evalid = EvalID evalid'
  liftIO $ getDrvTimes conn evalid

renderGraph :: DB.Connection -> Int -> Handler SVG.Element
renderGraph conn evalid' = do
  let
    -- TODO, make a FromHttpApiData instance
    evalid = EvalID evalid'
  (times, evalTime) <- liftIO $ getDrvTimes conn evalid
  liftIO $ forM_ times (\x -> print x)
  let
    diagram :: Diagram SVG
    diagram = drvTimesToDiagram times evalTime
    spec :: SizeSpec V2 Double
    spec = mkSizeSpec2D (Just 1000) (Just 1000)
    svgelement = renderDia SVG (SVGOptions spec Nothing "prefix" [] True) diagram
  pure svgelement

getDrvTimes :: DB.Connection -> EvalID -> IO ([Times], Int)
getDrvTimes conn evalid = do
  drvs <- getDrvClosureFromEval conn evalid
  evalTime <- getEvalTime conn evalid
  times <- getStepTimes conn drvs (evalTime - 600) 5
  pure (times, evalTime)

drvTimesToDiagram :: Renderable (Path V2 Double) b => Renderable (DText.Text Double) b => [ Times ] -> Int -> QDiagram b V2 Double Any
drvTimesToDiagram times evalTime = do
  let
    labeledStepBox :: Renderable (Path V2 Double) b => Renderable (DText.Text Double) b => Times -> QDiagram b V2 Double Any
    labeledStepBox time = do
      let
        stepBox :: Renderable (Path V2 Double) b => QDiagram b V2 Double Any
        stepBox = square 1 # fc red # scaleY ( fromIntegral ((_stoptime time) - (_starttime time)) ) # scaleX 10 # lw none
        drvpathText :: LT.Text
        drvpathText = LT.fromStrict $ _drvpath time
        (_, withouthash) = LT.splitAt 44 drvpathText
        msg = format (Formatting.text%" "%shown) withouthash ((_stoptime time) - (_starttime time))
        label :: Renderable (DText.Text Double) b => QDiagram b V2 Double Any
        label = text (LT.unpack msg) # fontSizeL 10 # fc green # rotate (-90 @@ deg)
      label `atop` stepBox
    mkPoint :: Times -> Point V2 Double
    mkPoint time = P (r2 (fromIntegral $ _stepnr time * 10, fromIntegral $ _starttime time))
    listOfSteps :: Renderable (Path V2 Double) b => Renderable (DText.Text Double) b => [ (Point V2 Double, QDiagram b V2 Double Any) ]
    listOfSteps = (zip (map mkPoint times) (map labeledStepBox times))
    line :: Renderable (Path V2 Double) b => Int -> (Point V2 Double, QDiagram b V2 Double Any)
    line pos = (P (r2 (0, fromIntegral pos)), hrule 200 # lc green)
    everything :: Renderable (Path V2 Double) b => Renderable (DText.Text Double) b => QDiagram b V2 Double Any
    everything = position (listOfSteps <> [line evalTime] <> timeGrid)
    timelist = (map _starttime times) <> (map _stoptime times) <> [ evalTime ]
    lowest = minimum timelist
    highest = maximum timelist
    makeInterval :: Int -> Int -> [ Int ]
    makeInterval start' end' = [ start', start'+60 .. end' ]
    timeGrid :: Renderable (Path V2 Double) b => [ (Point V2 Double, QDiagram b V2 Double Any) ]
    timeGrid = zip (map mkTimePoint (makeInterval (lowest+60) highest)) (repeat timeLine)
    timeLine :: Renderable (Path V2 Double) b => QDiagram b V2 Double Any
    timeLine = hrule 200
    mkTimePoint :: Int -> Point V2 Double
    mkTimePoint timeoffset = P (r2 (0, fromIntegral timeoffset))
  everything

utilApi :: Proxy UtilAPI
utilApi = Proxy

app1 :: DB.Connection -> Application
app1 conn = serve utilApi (server1 conn)

httpServer :: IO ()
httpServer = do
  conn <- DB.connectPostgreSQL "dbname='hydra'"
  run 8081 (app1 conn)
