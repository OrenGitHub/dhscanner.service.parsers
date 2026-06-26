{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Yesod
import Prelude
import GHC.Generics hiding (from, to)
import Data.Text
import Text.Read (readMaybe)
import Data.Time
import Yesod.Core.Types
import System.Log.FastLogger
import Network.Wai.Handler.Warp
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS8

-- project imports
import qualified Location
import qualified Ast
import qualified Common

-- project imports
import qualified JsParser
import qualified CsParser
import qualified GoParser
import qualified TsParser
import qualified PyParser
import qualified RbParser
import qualified PhpParser
import qualified YamlParser

data PathMappingInput
   = PathMappingInput
     {
         from :: String,
         to :: String
     }
     deriving ( Generic, ToJSON, FromJSON )

data SourceFileInput
   = SourceFileInput
     {
         filename :: String,
         content :: String,
         optional_github_url :: Maybe String,
         source_containing_dirs :: [ String ],
         all_filenames :: [ String ],
         path_mappings :: Maybe [ PathMappingInput ]
     }
     deriving ( Generic, ToJSON, FromJSON )

data Healthy = Healthy Bool deriving ( Generic )

data Error
   = Error
     {
         status :: String,
         location :: Location.Location
     }
     deriving ( Generic, ToJSON )

-- | This is just for the health check ...
instance ToJSON Healthy where toJSON (Healthy isHealthy) = object [ "healthy" .= isHealthy ]

data App = App

mkYesod "App" [parseRoutes|
/from/php/to/dhscanner/ast FromPhpR POST
/from/py/to/dhscanner/ast FromPyR POST
/from/rb/to/dhscanner/ast FromRbR POST
/from/js/to/dhscanner/ast FromJsR POST
/from/cs/to/dhscanner/ast FromCsR POST
/from/go/to/dhscanner/ast FromGoR POST
/from/ts/to/dhscanner/ast FromTsR POST
/from/yaml/to/dhscanner/ast FromYamlR POST
/healthcheck HealthcheckR GET
|]

instance Yesod App where
    makeLogger _app = myLogger
    maximumContentLength _app _anyRouteReally = Just 80000000
    messageLoggerSource _app = messageLoggerWithoutSource

getHealthcheckR :: Handler Value
getHealthcheckR = returnJson $ Healthy True

postFromPhpR :: Handler Value
postFromPhpR = post PhpParser.parseProgram

postFromPyR :: Handler Value
postFromPyR = post PyParser.parseProgram

postFromRbR :: Handler Value
postFromRbR = post RbParser.parseProgram

postFromJsR :: Handler Value
postFromJsR = post JsParser.parseProgram

postFromCsR :: Handler Value
postFromCsR = post CsParser.parseProgram

postFromGoR :: Handler Value
postFromGoR = post GoParser.parseProgram

postFromTsR :: Handler Value
postFromTsR = post TsParser.parseProgram

postFromYamlR :: Handler Value
postFromYamlR = post YamlParser.parseProgram

postFailed :: String -> String -> Handler Value
postFailed errorMsg _filename = do
    $logWarnS "(Parser)" (Data.Text.pack ("Failed: " ++ errorMsg))
    let defaultLoc = Location.Location _filename 1 1 1 1
    let loc = case (readMaybe errorMsg :: Maybe Location.Location) of { Just l -> l; _ -> defaultLoc }
    returnJson (Error "FAILED" loc)

postSucceeded :: Ast.Root -> Handler Value
postSucceeded = returnJson

type ProgramParser = Common.SourceCodeFilePath -> Common.SourceCodeContent -> Common.AdditionalRepoInfo -> Either String Ast.Root

post :: ProgramParser -> Handler Value
post parseProgram = do
    src <- requireCheckJsonBody :: Handler SourceFileInput
    $logInfoS "" (Data.Text.pack $ "Parsed: " ++ filename src)
    post' parseProgram src

post' :: ProgramParser -> SourceFileInput -> Handler Value
post' parseProgram src = case parseProgram (sourceCodeFilePath src) (sourceContent src) (additionalInfo src) of
    Left errorMsg -> postFailed errorMsg (filename src)
    Right ast -> postSucceeded ast

sourceCodeFilePath :: SourceFileInput -> Common.SourceCodeFilePath
sourceCodeFilePath = Common.SourceCodeFilePath . filename

sourceContent :: SourceFileInput -> Common.SourceCodeContent
sourceContent = Common.SourceCodeContent . content

additionalInfo :: SourceFileInput -> Common.AdditionalRepoInfo
additionalInfo src = Common.AdditionalRepoInfo (source_containing_dirs src) (all_filenames src) (optional_github_url src) (convertPathMappings (path_mappings src))

convertPathMappings :: Maybe [ PathMappingInput ] -> [ Common.PathMapping ]
convertPathMappings Nothing = []
convertPathMappings (Just mappings) = Prelude.map (\m -> Common.PathMapping (from m) (to m)) mappings

myLogger :: IO Logger
myLogger = do
    _loggerSet <- newStdoutLoggerSet defaultBufSize
    _formatter <- newTimeCache "[%d/%m/%Y ( %H:%M:%S )]"
    return $ Logger _loggerSet _formatter

timeFormat :: String
timeFormat = "[%d/%m/%Y ( %H:%M:%S )]"

messageLoggerWithoutSource :: Logger -> loc -> T.Text -> LogLevel -> LogStr -> IO ()
messageLoggerWithoutSource (Logger loggerSetValue _) _loc _source level = messageLoggerWithoutSource' level loggerSetValue

messageLoggerWithoutSource' :: LogLevel -> LoggerSet -> LogStr -> IO ()
messageLoggerWithoutSource' LevelDebug = messageDebugLoggerWithoutSource
messageLoggerWithoutSource' LevelInfo = messageInfoLoggerWithoutSource
messageLoggerWithoutSource' LevelWarn = messageWarnLoggerWithoutSource
messageLoggerWithoutSource' LevelError = messageErrorLoggerWithoutSource
messageLoggerWithoutSource' (LevelOther other) = messageOtherLoggerWithoutSource (T.unpack other)

messageDebugLoggerWithoutSource :: LoggerSet -> LogStr -> IO ()
messageDebugLoggerWithoutSource = logWithoutSource "Debug"

messageInfoLoggerWithoutSource :: LoggerSet -> LogStr -> IO ()
messageInfoLoggerWithoutSource = logWithoutSource "Info"

messageWarnLoggerWithoutSource :: LoggerSet -> LogStr -> IO ()
messageWarnLoggerWithoutSource = logWithoutSource "Warn"

messageErrorLoggerWithoutSource :: LoggerSet -> LogStr -> IO ()
messageErrorLoggerWithoutSource = logWithoutSource "Error"

messageOtherLoggerWithoutSource :: String -> LoggerSet -> LogStr -> IO ()
messageOtherLoggerWithoutSource level = logWithoutSource level

logWithoutSource :: String -> LoggerSet -> LogStr -> IO ()
logWithoutSource levelText loggerSetValue msg = do
    now <- getZonedTime
    let timestamp = formatTime defaultTimeLocale timeFormat now
    let message = BS8.unpack (fromLogStr msg)
    let line = timestamp ++ " [" ++ levelText ++ "] " ++ message
    pushLogStrLn loggerSetValue (toLogStr line)

main :: IO ()
main = do
    waiApp <- toWaiAppPlain App
    run 3000 $ defaultMiddlewaresNoLogging waiApp
