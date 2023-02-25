{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Development.Scaffold.Cabal (
  ProjectOptions (..),
  newProject,
  runApp,
  projectOptionsP,
  decompressTemplate,
  importTemplate,
) where

import Conduit ((.|))
import Conduit qualified as C
import Control.Lens (re, (^?!))
import Data.Aeson qualified as J
import Data.Aeson.KeyMap qualified as JKM
import Data.Aeson.Lens qualified as JL
import Data.Bifunctor qualified as Bi
import Data.Generics.Labels ()
import Data.Maybe (fromJust)
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LT
import Data.Yaml qualified as Y
import Development.Scaffold.Cabal.Config
import Development.Scaffold.Cabal.Constants (getDataDir, getGlobalConfigFilePath)
import Development.Scaffold.Cabal.Snapshots
import Development.Scaffold.Cabal.Template
import Network.HTTP.Client.Conduit (Response (..), newManager)
import Network.HTTP.Conduit (http)
import Options.Applicative qualified as Opt
import Path
import Path.IO (AnyPath (makeRelativeToCurrentDir), copyFile, createDirIfMissing, doesDirExist, doesFileExist, findExecutable, listDir, removeDirRecur, resolveDir', resolveFile')
import RIO
import RIO.Directory qualified as RIOD
import RIO.Orphans (HasResourceMap (..), ResourceMap, withResourceMap)
import RIO.Process (proc, runProcess, withProcessContextNoLogging, withWorkingDir)
import RIO.Text qualified as T
import RIO.Time
import Streaming.ByteString qualified as Q
import Streaming.Prelude qualified as S

runApp :: MonadUnliftIO m => RIO App () -> m ()
runApp act = do
  config <- Y.decodeFileThrow . fromAbsFile =<< getGlobalConfigFilePath
  logOpts <- logOptionsHandle stdout True
  withLogFunc logOpts $ \logger ->
    withResourceMap $ \resourceMap ->
      runRIO App {..} act

data ProjectOptions = ProjectOptions
  { projectName :: T.Text
  , template :: Maybe String
  , resolver :: PartialSnapshotName
  , additionalParams :: J.Object
  }
  deriving (Show, Eq, Ord, Generic)

projectOptionsP :: Opt.Parser ProjectOptions
projectOptionsP = do
  resolver <-
    Opt.option (Opt.maybeReader parsePartialSnapsthot) $
      Opt.long "resolver"
        <> Opt.value (PartialLTS Nothing)
        <> Opt.showDefault
        <> Opt.help "Stackage resolver"
  projectName <-
    Opt.strArgument $
      Opt.metavar "PROJECT_NAME" <> Opt.help "Project name"
  additionalParams <-
    JKM.fromList
      <$> Opt.many
        ( Opt.option (Opt.maybeReader fieldReader) $
            Opt.long "param"
              <> Opt.short 'p'
              <> Opt.metavar "KEY:VALUE"
              <> Opt.help "Parameters to specify explicitly"
        )
  template <-
    Opt.optional $
      Opt.strArgument $
        Opt.metavar "TEMPLATE"
          <> Opt.help "Template Name or path"
  pure ProjectOptions {..}

fieldReader :: String -> Maybe (J.Key, J.Value)
fieldReader str =
  case break (== ':') str of
    (l, ':' : r) -> pure (fromString l, fromString r)
    _ -> Nothing

importTemplate :: T.Text -> FilePath -> RIO App ()
importTemplate name dirOrFile = do
  dataDir <- getDataDir
  createDirIfMissing True dataDir
  let dest = dataDir </> fromJust (parseRelFile $ T.unpack name <> ".hsfiles")
  already <- doesFileExist dest
  when already $
    throwString $
      "The template `" <> T.unpack name <> "' is already present."
  isFile <- RIOD.doesFileExist dirOrFile
  isDir <- RIOD.doesDirectoryExist dirOrFile
  unless (isFile || isDir) $ do
    throwString $ "Source template `" <> dirOrFile <> "' doesn't exist or not a regular file/directory"
  if isFile
    then do
      src <- resolveFile' dirOrFile
      S.effects (decodeTemplate $ Q.readFile dirOrFile)
        `catchAny` \exc ->
          throwString $ "Invalid template: " <> displayException exc
      copyFile src dest
    else do
      src <- resolveDir' dirOrFile
      encodeDirToTemplate src & Q.writeFile (fromAbsFile dest)
  logInfo $ "Template saved: " <> fromString (fromAbsFile dest)

decompressTemplate :: String -> FilePath -> RIO App ()
decompressTemplate targ dest0 = do
  cfg <- view #config
  dest <- resolveDir' dest0
  makeSureEmpty dest
  pth <-
    maybe (throwString $ "Template not found: " <> targ) pure
      =<< searchTemplatePath cfg targ
  readTemplateFile pth
    & decodeTemplate
    & S.map (Bi.second $ LT.toStrict . LT.decodeUtf8)
    & sinkToDir dest

makeSureEmpty :: MonadIO m => Path b Dir -> m ()
makeSureEmpty dest = do
  there <- doesDirExist dest
  when there $ do
    (ls, rs) <- listDir dest
    unless (null ls && null rs) $
      throwString $
        "Target directory not empty: " <> toFilePath dest

data App = App
  { config :: ScaffoldConfig
  , resourceMap :: ResourceMap
  , logger :: LogFunc
  }
  deriving (Generic)

instance HasLogFunc App where
  logFuncL = #logger

instance HasResourceMap App where
  resourceMapL = #resourceMap

newProject :: ProjectOptions -> RIO App ()
newProject ProjectOptions {..} = do
  dest <- resolveDir' $ T.unpack projectName
  makeSureEmpty dest
  dir <- makeRelativeToCurrentDir dest
  logInfo $ "Creating project to " <> fromString (fromRelDir dir)
  handleAny (\exc -> removeDirRecur dest >> throwIO exc) $ do
    createDirIfMissing True dest
    -- FIXME: use caching
    snap <-
      maybe (throwString "Snapshot resolution failed!") pure
        =<< resolveSnapshot Nothing Nothing resolver
    logInfo $ "Using snapsthot: " <> displayShow snap
    let req = fromString $ freezeFileUrl snap
    rsp <- http req =<< newManager
    C.runConduit $
      responseBody rsp
        .| C.sinkFile
          (fromAbsFile $ dest </> [relfile|cabal.project.freeze|])
    defTmplt <- view $ #config . #defaults . #template
    cfg <- view #config
    time <- zonedTimeToLocalTime <$> getZonedTime
    let YearMonthDay year month _day = localDay time
        tmpltName = fromMaybe "new-template" $ template <|> defTmplt
        params = cfg ^. #params
        configed = params ^?! re (JL._JSON' @J.Value) . JL._Object
        ctx =
          J.Object $
            mconcat
              [ configed
              , JKM.fromList
                  [ ("name", J.toJSON projectName)
                  , ("year", J.toJSON year)
                  , ("month", J.toJSON month)
                  ,
                    ( "copyright"
                    , fromMaybe
                        ( J.toJSON $
                            show year <> " (c) " <> T.unpack (params ^. #authorName)
                        )
                        $ JKM.lookup "copyright" configed
                    )
                  ]
              , additionalParams
              ]
    mtemplate <- searchTemplatePath cfg tmpltName
    case mtemplate of
      Nothing -> throwString "No template file found!"
      Just fp -> do
        pkgYamls <-
          readTemplateFile fp
            & decodeTemplate
            & S.store
              ( S.map fst
                  >>> S.filter ((== [relfile|package.yaml|]) . filename)
                  >>> S.toList_
              )
            & applyMustache ctx
            & sinkToDir dest
        hpack <- view $ #config . #hpack

        when (not (null pkgYamls) && hpack) $ do
          hpackExe <- findExecutable [relfile|hpack|]
          case hpackExe of
            Just hp -> do
              logInfo "package.yaml is found and hpack is enabled. generatring..."
              withProcessContextNoLogging $
                withWorkingDir (fromAbsDir dest) $
                  forM_ pkgYamls $ \yaml ->
                    proc (toFilePath hp) [fromRelFile yaml] runProcess
            Nothing ->
              logWarn "package.yaml is found, but no hpack is found. Skipping."

        liftIO $
          T.writeFile
            (fromAbsFile $ dest </> [relfile|cabal.project|])
            "packages: *.cabal"
        logInfo "Project Created."
