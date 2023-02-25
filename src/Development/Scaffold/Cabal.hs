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
) where

import Conduit ((.|))
import Conduit qualified as C
import Control.Lens (re, (^?!))
import Data.Aeson qualified as J
import Data.Aeson.KeyMap qualified as JKM
import Data.Aeson.Lens qualified as JL
import Data.Bifunctor qualified as Bi
import Data.Generics.Labels ()
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Data.Yaml qualified as Y
import Development.Scaffold.Cabal.Config
import Development.Scaffold.Cabal.Constants (getGlobalConfigFilePath)
import Development.Scaffold.Cabal.Snapshots
import Development.Scaffold.Cabal.Template
import Network.HTTP.Client.Conduit (Response (..), newManager)
import Network.HTTP.Conduit (http)
import Options.Applicative qualified as Opt
import Path
import Path.IO (createDirIfMissing, doesDirExist, listDir, removeDirRecur, resolveDir')
import RIO
import RIO.Orphans (HasResourceMap (..), ResourceMap, withResourceMap)
import RIO.Text qualified as T
import RIO.Time
import Streaming.Prelude qualified as S

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
    & S.map (Bi.second T.decodeUtf8)
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

runApp :: MonadUnliftIO m => RIO App () -> m ()
runApp act = do
  config <- Y.decodeFileThrow . fromAbsFile =<< getGlobalConfigFilePath
  logOpts <- logOptionsHandle stdout True
  withLogFunc logOpts $ \logger ->
    withResourceMap $ \resourceMap ->
      runRIO App {..} act

newProject :: ProjectOptions -> RIO App ()
newProject ProjectOptions {..} = do
  dest <- resolveDir' $ T.unpack projectName
  makeSureEmpty dest
  handleAny (\exc -> removeDirRecur dest >> throwIO exc) $ do
    createDirIfMissing True dest
    -- FIXME: use caching
    snap <-
      maybe (throwString "Snapshot resolution failed!") pure
        =<< resolveSnapshot Nothing Nothing resolver
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
    unless (isJust mtemplate) $
      throwString "No template file found!"
    forM_ mtemplate $ \fp ->
      readTemplateFile fp
        & decodeTemplate
        & applyMustache ctx
        & sinkToDir dest
    liftIO $
      T.writeFile
        (fromAbsFile $ dest </> [relfile|cabal.project|])
        "packages: *.cabal"

    pure ()
