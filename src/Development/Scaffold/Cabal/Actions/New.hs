{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoFieldSelectors #-}

module Development.Scaffold.Cabal.Actions.New (
  ProjectOptions (..),
  newProject,
  projectOptionsP,
) where

import Conduit ((.|))
import qualified Conduit as C
import Control.Applicative (empty)
import qualified Control.Foldl as L
import Control.Lens (re, (^?!), _1)
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as JKM
import qualified Data.Aeson.Lens as JL
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.Char as Char
import Data.Generics.Labels ()
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Development.Scaffold.Cabal.Config
import Development.Scaffold.Cabal.Runner
import Development.Scaffold.Cabal.Snapshots
import Development.Scaffold.Cabal.Template
import Network.HTTP.Client.Conduit (Response (..), newManager)
import Network.HTTP.Conduit (http)
import qualified Options.Applicative as Opt
import Path
import Path.IO (createDirIfMissing, findExecutable, makeRelativeToCurrentDir, removeDirRecur, resolveDir')
import Path.IO.Utils (makeSureEmpty)
import RIO
import RIO.Process (proc, runProcess, withProcessContextNoLogging, withWorkingDir)
import qualified RIO.Text as T
import RIO.Time
import qualified Streaming.ByteString as Q
import Streaming.ByteString.Char8.Replace.Attoparsec (replaceAll)
import qualified Streaming.Prelude as S

data ProjectOptions = ProjectOptions
  { projectName :: T.Text
  , template :: Maybe String
  , resolver :: PartialSnapshotName
  , noProjectFile :: Maybe Bool
  , additionalParams :: J.Object
  , withCompiler :: Maybe Text
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
  additionalParams <-
    JKM.fromList
      <$> Opt.many
        ( Opt.option (Opt.maybeReader fieldReader) $
            Opt.long "param"
              <> Opt.short 'p'
              <> Opt.metavar "KEY:VALUE"
              <> Opt.help "Parameters to specify explicitly"
        )
  withCompiler <-
    Opt.optional $
      Opt.strOption $
        Opt.long "with-compiler"
          <> Opt.short 'w'
          <> Opt.metavar "COMPILER"
          <> Opt.help "The compiler to use in the project"
  projectName <-
    Opt.strArgument $
      Opt.metavar "PROJECT_NAME" <> Opt.help "Project name"
  template <-
    Opt.optional $
      Opt.strArgument $
        Opt.metavar "TEMPLATE"
          <> Opt.help "Template Name or path"
  noProjectFile <-
    Opt.flag'
      (Just True)
      ( Opt.long "no-project-file"
          <> Opt.help "Create cabal.project and freeze files (default)"
          <> Opt.internal
      )
      <|> Opt.flag
        Nothing
        (Just False)
        ( Opt.long "project-file"
            <> Opt.help "Do not create cabal.project and freeze files"
            <> Opt.internal
        )
      <|> ( empty
              <* Opt.flag'
                Nothing
                ( Opt.long "[no-]project-file"
                    <> Opt.help "Whether to create cabal.project and freeze files or not (default: --project-file)"
                )
          )
  pure ProjectOptions {..}

fieldReader :: String -> Maybe (J.Key, J.Value)
fieldReader str =
  case break (== ':') str of
    (l, ':' : r) -> pure (fromString l, fromString r)
    _ -> Nothing

newProject :: ProjectOptions -> RIO App ()
newProject ProjectOptions {..} = do
  dest <- resolveDir' $ T.unpack projectName
  makeSureEmpty dest
  dir <- makeRelativeToCurrentDir dest
  logInfo $ "Creating project to " <> fromString (fromRelDir dir)
  handleAny (\exc -> removeDirRecur dest >> throwIO exc) $ do
    cfg <- view #config
    createDirIfMissing True dest
    let genProject =
          maybe True not $
            noProjectFile <|> cfg.defaults.noProject
    mver <-
      if genProject
        then do
          -- FIXME: use caching
          snap <-
            maybe (throwString "Snapshot resolution failed!") pure
              =<< resolveSnapshot Nothing Nothing resolver
          logInfo $ "Using snapsthot: " <> displayShow snap
          forM_ withCompiler $ \ghc ->
            logInfo $ "With compiler: " <> display ghc
          let req = fromString $ freezeFileUrl snap
              freezePath = fromAbsFile $ dest </> [relfile|cabal.project.freeze|]
          rsp <- http req =<< newManager
          C.runConduit
            (C.transPipe lift (responseBody rsp) .| C.mapM_C Q.fromStrict)
            & maybe id (replaceAll . rewriteCompiler) withCompiler
            & Q.writeFile freezePath
          Just <$> getSnapshotGHC snap
        else Nothing <$ logInfo "Project generation is disabled."

    defTmplt <- view $ #config . #defaults . #template
    time <- zonedTimeToLocalTime <$> getZonedTime
    let YearMonthDay year month _day = localDay time
        tmpltName = fromMaybe "new-template" $ template <|> defTmplt
        params = cfg ^. #params
        configed = params ^?! re (JL._JSON' @J.Value) . JL._Object
        ctx =
          J.Object $
            mconcat
              [ configed
              , maybe mempty (JKM.singleton "ghc" . J.toJSON) mver
              , JKM.fromList
                  [ ("name", J.toJSON projectName)
                  ,
                    ( "Paths_module"
                    , J.toJSON $ "Paths_" <> T.replace "-" "_" projectName
                    )
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
        (pkgYamls, projectThere) <-
          readTemplateFile fp
            & decodeTemplate
            & applyMustache ctx
            & S.store
              ( L.purely S.fold_ $
                  L.handles _1 $
                    (,)
                      <$> L.prefilter ((== [relfile|package.yaml|]) . filename) L.list
                      <*> L.elem [relfile|cabal.project|]
              )
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

        when genProject $
          unless projectThere $
            liftIO $
              T.writeFile
                (fromAbsFile $ dest </> [relfile|cabal.project|])
                "packages: **/*.cabal"
        logInfo "Project Created."

rewriteCompiler :: Text -> A.Parser BS.ByteString
rewriteCompiler compiler =
  ("with-compiler: " <> T.encodeUtf8 compiler)
    <$ A.string "with-compiler"
    <* A.skipMany A.space
    <* A.char ':'
    <* A.skipMany A.space
    <* some (A.satisfy $ \c -> Char.isAlphaNum c || c `elem` ['-', '_', '.'])
