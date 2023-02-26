{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Development.Scaffold.Cabal.Actions.Import (
  importTemplate,
  ImportOptions (..),
  importOptionsP,
) where

import Data.Generics.Labels ()
import Data.Maybe (fromJust)
import Data.Text.Lens (packed)
import Development.Scaffold.Cabal.Constants (getDataDir)
import Development.Scaffold.Cabal.Runner
import Development.Scaffold.Cabal.Template
import qualified Options.Applicative as Opt
import Path
import Path.IO (copyFile, createDirIfMissing, doesFileExist, removeFile, renameFile, resolveDir', resolveFile')
import RIO
import qualified RIO.Directory as RIOD
import qualified RIO.FilePath as RawFP
import qualified RIO.Text as T
import RIO.Time (defaultTimeLocale, formatTime, getZonedTime)
import qualified Streaming.ByteString as Q
import qualified Streaming.Prelude as S

data ImportOptions = ImportOptions
  { templatePath :: FilePath
  , templateName :: Maybe T.Text
  , override :: Bool
  }
  deriving (Show, Eq, Ord, Generic)

importOptionsP :: Opt.Parser ImportOptions
importOptionsP = do
  override <-
    Opt.switch $
      Opt.long "override"
        <> Opt.short 'S'
        <> Opt.help
          "If on, this will overrides the existing templates"
  templatePath <- Opt.strArgument $ Opt.metavar "PATH" <> Opt.help "The path to the hsfiles tempalte-file or directory to import as a template"
  templateName <-
    Opt.optional $
      Opt.strArgument $
        Opt.metavar "TEMPLATE_NAME"
          <> Opt.help "The name of the template. If omitted, uses the base name (without extension) of input template path."
  pure ImportOptions {..}

importTemplate :: HasCallStack => ImportOptions -> RIO App ()
importTemplate ImportOptions {..} = do
  now <- getZonedTime
  dataDir <- getDataDir
  createDirIfMissing True dataDir
  rawFP <-
    RIOD.canonicalizePath templatePath
      <&> packed %~ T.dropWhileEnd (== RawFP.pathSeparator)
  isFile <- RIOD.doesFileExist rawFP
  isDir <- RIOD.doesDirectoryExist rawFP
  unless (isFile || isDir) $ do
    throwString $ "Source template `" <> templatePath <> "' doesn't exist or not a regular file/directory"
  let defName = T.pack $ RawFP.takeBaseName templatePath
      name = fromMaybe defName templateName
  when (T.null name) $
    throwString "Template name MUST NOT be empty!"
  let dest = dataDir </> fromJust (parseRelFile $ T.unpack name <> ".hsfiles")
  logInfo $
    "New template will be saved as: "
      <> display name
      <> " ("
      <> fromString (fromAbsFile dest)
      <> ")"
  let backupPath =
        fromJust $
          addExtension ".bakup"
            =<< addExtension (formatTime defaultTimeLocale ".%Y%m%d-%H%M%S" now) dest

  already <- doesFileExist dest
  when already $
    if override
      then do
        logWarn $ "The existing template `" <> display name <> "' will be overriden"
        copyFile dest backupPath
        logWarn $ "The original one has been temporarilly saved as " <> fromString (fromAbsFile backupPath)
      else
        throwString $
          "The template `" <> T.unpack name <> "' is already present."
  let mainLoop = do
        if isFile
          then do
            src <- resolveFile' templatePath
            S.effects (decodeTemplate $ Q.readFile templatePath)
              `catchAny` \exc ->
                throwString $ "Invalid template: " <> displayException exc
            copyFile src dest
          else do
            src <- resolveDir' templatePath
            encodeDirToTemplate src & Q.writeFile (fromAbsFile dest)
        logInfo $ "Template saved: " <> fromString (fromAbsFile dest)
        when (override && already) $ do
          logInfo $ "You can salvage old one from: " <> fromString (fromAbsFile backupPath)
      restoreOriginal = do
        there <- doesFileExist dest
        when there $ removeFile dest
        when (override && already) $ do
          logWarn "Exception has occurred during import. Restoring the original..."
          renameFile backupPath dest
  mainLoop `onException` restoreOriginal
