{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Development.Scaffold.Cabal.Template.Presets (
  presetTemplates,
  writePresetTemplates,
) where

import qualified Data.ByteString as BS
import Data.FileEmbed (embedDir)
import Data.Maybe (fromJust)
import Development.Scaffold.Cabal
import Development.Scaffold.Cabal.Constants
import Path
import Path.IO
import RIO

presetTemplates :: [(FilePath, BS.ByteString)]
presetTemplates = $(embedDir "data")

writePresetTemplates :: RIO App ()
writePresetTemplates = do
  dataDir <- getDataDir
  createDirIfMissing True dataDir
  forM_ presetTemplates $ \(fp, content) -> do
    let target = dataDir </> fromJust (parseRelFile fp)
    exists <- doesFileExist target
    if exists
      then do
        writeFileBinary (toFilePath target) content
        logInfo $ "Written: " <> displayShow target
      else
        logWarn $ "Skipped: " <> displayShow target <> " (already exists)"
