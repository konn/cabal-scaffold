{-# LANGUAGE QuasiQuotes #-}

module Development.Scaffold.Cabal.Constants (
  getGlobalConfigFilePath,
  getDataDir,
) where

import Data.Functor ((<&>))
import Path
import Path.IO
import RIO (MonadIO)

getGlobalConfigFilePath :: (MonadIO m) => m (Path Abs File)
getGlobalConfigFilePath =
  getXdgDir XdgConfig Nothing
    <&> (</> [relfile|cabal-scaffold.yaml|])

getDataDir :: (MonadIO m) => m (Path Abs Dir)
getDataDir = getXdgDir XdgData (Just appDir)

appDir :: Path Rel Dir
appDir = [reldir|cabal-scaffold|]
