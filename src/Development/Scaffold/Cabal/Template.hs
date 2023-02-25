{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Development.Scaffold.Cabal.Template (
  OnlineTemplate (..),
  decodeTemplate,
  searchTemplatePath,
  applyMustache,
  sinkToDir,
  encodeDirToTemplate,
  readTemplateFile,
) where

import Conduit qualified as C
import Control.Monad.Catch.Pure (CatchT (..))
import Data.Aeson (FromJSON)
import Data.Aeson qualified as J
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.Functor.Of (Of)
import Data.Maybe (fromJust)
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Development.Scaffold.Cabal.Config
import Development.Scaffold.Cabal.Constants
import GHC.Records (HasField (..))
import Path (Abs, Dir, File, Path, Rel, fromRelFile, parent, parseAbsDir, parseAbsFile, parseRelDir, parseRelFile, toFilePath, (</>))
import Path.IO
import Path.IO qualified as P
import RIO
import Streaming (hoist)
import Streaming.ByteString qualified as Q
import Streaming.Prelude qualified as S
import Text.Mustache (compileTemplate, substitute)
import Text.ProjectTemplate

data OnlineTemplate = OnlineTemplate
  { download_url :: String
  , content :: DecodedBase64
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON)

newtype DecodedBase64 = DecodedBase64 {runBase64 :: BS.ByteString}
  deriving (Show, Eq, Ord, Generic)

instance FromJSON DecodedBase64 where
  parseJSON = J.withText "base64string" $ \txt ->
    pure $ DecodedBase64 $ B64.decodeLenient $ T.encodeUtf8 txt

newtype ETag = ETag {runETag :: Text}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (IsString)

readTemplateFile :: (C.MonadResource m) => Path b File -> Q.ByteStream m ()
readTemplateFile fp = do
  eith <- liftIO $ J.eitherDecodeFileStrict @OnlineTemplate $ toFilePath fp
  case eith of
    Right OnlineTemplate {..} -> Q.fromStrict $ getField @"runBase64" content
    Left {} -> Q.readFile (toFilePath fp)

searchTemplatePath ::
  MonadIO m =>
  ScaffoldConfig ->
  String ->
  m (Maybe (Path Abs File))
searchTemplatePath ScaffoldConfig {..} hint =
  case parseAbsFile hint of
    Just p -> do
      there <- doesFileExist p
      if there then pure $ Just p else pure Nothing
    Nothing -> do
      targ <- resolveFile' hint
      there <- doesFileExist targ
      dataDir <- getDataDir
      temps <- mapM expandAndResolve templateDirs
      if there
        then pure $ Just targ
        else asum <$> mapM (`findInDir` hint) (temps <> [dataDir])

expandAndResolve :: MonadIO m => FilePath -> m (Path Abs Dir)
expandAndResolve "~" = getHomeDir
expandAndResolve ('~' : '/' : rest) = do
  home <- getHomeDir
  either throwIO (pure . (home </>)) (parseRelDir rest)
expandAndResolve fp = either throwIO pure $ parseAbsDir fp

findInDir :: MonadIO m => Path Abs Dir -> String -> m (Maybe (Path Abs File))
findInDir dir hint = fmap join $ forM (parseRelFile (hint <> ".hsfiles")) $ \fp -> do
  let targ = dir </> fp
  there <- doesFileExist targ
  pure $ targ <$ guard there

encodeDirToTemplate :: MonadIO m => Path b Dir -> Q.ByteStream m ()
encodeDirToTemplate dir =
  C.runConduit
    ( S.mapM_
        C.yield
        ( dirFiles dir
            & S.map (\fp -> (toFilePath fp, liftIO $ BS.readFile $ toFilePath fp))
        )
        C..| createTemplate
        C..| C.mapM_C S.yield
    )
    & Q.fromChunks

dirFiles :: MonadIO m => Path b Dir -> S.Stream (Of (Path Rel File)) m ()
dirFiles = P.listDirRecurRel >=> snd >>> S.each

fromByteStream ::
  Monad m =>
  Q.ByteStream m r ->
  C.ConduitT a BS.ByteString m r
fromByteStream =
  Q.chunkMapM_ C.yield . hoist lift

decodeTemplate ::
  (MonadThrow m) =>
  Q.ByteStream m () ->
  S.Stream (Of (Path Rel File, ByteString)) m ()
decodeTemplate bs = do
  either (lift . throwM) pure
    =<< runCatchT
      ( C.runConduit $
          fromByteStream (hoist (lift . lift) bs)
            C..| unpackTemplate (\fp -> C.mapM_C $ lift . S.yield . (fromJust $ parseRelFile fp,)) id
      )

type Context = J.Value

applyMustache ::
  MonadThrow m =>
  Context ->
  S.Stream (Of (Path Rel File, ByteString)) m () ->
  S.Stream (Of (Path Rel File, Text)) m ()
applyMustache ctx =
  S.mapM $ \(fp, bs) -> do
    tmplt <-
      either (throwM . userError . show) pure $
        compileTemplate (fromRelFile fp) $
          T.decodeUtf8 bs
    pure (fp, substitute tmplt ctx)

sinkToDir ::
  MonadIO m =>
  -- | Base Directory
  Path b Dir ->
  S.Stream (Of (Path Rel File, Text)) m r ->
  m r
sinkToDir baseDir =
  S.mapM_ $ \(fp, txt) -> do
    let dest = baseDir </> fp
    createDirIfMissing True $ parent dest
    liftIO $ T.writeFile (toFilePath dest) txt
