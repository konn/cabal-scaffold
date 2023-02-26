{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
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

import Conduit ((.|))
import qualified Conduit as C
import Control.Lens (imapM_)
import Control.Monad.Catch.Pure (CatchT (..))
import Control.Monad.Trans.Writer.Strict (execWriterT)
import Data.Aeson (FromJSON)
import qualified Data.Aeson as J
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce (coerce)
import Data.Functor.Of (Of)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Development.Scaffold.Cabal.Config
import Development.Scaffold.Cabal.Constants
import Network.HTTP.Conduit (Request (..), Response (..), http, newManager, tlsManagerSettings)
import Network.HTTP.Types (status200)
import Path (Abs, Dir, File, Path, Rel, addExtension, fromRelFile, parent, parseAbsDir, parseAbsFile, parseRelDir, parseRelFile, toFilePath, (</>))
import Path.IO
import qualified Path.IO as P
import RIO
import Streaming (hoist)
import qualified Streaming.ByteString as Q
import qualified Streaming.Prelude as S
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
    Right OnlineTemplate {..} -> do
      man <- liftIO $ newManager tlsManagerSettings
      let etagFile = fromJust $ addExtension ".etag" fp
      thereETag <- doesFileExist etagFile
      etag <-
        if thereETag
          then Just <$> readFileBinary (toFilePath etagFile)
          else pure Nothing
      let req =
            (fromString download_url)
              { requestHeaders =
                  [("If-None-Match", et) | et <- maybeToList etag]
              }
      rsp <- http req man
      if responseStatus rsp == status200
        then C.runConduit $ responseBody rsp .| C.mapM_C Q.fromStrict
        else Q.fromStrict $ coerce content
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
            & S.map (\fp -> (toFilePath fp, liftIO $ BS.readFile $ toFilePath $ dir </> fp))
        )
        C..| createTemplate
        C..| C.mapM_C S.yield
    )
    & Q.fromChunks

dirFiles :: MonadIO m => Path b Dir -> S.Stream (Of (Path Rel File)) m ()
dirFiles = P.walkDirRel $ \dir _ chs -> WalkExclude [] <$ S.map (dir </>) (S.each chs)

fromByteStream ::
  Monad m =>
  Q.ByteStream m r ->
  C.ConduitT a BS.ByteString m r
fromByteStream =
  Q.chunkMapM_ C.yield . hoist lift

decodeTemplate ::
  (MonadThrow m) =>
  Q.ByteStream m () ->
  S.Stream (Of (Path Rel File, LBS.ByteString)) m ()
decodeTemplate bs = do
  dic <-
    lift $
      execWriterT
        ( runCatchT
            ( C.runConduit $
                fromByteStream (hoist (lift . lift) bs)
                  C..| unpackTemplate receiveMem id
            )
        )
  imapM_ (curry S.yield . fromJust . parseRelFile) dic

type Context = J.Value

applyMustache ::
  (MonadThrow m) =>
  Context ->
  S.Stream (Of (Path Rel File, LBS.ByteString)) m r ->
  S.Stream (Of (Path Rel File, T.Text)) m r
applyMustache ctx =
  S.mapM $ \(fp, bs) -> do
    tmplt <-
      either (throwM . userError . show) pure $
        compileTemplate (fromRelFile fp) $
          LT.toStrict $
            LT.decodeUtf8 bs
    pure (fp, substitute tmplt ctx)

sinkToDir ::
  MonadIO m =>
  -- | Base Directory
  Path b Dir ->
  S.Stream (Of (Path Rel File, T.Text)) m r ->
  m r
sinkToDir baseDir =
  S.mapM_ $ \(fp, txt) -> do
    let dest = baseDir </> fp
    createDirIfMissing True $ parent dest
    liftIO $ T.writeFile (toFilePath dest) txt
