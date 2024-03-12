{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
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
  listTemplateDirs,
  templateStackageContext,
) where

import Conduit ((.|))
import qualified Conduit as C
import Control.Applicative (empty)
import Control.Lens (alaf, imapM_)
import Control.Monad.Catch.Pure (CatchT (..))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Writer.Strict (execWriterT)
import Data.Aeson (FromJSON)
import qualified Data.Aeson as J
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Bifunctor as Bi
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Coerce (coerce)
import Data.Foldable1 (Foldable1 (..))
import Data.Functor.Of (Of)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust)
import Data.Monoid (Ap (..), First (..))
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Development.Scaffold.Cabal.Config
import Development.Scaffold.Cabal.Constants
import Development.Scaffold.Cabal.Runner (App (..))
import Development.Scaffold.Cabal.Snapshots (PartialSnapshotName (..), freezeFileUrl, getLatestSnapshots, parsePartialSnapsthot, resolveSnapshot)
import Network.HTTP.Conduit (Request (..), Response (..), http, newManager, tlsManagerSettings)
import Network.HTTP.Simple (httpLbs)
import Network.HTTP.Types (status200)
import Path (Abs, Dir, File, Path, Rel, SomeBase (..), addExtension, fromRelFile, parent, parseAbsFile, parseRelDir, parseRelFile, parseSomeDir, toFilePath, (</>))
import Path.IO
import qualified Path.IO as P
import RIO
import Streaming (hoist)
import qualified Streaming.ByteString as Q
import qualified Streaming.Prelude as S
import Text.Mustache (Template (..), compileTemplate, substitute)
import Text.Mustache.Types (DataIdentifier (..), Node (..))
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

templateStackageContext ::
  ( MonadUnliftIO m
  ) =>
  Template ->
  m (AKM.KeyMap J.Value)
templateStackageContext tplt = do
  let vars = mustacheVariables tplt
  snaps <- getLatestSnapshots Nothing Nothing
  flip (alaf Ap foldMap) vars $
    NE.nonEmpty >>> \mkeys -> do
      fold <$> runMaybeT do
        keys <- MaybeT $ pure mkeys
        let origkeys = fmap CI.original keys
        url <- case keys of
          "stackage" :| ["nightly"] -> do
            snap <- lift $ resolveSnapshot Nothing (Just snaps) (PartialNightly Nothing)
            maybe (throwString "nightly snapshot not found") (pure . freezeFileUrl) snap
          "stackage" :| ["nightly", stamp] -> do
            snap <-
              lift . resolveSnapshot Nothing (Just snaps)
                =<< MaybeT
                  ( pure $
                      parsePartialSnapsthot
                        ("nightly- " <> T.unpack stamp.original)
                  )
            maybe (throwString "nightly snapshot not found") (pure . freezeFileUrl) snap
          "stackage" :| ["lts"] -> do
            snap <-
              lift . resolveSnapshot Nothing (Just snaps) $ PartialLTS Nothing
            maybe (throwString "LTS snapshot not found") (pure . freezeFileUrl) snap
          "stackage" :| ["lts", major] -> do
            snap <-
              lift . resolveSnapshot Nothing (Just snaps)
                =<< MaybeT
                  ( pure $
                      parsePartialSnapsthot
                        ("lts-" <> T.unpack major.original)
                  )
            maybe (throwString "LTS snapshot not found") (pure . freezeFileUrl) snap
          "stackage" :| ["lts", major, minor] -> do
            snap <-
              lift . resolveSnapshot Nothing (Just snaps)
                =<< MaybeT
                  ( pure $
                      parsePartialSnapsthot
                        ( "lts-"
                            <> T.unpack major.original
                            <> "."
                            <> T.unpack minor.original
                        )
                  )
            maybe (throwString "LTS snapshot not found") (pure . freezeFileUrl) snap
          _ -> empty
        httpLbs (fromString url) <&> \txt ->
          foldrMap1'
            (flip AKM.singleton (J.toJSON $ LT.decodeUtf8 (responseBody txt)) . AK.fromText)
            (\t -> AKM.singleton (AK.fromText t) . J.Object)
            -- (flip AKM.singleton $ J.toJSON $ LT.decodeUtf8 (responseBody txt))
            -- AKM.singleton
            origkeys

mustacheVariables :: Template -> Set [CI Text]
mustacheVariables = foldMap go . ast
  where
    go (TextBlock _) = mempty
    go (Section di t) = identSet di <> foldMap go t
    go (InvertedSection di t) = identSet di <> foldMap go t
    go Partial {} = mempty
    go (Variable _ di) = identSet di
    identSet (NamedData k) = Set.singleton $ map CI.mk k
    identSet Implicit = mempty

listTemplateDirs :: (MonadIO m, MonadReader App m, MonadThrow m) => ScaffoldConfig -> m (NonEmpty (Path Abs Dir))
listTemplateDirs ScaffoldConfig {..} = do
  (:|) <$> getDataDir <*> mapM expandAndResolve templateDirs

searchTemplatePath ::
  (MonadIO m, MonadReader App m, MonadThrow m) =>
  ScaffoldConfig ->
  String ->
  m (Maybe (Path Abs File))
searchTemplatePath cfg hint =
  case parseAbsFile hint of
    Just p -> do
      there <- doesFileExist p
      if there then pure $ Just p else pure Nothing
    Nothing -> do
      targ <- resolveFile' hint
      there <- doesFileExist targ
      if there
        then pure $ Just targ
        else
          fmap getFirst . getAp . foldMap (Ap . fmap First . (`findInDir` hint))
            =<< listTemplateDirs cfg

expandAndResolve :: (MonadIO m, MonadReader App m, MonadThrow m) => FilePath -> m (Path Abs Dir)
expandAndResolve "~" = getHomeDir
expandAndResolve ('~' : '/' : rest) = do
  home <- getHomeDir
  either throwIO (pure . (home </>)) (parseRelDir rest)
expandAndResolve fp = do
  somePath <- parseSomeDir fp
  case somePath of
    Abs d -> pure d
    Rel rel -> do
      asks (.configBasePath) >>= \case
        Just base -> pure $ base </> rel
        Nothing ->
          throwString "Relative path is not allowed with default config."

findInDir :: (MonadIO m) => Path Abs Dir -> String -> m (Maybe (Path Abs File))
findInDir dir hint = fmap join $ forM (parseRelFile (hint <> ".hsfiles")) $ \fp -> do
  let targ = dir </> fp
  there <- doesFileExist targ
  pure $ targ <$ guard there

encodeDirToTemplate :: (MonadIO m) => Path b Dir -> Q.ByteStream m ()
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

dirFiles :: (MonadIO m) => Path b Dir -> S.Stream (Of (Path Rel File)) m ()
dirFiles = P.walkDirRel $ \dir _ chs -> WalkExclude [] <$ S.map (dir </>) (S.each chs)

fromByteStream ::
  (Monad m) =>
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
  ( MonadThrow m
  , MonadUnliftIO m
  , HasCallStack
  ) =>
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
    !ctx' <-
      templateStackageContext tmplt <&> \dic ->
        if AKM.null dic
          then ctx
          else case ctx of
            J.Object dic0 -> J.Object $ dic0 <> AKM.map J.toJSON dic
            _ -> error "Invalid context: object required"
    fp' <-
      either throwM pure $
        parseRelFile . T.unpack . flip substitute ctx
          =<< Bi.first (toException . stringException . show) (compileTemplate "<FileName>" (T.pack $ toFilePath fp))
    pure (fp', substitute tmplt ctx')

sinkToDir ::
  (MonadIO m) =>
  -- | Base Directory
  Path b Dir ->
  S.Stream (Of (Path Rel File, T.Text)) m r ->
  m r
sinkToDir baseDir =
  S.mapM_ $ \(fp, txt) -> do
    let dest = baseDir </> fp
    createDirIfMissing True $ parent dest
    liftIO $ T.writeFile (toFilePath dest) txt
