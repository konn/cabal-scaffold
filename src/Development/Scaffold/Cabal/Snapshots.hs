{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Development.Scaffold.Cabal.Snapshots (
  Snapshots (..),
  SnapshotName (..),
  PartialSnapshotName (..),
  parsePartialSnapsthot,
  SnapshotHistory (..),
  defaultSnapshotUrl,
  getLatestSnapshots,
  freezeFileUrl,
  fromPartialSnapshot,
  lookupSnapshot,
  resolveSnapshot,
  HasSnapshotHistory (..),
  snapshotYamlUrl,
  getSnapshotGHC,
) where

import Conduit (MonadUnliftIO)
import Control.Lens (Lens')
import Control.Monad (replicateM)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Aeson (FromJSON (..), FromJSONKey, ToJSON, ToJSONKey)
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as AE
import qualified Data.Aeson.Key as AK
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Char as C
import Data.Coerce (coerce)
import Data.Functor ((<&>))
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Calendar
import Data.Version (Version, parseVersion)
import qualified Data.Yaml as Y
import GHC.Generics (Generic)
import Network.HTTP.Client.Conduit (HttpException (..), HttpExceptionContent (..), Request (..), Response (..), parseUrlThrow)
import Network.HTTP.Simple (httpLbs)
import Network.HTTP.Types (status304)
import RIO (Hashable, MonadThrow, ReaderT (..), handle, throwIO, throwString, tshow)
import RIO.Time (defaultTimeLocale, formatTime)
import Text.ParserCombinators.ReadP (readP_to_S)
import Text.Regex.Applicative
import qualified Text.Regex.Applicative as RE
import qualified Text.Regex.Applicative.Object as RE

data PartialSnapshotName
  = PartialLTS (Maybe (Int, Maybe Int))
  | PartialNightly (Maybe Day)
  deriving (Eq, Ord, Generic)
  deriving anyclass (Hashable)

matchText :: RE Char a -> T.Text -> Maybe a
matchText re =
  listToMaybe . RE.results . T.foldl' (flip RE.step) obj
  where
    !obj = RE.compile re

parsePartialSnapsthot :: String -> Maybe PartialSnapshotName
parsePartialSnapsthot = RE.match partialSnapshotRE

instance Show PartialSnapshotName where
  showsPrec _ (PartialLTS mlts) =
    showString "lts"
      . maybe
        id
        ( \(x, my) ->
            showChar '-' . shows x . maybe id (\y -> showChar '.' . shows y) my
        )
        mlts
  showsPrec _ (PartialNightly mday) =
    showString "nightly"
      . maybe
        id
        ( \day ->
            showChar '-' . showString (formatTime defaultTimeLocale "%0Y-%m-%d" day)
        )
        mday

instance FromJSONKey PartialSnapshotName where
  fromJSONKey = J.FromJSONKeyTextParser (maybe (fail "partial name") pure . matchText partialSnapshotRE)

instance FromJSON PartialSnapshotName where
  parseJSON =
    J.withText "snapshot name" $
      maybe (fail "lts-*-* or nightly-YYYY-mm-dd expected") pure
        . matchText partialSnapshotRE
  {-# INLINE parseJSON #-}

instance ToJSON PartialSnapshotName where
  toJSON = J.toJSON . show
  {-# INLINE toJSON #-}

instance ToJSONKey PartialSnapshotName where
  toJSONKey = J.ToJSONKeyText (AK.fromText . tshow) (AE.text . tshow)

type RE' = RE Char

partialSnapshotRE :: RE' PartialSnapshotName
partialSnapshotRE =
  asum
    [ do
        RE.string "lts"
        ver <- optional $ do
          x <- RE.sym '-' *> decimal
          my <- optional $ sym '.' *> decimal
          pure (x, my)
        pure $ PartialLTS ver
    , PartialNightly <$ RE.string "nightly" <*> optional (RE.sym '-' *> dayRE)
    ]

data SnapshotName
  = LTS !Int !Int
  | Nightly !Day
  deriving (Eq, Ord, Generic)
  deriving anyclass (Hashable)

-- >>> import Data.Ge

instance Show SnapshotName where
  {-# INLINE showsPrec #-}
  showsPrec _ = \case
    LTS x y -> showString "lts-" . shows x . showChar '.' . shows y
    Nightly day -> showString $ formatTime defaultTimeLocale "nightly-%0Y-%m-%d" day

instance ToJSON SnapshotName where
  toJSON = J.toJSON . show
  {-# INLINE toJSON #-}

instance ToJSONKey SnapshotName where
  toJSONKey = J.ToJSONKeyText (AK.fromText . tshow) (AE.text . tshow)

instance FromJSON SnapshotName where
  {-# INLINE parseJSON #-}
  parseJSON =
    J.withText "snapshot name" $
      maybe (fail "lts-*-* or nightly-YYYY-mm-dd expected") pure
        . matchText snapshotRE

decimal :: (Integral a) => RE' a
decimal =
  RE.reFoldl
    RE.Greedy
    (\l r -> l * 10 + fromIntegral r)
    0
    (C.digitToInt <$> RE.psym C.isDigit)

decimalN :: (Integral a) => Int -> RE' a
decimalN n = fromInteger . read <$> replicateM n (RE.psym C.isDigit)

dayRE :: RE Char Day
dayRE =
  YearMonthDay <$> decimalN 4 <* RE.sym '-' <*> decimalN 2 <* RE.sym '-' <*> decimalN 2

snapshotRE :: RE' SnapshotName
snapshotRE =
  LTS
    <$ RE.string "lts-"
    <*> decimal
    <* RE.sym '.'
    <*> decimal
      <|> Nightly
    <$ RE.string "nightly-"
    <*> dayRE

instance FromJSONKey SnapshotName where
  fromJSONKey = J.FromJSONKeyTextParser (maybe (fail "name") pure . matchText snapshotRE)

newtype Snapshots = Snapshots {getSnapshots :: HM.HashMap PartialSnapshotName SnapshotName}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON)

class HasSnapshotHistory a where
  snapshotHistoryL :: Lens' a SnapshotHistory

data SnapshotHistory = SnapshotHistory
  { snapshotsETag :: BS.ByteString
  , snapshotsBody :: Snapshots
  }
  deriving (Show, Eq, Ord, Generic)

getLatestSnapshots ::
  (MonadUnliftIO m) =>
  -- | The url of snapshots.json
  Maybe String ->
  Maybe SnapshotHistory ->
  m SnapshotHistory
getLatestSnapshots murl mcurrent = do
  req <-
    either throwIO pure $
      parseUrlThrow (fromMaybe defaultSnapshotUrl murl)
        <&> \r ->
          r
            { requestHeaders =
                maybe
                  []
                  ( \SnapshotHistory {..} ->
                      [("If-None-Match", snapshotsETag)]
                  )
                  mcurrent
            }
  handle
    ( \case
        HttpExceptionRequest _ (StatusCodeException rsp _)
          | responseStatus rsp == status304
          , Just prev <- mcurrent ->
              pure prev
        exc -> throwIO exc
    )
    $ do
      rsp <- httpLbs req
      let snapshotsETag =
            maybe
              ""
              (maybe "" TE.encodeUtf8 . J.decodeStrict)
              $ lookup "ETag"
              $ responseHeaders rsp
      snapshotsBody <- either throwString pure $ J.eitherDecode $ responseBody rsp
      pure SnapshotHistory {..}

defaultSnapshotUrl :: String
defaultSnapshotUrl = "https://www.stackage.org/download/snapshots.json"

fromPartialSnapshot :: PartialSnapshotName -> Maybe SnapshotName
fromPartialSnapshot (PartialLTS mxy) = do
  (x, my) <- mxy
  LTS x <$> my
fromPartialSnapshot (PartialNightly mday) = Nightly <$> mday

lookupSnapshot :: PartialSnapshotName -> Snapshots -> Maybe SnapshotName
{-# INLINE lookupSnapshot #-}
lookupSnapshot snap =
  coerce $
    asum
      [ ReaderT $ const $ fromPartialSnapshot snap
      , ReaderT $ HM.lookup snap . getSnapshots
      ]

{-
>>> resolveSnapshot Nothing Nothing $ PartialNightly Nothing
Just nightly-2023-02-14
-}
resolveSnapshot ::
  (MonadUnliftIO m) =>
  Maybe String ->
  Maybe SnapshotHistory ->
  PartialSnapshotName ->
  m (Maybe SnapshotName)
resolveSnapshot murl mhist pn =
  runMaybeT $
    asum $
      map
        MaybeT
        [ pure $ fromPartialSnapshot pn
        , do
            snaps <- getLatestSnapshots murl mhist
            pure $ lookupSnapshot pn $ snapshotsBody snaps
        ]

-- >>> freezeFileUrl $ Nightly $ YearMonthDay 2023 02 14
-- "https://www.stackage.org/nightly-2023-02-14/cabal.config"

freezeFileUrl :: SnapshotName -> String
freezeFileUrl snap = "https://www.stackage.org/" <> show snap <> "/cabal.config"

snapshotYamlUrl :: SnapshotName -> String
snapshotYamlUrl (Nightly (YearMonthDay yyyy mm dd)) =
  "https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/nightly/" <> show yyyy <> "/" <> show mm <> "/" <> show dd <> ".yaml"
snapshotYamlUrl (LTS major minor) =
  "https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/" <> show major <> "/" <> show minor <> ".yaml"

newtype RawSnapshotGHC = RawSnapshotGHC {compiler :: String}
  deriving (Show, Eq, Ord, Generic)

instance FromJSON RawSnapshotGHC where
  parseJSON = J.withObject "{compiler: ghc-*} or {resolver.compiler: ghc-*}" $ \o -> do
    compiler <- o J..: "compiler" <|> (o J..: "resolver" >>= (J..: "compiler"))
    pure RawSnapshotGHC {..}

getSnapshotGHC :: (MonadUnliftIO m, MonadThrow m) => SnapshotName -> m Version
getSnapshotGHC snap = do
  src <- fmap responseBody . httpLbs =<< parseUrlThrow (snapshotYamlUrl snap)
  RawSnapshotGHC compiler <- Y.decodeThrow $ LBS.toStrict src

  ver <-
    maybe
      (throwString $ "Invalid compiler: " <> compiler)
      pure
      (L.stripPrefix "ghc-" compiler)
  case [x | (x, "") <- readP_to_S parseVersion ver] of
    [x] -> pure x
    _ -> throwString $ "Invalid version: " <> ver
