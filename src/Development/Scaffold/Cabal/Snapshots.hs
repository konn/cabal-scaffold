{-# LANGUAGE ApplicativeDo #-}
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
import qualified Data.Char as C
import Data.Coerce (coerce)
import Data.Functor ((<&>))
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Calendar
import GHC.Generics (Generic)
import Network.HTTP.Client.Conduit (HttpException (..), HttpExceptionContent (..), Request (..), Response (..), parseUrlThrow)
import Network.HTTP.Simple (httpLbs)
import Network.HTTP.Types (status304)
import RIO (Hashable, ReaderT (..), handle, throwIO, throwString, tshow)
import RIO.Time (defaultTimeLocale, formatTime)
import Text.Regex.Applicative.Text
import qualified Text.Regex.Applicative.Text as RE

data PartialSnapshotName
  = PartialLTS (Maybe (Int, Maybe Int))
  | PartialNightly (Maybe Day)
  deriving (Eq, Ord, Generic)
  deriving anyclass (Hashable)

parsePartialSnapsthot :: String -> Maybe PartialSnapshotName
parsePartialSnapsthot = RE.match partialSnapshotRE . T.pack

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
  fromJSONKey = J.FromJSONKeyTextParser (maybe (fail "partial name") pure . RE.match partialSnapshotRE)

instance FromJSON PartialSnapshotName where
  parseJSON =
    J.withText "snapshot name" $
      maybe (fail "lts-*-* or nightly-YYYY-mm-dd expected") pure
        . RE.match partialSnapshotRE
  {-# INLINE parseJSON #-}

instance ToJSON PartialSnapshotName where
  toJSON = J.toJSON . show
  {-# INLINE toJSON #-}

instance ToJSONKey PartialSnapshotName where
  toJSONKey = J.ToJSONKeyText (AK.fromText . tshow) (AE.text . tshow)

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
        . RE.match snapshotRE

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

snapshotRE :: RE.RE' SnapshotName
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
  fromJSONKey = J.FromJSONKeyTextParser (maybe (fail "name") pure . RE.match snapshotRE)

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
