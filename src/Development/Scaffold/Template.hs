{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}

{- |
Stack's project-template, extended with escape for @START_FILE@ and other headers
-}
module Development.Scaffold.Template () where

import qualified Control.Foldl as L
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.Functor.Of (Of (..))
import qualified Data.Strict.Either as S
import Data.Strict.Tuple
import qualified Data.Strict.Tuple as S
import qualified Data.Text.Encoding as TE
import qualified Streaming.ByteString as Q

validateUtf8L :: L.Fold BS.ByteString Bool
{-# INLINE validateUtf8L #-}
validateUtf8L =
  S.either (const False) . (==)
    <$> L.length
    <*> L.Fold
      ( S.either (const . S.Left) \(n :!: st) bs ->
          case TE.validateUtf8More st bs of
            (_, Nothing) -> S.Left False
            (m, Just st') -> S.Right ((n + m) :!: st')
      )
      (S.Right (0 :!: TE.startUtf8State))
      (fmap S.fst)

encodeBase64 :: (Monad m) => Q.ByteStream m r -> Q.ByteStream m r
encodeBase64 = Q.chunkMap B64.encode

decodeBase64 :: (Monad m) => Q.ByteStream m r -> Q.ByteStream m r
decodeBase64 = Q.chunkMap B64.decodeLenient

validateUtf8 :: (Monad m) => Q.ByteStream m r -> m (Of Bool r)
validateUtf8 = L.purely Q.chunkFold validateUtf8L
