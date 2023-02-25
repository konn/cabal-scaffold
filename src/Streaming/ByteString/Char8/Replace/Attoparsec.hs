module Streaming.ByteString.Char8.Replace.Attoparsec (replaceAll) where

import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Streaming as AS
import qualified Data.ByteString as BS
import Data.Function ((&))
import qualified Replace.Attoparsec.ByteString as Repl
import qualified Streaming.ByteString.Char8 as Q
import qualified Streaming.Prelude as S

replaceAll ::
  Monad m =>
  Parser BS.ByteString ->
  Q.ByteStream m x ->
  Q.ByteStream m x
replaceAll p q = Q.mwrap $ do
  (ebs, lo) <- AS.parse (Repl.sepCap p) q
  pure $ do
    case ebs of
      Left {} -> pure ()
      Right r -> r & S.each & S.map (either id id) & Q.fromChunks
    lo
