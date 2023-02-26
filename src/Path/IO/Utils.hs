module Path.IO.Utils (makeSureEmpty) where

import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO)
import Path (Dir, Path, toFilePath)
import Path.IO
import RIO (HasCallStack, throwString)

makeSureEmpty :: (MonadIO m, HasCallStack) => Path b Dir -> m ()
makeSureEmpty dest = do
  there <- doesDirExist dest
  when there $ do
    (ls, rs) <- listDir dest
    unless (null ls && null rs) $
      throwString $
        "Target directory not empty: " <> toFilePath dest
