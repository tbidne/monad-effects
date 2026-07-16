-- | Provides the MonadHandleRW effect, for opening files with read and
-- write abilities.
--
-- @since 0.1
module Effects.FileSystem.HandleRW
  ( -- * Effect
    MonadHandleRW (..),
    Handle,
    HandleMode (HandleModeReadWrite),

    -- * Reexports
    ByteString,
    OsPath,
  )
where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.ByteString (ByteString)
import Effects.FileSystem.Handle.Internal
  ( Handle (MkHandle),
    HandleMode (HandleModeReadWrite),
  )
import Effects.FileSystem.HandleReader (MonadHandleReader)
import Effects.FileSystem.HandleWriter (MonadHandleWriter)
import FileSystem.IO qualified as FS.IO
import FileSystem.OsPath (OsPath)
import GHC.Stack (HasCallStack)
import System.IO (IOMode (ReadWriteMode))

-- | Represents handle read and write effects.
--
-- @since 0.1
class (MonadHandleReader m, MonadHandleWriter m) => MonadHandleRW m where
  -- | Opens a file in ReadWrite mode.
  --
  -- @since 0.1
  openBinaryFile :: (HasCallStack) => OsPath -> m (Handle HandleModeReadWrite)

  -- | Opens a file in ReadWrite mode.
  --
  -- @since 0.1
  withBinaryFile :: (HasCallStack) => OsPath -> (Handle HandleModeReadWrite -> m a) -> m a

-- | @since 0.1
instance MonadHandleRW IO where
  openBinaryFile p = MkHandle <$> FS.IO.openBinaryFileIO p ReadWriteMode
  {-# INLINEABLE openBinaryFile #-}
  withBinaryFile p onHandle =
    FS.IO.withBinaryFileIO p ReadWriteMode (onHandle . MkHandle)
  {-# INLINEABLE withBinaryFile #-}

-- | @since 0.1
instance (MonadHandleRW m) => MonadHandleRW (ReaderT e m) where
  openBinaryFile = lift . openBinaryFile
  {-# INLINEABLE openBinaryFile #-}
  withBinaryFile p f = ask >>= lift . \e -> withBinaryFile p ((`runReaderT` e) . f)
  {-# INLINEABLE withBinaryFile #-}
