-- | Provides the 'MonadTerminal' typeclass.
--
-- @since 0.1
module Effects.MonadTerminal
  ( -- * Class
    MonadTerminal (..),

    -- * Functions
    getTerminalWidth,
    getTerminalHeight,

    -- * Reexports
    Window (..),
  )
where

import Control.Exception
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT)
import Data.Text (Text)
import Data.Text qualified as T
import Effects.MonadCallStack
  ( MonadCallStack,
    checkpointCallStack,
    throwWithCallStack,
  )
import GHC.Natural (Natural)
import GHC.Stack (HasCallStack)
import System.Console.Terminal.Size (Window (..), size)
import System.IO qualified as IO
import Prelude hiding (getChar)

-- | @since 0.1
data TermSizeException = MkTermSizeException
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception TermSizeException where
  displayException = const "Failed to detect the terminal size."

-- | Represents a terminal.
--
-- @since 0.1
class Monad m => MonadTerminal m where
  -- | Simple print function.
  --
  -- @since 0.1
  putText :: HasCallStack => Text -> m ()

  -- | Simple print function with newline.
  --
  -- @since 0.1
  putTextLn :: HasCallStack => Text -> m ()
  putTextLn = putText . (<> "\n")

  -- | Retrieves a 'Char'.
  --
  -- @since 0.1
  getChar :: HasCallStack => m Char

  -- | Retrieves the terminal size.
  --
  -- @since 0.1
  getTerminalSize :: HasCallStack => m (Window Natural)

-- | @since 0.1
instance MonadTerminal IO where
  putText = checkpointCallStack . putStr . T.unpack
  putTextLn = checkpointCallStack . putStrLn . T.unpack
  getChar = IO.getChar
  getTerminalSize =
    size >>= \case
      Just h -> pure h
      Nothing -> throwWithCallStack MkTermSizeException

-- | @since 0.1
instance MonadTerminal m => MonadTerminal (ReaderT e m) where
  putText = lift . putText
  putTextLn = lift . putTextLn
  getChar = lift getChar
  getTerminalSize = lift getTerminalSize

-- | Retrieves the terminal width.
--
-- @since 0.1
getTerminalWidth :: (HasCallStack, MonadCallStack m, MonadTerminal m) => m Natural
getTerminalWidth = checkpointCallStack (width <$> getTerminalSize)

-- | Retrieves the terminal height.
--
-- @since 0.1
getTerminalHeight :: (HasCallStack, MonadCallStack m, MonadTerminal m) => m Natural
getTerminalHeight = checkpointCallStack (height <$> getTerminalSize)
