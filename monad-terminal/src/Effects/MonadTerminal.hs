{-# LANGUAGE CPP #-}

-- | Provides the 'MonadTerminal' typeclass.
--
-- @since 0.1
module Effects.MonadTerminal
  ( -- * Class
    MonadTerminal (..),
    TermSizeException (..),

    -- * Functions

    -- ** Text
    putText,
    putTextLn,
    getTextLine,
#if MIN_VERSION_base(4,15,0)
    getTextContents',
#endif

    -- ** Window
    getTerminalWidth,
    getTerminalHeight,

    -- * Reexports
    Natural,
    Window (..),
    Text,
  )
where

import Control.Exception ( Exception(displayException) )
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT)
import Data.Text (Text)
import Data.Text qualified as T
import Effects.MonadCallStack
  ( addCallStack,
    throwWithCallStack,
  )
import GHC.Natural (Natural)
import GHC.Stack (HasCallStack)
import System.Console.Terminal.Size (Window (..), size)
import System.IO qualified as IO
import Prelude hiding (getChar, getLine, putStr, putStrLn)

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
  {-# INLINEABLE displayException #-}

-- | Represents a terminal.
--
-- @since 0.1
class Monad m => MonadTerminal m where
  -- | Simple print function.
  --
  -- @since 0.1
  putStr :: HasCallStack => String -> m ()

  -- | Simple print function with newline.
  --
  -- @since 0.1
  putStrLn :: HasCallStack => String -> m ()
  putStrLn = putStr . (<> "\n")
  {-# INLINEABLE putStrLn #-}

  -- | Retrieves a 'Char'.
  --
  -- @since 0.1
  getChar :: HasCallStack => m Char

  -- | @since 0.1
  getLine :: HasCallStack => m String

#if MIN_VERSION_base(4,15,0)
  -- | @since 0.1
  getContents' :: HasCallStack => m String
#endif

  -- | Retrieves the terminal size.
  --
  -- @since 0.1
  getTerminalSize :: HasCallStack => m (Window Natural)

-- | @since 0.1
instance MonadTerminal IO where
  putStr = addCallStack . IO.putStr
  {-# INLINEABLE putStr #-}
  putStrLn = addCallStack . IO.putStrLn
  {-# INLINEABLE putStrLn #-}
  getChar = addCallStack IO.getChar
  {-# INLINEABLE getChar #-}
  getLine = addCallStack IO.getLine
  {-# INLINEABLE getLine #-}
#if MIN_VERSION_base(4,15,0)
  getContents' = addCallStack IO.getContents'
  {-# INLINEABLE getContents' #-}
#endif
  getTerminalSize =
    size >>= \case
      Just h -> pure h
      Nothing -> throwWithCallStack MkTermSizeException
  {-# INLINEABLE getTerminalSize #-}

-- | @since 0.1
instance MonadTerminal m => MonadTerminal (ReaderT e m) where
  putStr = lift . putStr
  {-# INLINEABLE putStr #-}
  putStrLn = lift . putStrLn
  {-# INLINEABLE putStrLn #-}
  getChar = lift getChar
  {-# INLINEABLE getChar #-}
  getLine = lift getLine
  {-# INLINEABLE getLine #-}
#if MIN_VERSION_base(4,15,0)
  getContents' = lift getContents'
  {-# INLINEABLE getContents' #-}
#endif
  getTerminalSize = lift getTerminalSize
  {-# INLINEABLE getTerminalSize #-}

-- | 'Text' version of 'putStr'.
--
-- @since 0.1
putText :: (HasCallStack, MonadTerminal m) => Text -> m ()
putText = putStr . T.unpack
{-# INLINEABLE putText #-}

-- | 'Text' version of 'putStrLn'.
--
-- @since 0.1
putTextLn :: (HasCallStack, MonadTerminal m) => Text -> m ()
putTextLn = putStrLn . T.unpack
{-# INLINEABLE putTextLn #-}

-- | @since 0.1
getTextLine :: (HasCallStack, MonadTerminal m) => m Text
getTextLine = T.pack <$> getLine
{-# INLINEABLE getTextLine #-}

#if MIN_VERSION_base(4,15,0)
-- | @since 0.1
getTextContents' :: (HasCallStack, MonadTerminal m) => m Text
getTextContents' = T.pack <$> getContents'
{-# INLINEABLE getTextContents' #-}
#endif

-- | Retrieves the terminal width.
--
-- @since 0.1
getTerminalWidth :: (HasCallStack, MonadTerminal m) => m Natural
getTerminalWidth = width <$> getTerminalSize
{-# INLINEABLE getTerminalWidth #-}

-- | Retrieves the terminal height.
--
-- @since 0.1
getTerminalHeight :: (HasCallStack, MonadTerminal m) => m Natural
getTerminalHeight = height <$> getTerminalSize
{-# INLINEABLE getTerminalHeight #-}
