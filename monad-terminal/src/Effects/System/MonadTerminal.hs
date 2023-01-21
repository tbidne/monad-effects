{-# LANGUAGE CPP #-}

{- ORMOLU_DISABLE -}

-- | Provides the 'MonadTerminal' typeclass.
--
-- @since 0.1
module Effects.System.MonadTerminal
  ( -- * Effect
    MonadTerminal (..),
    TermSizeException (..),

    -- * Functions
    print,

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

{- ORMOLU_ENABLE -}

import Control.Exception (Exception (displayException))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Data.Text (Text)
import Data.Text qualified as T
import Effects.Exception
  ( addCallStack,
    throwWithCallStack,
  )
import GHC.Natural (Natural)
import GHC.Stack (HasCallStack)
import System.Console.Terminal.Size (Window (..), size)
import System.IO qualified as IO
import Prelude hiding (getChar, getLine, print, putStr, putStrLn)

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

{- ORMOLU_DISABLE -}

-- | Represents a terminal.
--
-- @since 0.1
class Monad m => MonadTerminal m where
  -- | Lifted 'IO.putStr'.
  --
  -- @since 0.1
  putStr :: HasCallStack => String -> m ()
  putStr = putBinary . Char8.pack
  {-# INLINEABLE putStr #-}

  -- | Lifted 'IO.putStrLn'.
  --
  -- @since 0.1
  putStrLn :: HasCallStack => String -> m ()
  putStrLn = putStr . (<> "\n")
  {-# INLINEABLE putStrLn #-}

  -- | Lifted 'BS.putStr'.
  --
  -- @since 0.1
  putBinary :: HasCallStack => ByteString -> m ()
  putBinary = putStr . Char8.unpack
  {-# INLINEABLE putBinary #-}

  -- | Lifted 'IO.getChar'.
  --
  -- @since 0.1
  getChar :: HasCallStack => m Char

  -- | Lifted 'IO.getLine'.
  --
  -- @since 0.1
  getLine :: HasCallStack => m String

#if MIN_VERSION_base(4,15,0)
  -- | Lifted 'IO.getContents''.
  --
  -- @since 0.1
  getContents' :: HasCallStack => m String
#endif

  -- | Retrieves the terminal size.
  --
  -- @since 0.1
  getTerminalSize :: HasCallStack => m (Window Natural)

#if MIN_VERSION_base(4,15,0)
  {-# MINIMAL (putStr | putBinary), getChar, getLine, getContents', getTerminalSize #-}
#else
  {-# MINIMAL (putStr | putBinary), getChar, getLine, getTerminalSize #-}
#endif

-- | @since 0.1
instance MonadTerminal IO where
  putStr = addCallStack . IO.putStr
  {-# INLINEABLE putStr #-}
  putStrLn = addCallStack . IO.putStrLn
  {-# INLINEABLE putStrLn #-}
  putBinary = addCallStack . BS.putStr
  {-# INLINEABLE putBinary #-}
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
  putBinary = lift . putBinary
  {-# INLINEABLE putBinary #-}
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

{- ORMOLU_ENABLE -}

-- | Lifted 'IO.print'.
--
-- @since 0.1
print :: (HasCallStack, MonadTerminal m) => String -> m ()
print = putStrLn . show
{-# INLINEABLE print #-}

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

-- | 'Text' version of 'getLine'.
--
-- @since 0.1
getTextLine :: (HasCallStack, MonadTerminal m) => m Text
getTextLine = T.pack <$> getLine
{-# INLINEABLE getTextLine #-}

#if MIN_VERSION_base(4,15,0)
-- | 'Text' version of 'getContents''.
--
-- @since 0.1
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
