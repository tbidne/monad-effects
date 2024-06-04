{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- ORMOLU_DISABLE -}

-- | Provides the 'MonadTerminal' typeclass.
--
-- @since 0.1
module Effects.System.Terminal
  ( -- * Effect
    MonadTerminal (..),

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

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Effects.Exception (addCS, throwCS)
import GHC.IO.Exception
  ( IOErrorType (SystemError),
    IOException
      ( IOError,
        ioe_description,
        ioe_errno,
        ioe_filename,
        ioe_handle,
        ioe_location,
        ioe_type
      ),
  )
import GHC.Natural (Natural)
import GHC.Stack (HasCallStack)
import System.Console.Pretty qualified as CPretty
import System.Console.Terminal.Size (Window (Window, height, width), size)
import System.IO qualified as IO
import Prelude
  ( Applicative (pure),
    Bool,
    Char,
    IO,
    Integral,
    Maybe (Just, Nothing),
    Monad ((>>=)),
    Show (show),
    String,
    ($),
    (.),
    (<$>),
    (<>),
  )

-- Explicit prelude because of IO clashes.

{- ORMOLU_DISABLE -}

-- | Represents a terminal.
--
-- @since 0.1
class Monad m => MonadTerminal m where
  -- | Lifted 'IO.putStr'.
  --
  -- @since 0.1
  putStr :: HasCallStack => String -> m ()

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
  getTerminalSize :: (HasCallStack, Integral a) => m (Window a)

  -- | Lifted 'CPretty.supportsPretty'.
  --
  -- @since 0.1
  supportsPretty :: HasCallStack => m Bool

#if MIN_VERSION_base(4,15,0)
  {-# MINIMAL putStr, putBinary, getChar, getLine, getContents', getTerminalSize, supportsPretty #-}
#else
  {-# MINIMAL putStr , putBinary, getChar, getLine, getTerminalSize, supportsPretty #-}
#endif

-- | @since 0.1
instance MonadTerminal IO where
  putStr = addCS . IO.putStr
  {-# INLINEABLE putStr #-}
  putStrLn = addCS . IO.putStrLn
  {-# INLINEABLE putStrLn #-}
  putBinary = addCS . BS.putStr
  {-# INLINEABLE putBinary #-}
  getChar = addCS IO.getChar
  {-# INLINEABLE getChar #-}
  getLine = addCS IO.getLine
  {-# INLINEABLE getLine #-}
#if MIN_VERSION_base(4,15,0)
  getContents' = addCS IO.getContents'
  {-# INLINEABLE getContents' #-}
#endif
  getTerminalSize =
    size >>= \case
      Just h -> pure h
      Nothing -> throwCS $
        IOError
          { ioe_handle = Nothing,
            ioe_type = SystemError,
            ioe_location = "getTerminalSize",
            ioe_description = "Failed to detect the terminal size",
            ioe_errno = Nothing,
            ioe_filename = Nothing
          }
  {-# INLINEABLE getTerminalSize #-}

  supportsPretty = CPretty.supportsPretty
  {-# INLINEABLE supportsPretty #-}

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
  supportsPretty = lift supportsPretty
  {-# INLINEABLE supportsPretty #-}

{- ORMOLU_ENABLE -}

-- | 'putStrLn' and 'show'.
--
-- @since 0.1
print :: (HasCallStack, MonadTerminal m, Show a) => a -> m ()
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
getTerminalWidth :: (HasCallStack, Integral a, MonadTerminal m) => m a
getTerminalWidth = width <$> getTerminalSize
{-# INLINEABLE getTerminalWidth #-}

-- | Retrieves the terminal height.
--
-- @since 0.1
getTerminalHeight :: (HasCallStack, Integral a, MonadTerminal m) => m a
getTerminalHeight = height <$> getTerminalSize
{-# INLINEABLE getTerminalHeight #-}
