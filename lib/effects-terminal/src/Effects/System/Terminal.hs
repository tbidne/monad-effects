{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- HLINT ignore "Use putStrLn" -}

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
import Effects.Exception (throwM)
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
import System.Console.Pretty qualified as CPretty
import System.Console.Terminal.Size (Window (Window, height, width), size)
import System.IO qualified as IO
import Prelude
  ( Applicative (pure),
    Bool,
    Char,
    IO,
    Maybe (Just, Nothing),
    Monad ((>>=)),
    Show (show),
    String,
    ($),
    (++),
    (.),
    (<$>),
  )

-- Explicit prelude because of IO clashes.

{- ORMOLU_DISABLE -}

-- | Represents a terminal.
--
-- @since 0.1
class Monad m => MonadTerminal m where
  -- | Write a character to the standard output device
  -- (same as 'hPutChar' 'stdout').
  --
  -- @since 0.1
  putStr :: String -> m ()

  -- | The same as 'putStr', but adds a newline character.
  --
  -- @since 0.1
  putStrLn :: String -> m ()
  putStrLn = putStr . (++ "\n")
  {-# INLINEABLE putStrLn #-}

  -- | Write a ByteString to 'stdout'.
  --
  -- @since 0.1
  putBinary :: ByteString -> m ()

  -- | Read a character from the standard input device
  -- (same as 'hGetChar' 'stdin').
  --
  -- @since 0.1
  getChar :: m Char

  -- | Read a line from the standard input device
  -- (same as 'hGetLine' 'stdin').
  --
  -- @since 0.1
  getLine :: m String

#if MIN_VERSION_base(4,15,0)
  -- | The 'getContents'' operation returns all user input as a single string,
  -- which is fully read before being returned
  -- (same as 'hGetContents'' 'stdin').
  --
  -- @since 0.1
  getContents' :: m String
#endif

  -- | Retrieves the terminal size.
  --
  -- @since 0.1
  getTerminalSize :: m (Window Natural)

  -- | Determines if we support ANSI styling.
  --
  -- @since 0.1
  supportsPretty :: m Bool

#if MIN_VERSION_base(4,15,0)
  {-# MINIMAL putStr, putBinary, getChar, getLine, getContents', getTerminalSize, supportsPretty #-}
#else
  {-# MINIMAL putStr , putBinary, getChar, getLine, getTerminalSize, supportsPretty #-}
#endif

-- | @since 0.1
instance MonadTerminal IO where
  putStr = IO.putStr
  {-# INLINEABLE putStr #-}
  putStrLn = IO.putStrLn
  {-# INLINEABLE putStrLn #-}
  putBinary = BS.putStr
  {-# INLINEABLE putBinary #-}
  getChar = IO.getChar
  {-# INLINEABLE getChar #-}
  getLine = IO.getLine
  {-# INLINEABLE getLine #-}
#if MIN_VERSION_base(4,15,0)
  getContents' = IO.getContents'
  {-# INLINEABLE getContents' #-}
#endif
  getTerminalSize =
    size >>= \case
      Just h -> pure h
      Nothing -> throwM $
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

-- | The 'print' function outputs a value of any printable type to the
-- standard output device.
-- Printable types are those that are instances of class 'Show'; 'print'
-- converts values to strings for output using the 'show' operation and
-- adds a newline.
--
-- For example, a program to print the first 20 integers and their
-- powers of 2 could be written as:
--
-- > main = print ([(n, 2^n) | n <- [0..19]])
--
-- @since 0.1
print :: (MonadTerminal m, Show a) => a -> m ()
print = putStrLn . show
{-# INLINEABLE print #-}

-- | 'Text' version of 'putStr'.
--
-- @since 0.1
putText :: (MonadTerminal m) => Text -> m ()
putText = putStr . T.unpack
{-# INLINEABLE putText #-}

-- | 'Text' version of 'putStrLn'.
--
-- @since 0.1
putTextLn :: (MonadTerminal m) => Text -> m ()
putTextLn = putStrLn . T.unpack
{-# INLINEABLE putTextLn #-}

-- | 'Text' version of 'getLine'.
--
-- @since 0.1
getTextLine :: (MonadTerminal m) => m Text
getTextLine = T.pack <$> getLine
{-# INLINEABLE getTextLine #-}

#if MIN_VERSION_base(4,15,0)
-- | 'Text' version of 'getContents''.
--
-- @since 0.1
getTextContents' :: (MonadTerminal m) => m Text
getTextContents' = T.pack <$> getContents'
{-# INLINEABLE getTextContents' #-}
#endif

-- | Retrieves the terminal width.
--
-- @since 0.1
getTerminalWidth :: (MonadTerminal m) => m Natural
getTerminalWidth = width <$> getTerminalSize
{-# INLINEABLE getTerminalWidth #-}

-- | Retrieves the terminal height.
--
-- @since 0.1
getTerminalHeight :: (MonadTerminal m) => m Natural
getTerminalHeight = height <$> getTerminalSize
{-# INLINEABLE getTerminalHeight #-}
