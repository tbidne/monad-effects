{-# LANGUAGE CPP #-}

-- | Provides the 'MonadEnv' typeclass.
--
-- @since 0.1
module Effects.System.Environment
  ( -- * Effect
    MonadEnv (..),
  )
where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Effects.Exception (addCS)
import GHC.Stack (HasCallStack)
import System.Environment qualified as Env

{- ORMOLU_DISABLE -}

-- | Environment effects.
--
-- @since 0.1
class Monad m => MonadEnv m where
  -- | Computation 'getArgs' returns a list of the program's command
  -- line arguments (not including the program name).
  --
  -- @since 0.1
  getArgs :: HasCallStack => m [String]

  -- | Computation 'getProgName' returns the name of the program as it was
  -- invoked.
  --
  -- However, this is hard-to-impossible to implement on some non-Unix
  -- OSes, so instead, for maximum portability, we just return the leafname
  -- of the program as invoked. Even then there are some differences
  -- between platforms: on Windows, for example, a program invoked as foo
  -- is probably really @FOO.EXE@, and that is what 'getProgName' will return.
  --
  -- @since 0.1
  getProgName :: HasCallStack => m String

#if MIN_VERSION_base(4,17,0)
  -- | Get an action to query the absolute pathname of the current executable.
  --
  -- If the operating system provides a reliable way to determine the current
  -- executable, return the query action, otherwise return @Nothing@. The action
  -- is defined on FreeBSD, Linux, MacOS, NetBSD, and Windows.
  --
  -- Even where the query action is defined, there may be situations where no
  -- result is available, e.g. if the executable file was deleted while the
  -- program is running. Therefore the result of the query action is a @Maybe
  -- FilePath@.
  --
  -- Note that for scripts and interactive sessions, the result is the path to
  -- the interpreter (e.g. ghci.)
  --
  -- @since 0.1
  executablePath :: HasCallStack => Maybe (m (Maybe FilePath))
#endif

  -- | Returns the absolute pathname of the current executable,
  -- or @argv[0]@ if the operating system does not provide a reliable
  -- way query the current executable.
  --
  -- Note that for scripts and interactive sessions, this is the path to
  -- the interpreter (e.g. ghci.)
  --
  -- Since base 4.11.0.0, 'getExecutablePath' resolves symlinks on Windows.
  -- If an executable is launched through a symlink, 'getExecutablePath'
  -- returns the absolute path of the original executable.
  --
  -- If the executable has been deleted, behaviour is ill-defined and
  -- varies by operating system. See 'executablePath' for a more
  -- reliable way to query the current executable.
  --
  -- @since 0.1
  getExecutablePath :: HasCallStack => m FilePath
  -- | Computation 'getEnv' @var@ returns the value
  -- of the environment variable @var@. For the inverse, the
  -- `System.Environment.setEnv` function can be used.
  --
  -- This computation may fail with:
  --
  --  * 'System.IO.Error.isDoesNotExistError' if the environment variable
  --    does not exist.

  --
  -- @since 0.1
  getEnv :: HasCallStack => String -> m String
  -- | Return the value of the environment variable @var@, or @Nothing@ if
  -- there is no such value.
  --
  -- For POSIX users, this is equivalent to 'System.Posix.Env.getEnv'.
  --
  -- @since 0.1
  lookupEnv :: HasCallStack => String -> m (Maybe String)
  -- | @setEnv name value@ sets the specified environment variable to @value@.
  --
  -- Early versions of this function operated under the mistaken belief that
  -- setting an environment variable to the /empty string/ on Windows removes
  -- that environment variable from the environment. For the sake of
  -- compatibility, it adopted that behavior on POSIX. In particular
  --
  -- @
  -- setEnv name \"\"
  -- @
  --
  -- has the same effect as
  --
  -- @
  -- `unsetEnv` name
  -- @
  --
  -- If you'd like to be able to set environment variables to blank strings,
  -- use `System.Environment.Blank.setEnv`.
  --
  -- Throws `Control.Exception.IOException` if @name@ is the empty string or
  -- contains an equals sign.
  --
  -- @since 0.1
  setEnv :: HasCallStack => String -> String -> m ()
  -- | @unsetEnv name@ removes the specified environment variable from the
  -- environment of the current process.
  --
  -- Throws `Control.Exception.IOException` if @name@ is the empty string or
  -- contains an equals sign.
  --
  -- @since 0.1
  unsetEnv :: HasCallStack => String -> m ()
  -- | 'withArgs' @args act@ - while executing action @act@, have 'getArgs'
  -- return @args@.
  --
  -- @since 0.1
  withArgs :: HasCallStack => [String] -> m a -> m a
  -- | 'withProgName' @name act@ - while executing action @act@,
  -- have 'getProgName' return @name@.
  --
  -- @since 0.1
  withProgName :: HasCallStack => String -> m a -> m a
  -- | 'getEnvironment' retrieves the entire environment as a
  -- list of @(key,value)@ pairs.
  --
  -- If an environment entry does not contain an @\'=\'@ character,
  -- the @key@ is the whole entry and the @value@ is the empty string.
  --
  -- @since 0.1
  getEnvironment :: HasCallStack => m [(String, String)]

-- | @since 0.1
instance MonadEnv IO where
  getArgs = addCS Env.getArgs
  {-# INLINEABLE getArgs #-}
  getProgName = addCS Env.getProgName
  {-# INLINEABLE getProgName #-}
#if MIN_VERSION_base(4,17,0)
  executablePath = fmap addCS Env.executablePath
  {-# INLINEABLE executablePath #-}
#endif
  getExecutablePath = addCS Env.getExecutablePath
  {-# INLINEABLE getExecutablePath #-}
  getEnv = addCS . Env.getEnv
  {-# INLINEABLE getEnv #-}
  lookupEnv = addCS . Env.lookupEnv
  {-# INLINEABLE lookupEnv #-}
  setEnv x = addCS . Env.setEnv x
  {-# INLINEABLE setEnv #-}
  unsetEnv = addCS . Env.unsetEnv
  {-# INLINEABLE unsetEnv #-}
  withArgs x = addCS . Env.withArgs x
  {-# INLINEABLE withArgs #-}
  withProgName x = addCS . Env.withProgName x
  {-# INLINEABLE withProgName #-}
  getEnvironment = addCS Env.getEnvironment
  {-# INLINEABLE getEnvironment #-}

-- | @since 0.1
instance MonadEnv m => MonadEnv (ReaderT env m) where
  getArgs = lift getArgs
  {-# INLINEABLE getArgs #-}
  getProgName = lift getProgName
  {-# INLINEABLE getProgName #-}
#if MIN_VERSION_base(4,17,0)
  executablePath = fmap lift executablePath
  {-# INLINEABLE executablePath #-}
#endif
  getExecutablePath = lift getExecutablePath
  {-# INLINEABLE getExecutablePath #-}
  getEnv = lift . getEnv
  {-# INLINEABLE getEnv #-}
  lookupEnv = lift . lookupEnv
  {-# INLINEABLE lookupEnv #-}
  setEnv x = lift . setEnv x
  {-# INLINEABLE setEnv #-}
  unsetEnv = lift . unsetEnv
  {-# INLINEABLE unsetEnv #-}
  withArgs x m = ask >>= \e -> lift (withArgs x (runReaderT m e))
  {-# INLINEABLE withArgs #-}
  withProgName x m = ask >>= \e -> lift (withProgName x (runReaderT m e))
  {-# INLINEABLE withProgName #-}
  getEnvironment = lift getEnvironment
  {-# INLINEABLE getEnvironment #-}

{- ORMOLU_ENABLE -}
