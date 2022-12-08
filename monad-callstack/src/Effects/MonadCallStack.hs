-- | Provides an effect for handling exceptions with callstacks.
--
-- @since 0.1
module Effects.MonadCallStack
  ( -- * Class
    MonadCallStack (..),

    -- * Utils
    displayCallStack,

    -- * Reexports
    CallStack,
    HasCallStack,
    AnnotatedException (..),
    Annotation (..),
    Ann.throw,
    Ann.try,
    Ann.catch,
  )
where

import Control.Exception.Annotated.UnliftIO
  ( AnnotatedException (..),
    Annotation (..),
    Exception (displayException, fromException, toException),
    SomeException,
  )
import Control.Exception.Annotated.UnliftIO qualified as Ann
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT (ReaderT), ask)
import Data.Foldable (Foldable (foldMap'))
import Data.Typeable (cast)
import GHC.Stack (CallStack, HasCallStack, callStack, prettyCallStack)

-- | Typeclass for 'CallStack' effects. The 'IO' instance uses the machinery
-- from @annotated-exception@. Note that this means the try/catch/etc.
-- function from that package should be used, if you are using the 'IO'
-- instance.
--
-- @since 0.1
class Monad m => MonadCallStack m where
  -- | Retrieves the 'CallStack'.
  --
  -- @since 0.1
  getCallStack :: HasCallStack => m CallStack

  -- | Throws an exception with the 'CallStack'.
  --
  -- @since 0.1
  throwWithCallStack :: (Exception e, HasCallStack) => e -> m a

  -- | Adds the 'CallStack' to any thrown exceptions.
  --
  -- @since 0.1
  checkpointCallStack :: HasCallStack => m a -> m a

-- | @since 0.1
instance MonadCallStack IO where
  getCallStack = pure callStack
  throwWithCallStack = Ann.throwWithCallStack
  checkpointCallStack = Ann.checkpointCallStack

-- | @since 0.1
instance MonadCallStack m => MonadCallStack (ReaderT e m) where
  getCallStack = lift getCallStack
  throwWithCallStack = lift . throwWithCallStack
  checkpointCallStack (ReaderT r) = ask >>= lift . checkpointCallStack . r

-- | Like 'displayException', except it has extra logic that attempts to
-- display any found 'CallStack's in a pretty way.
--
-- @since 0.1
displayCallStack :: forall e. Exception e => e -> String
displayCallStack ex =
  case fromException @(AnnotatedException SomeException) (toException ex) of
    Nothing -> displayException ex
    Just (AnnotatedException anns anEx) ->
      mconcat
        [ displayException anEx,
          foldMap' (\a -> "\n" <> prettyAnn a) anns
        ]
  where
    prettyAnn :: Annotation -> String
    prettyAnn (Annotation x) = case cast x of
      Just cs -> prettyCallStack cs
      Nothing -> show x
