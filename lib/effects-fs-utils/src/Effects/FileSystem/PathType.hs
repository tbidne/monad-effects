module Effects.FileSystem.PathType
  ( PathType (..),
    displayPathType,
  )
where

import Control.DeepSeq (NFData)
import Data.String (IsString)
import GHC.Generics (Generic)

-- | Path type.
--
-- @since 0.1
data PathType
  = -- | @since 0.1
    PathTypeFile
  | -- | @since 0.1
    PathTypeDirectory
  | -- | @since 0.1
    PathTypeSymbolicLink
  | -- | @since 0.1
    PathTypeOther
  deriving stock
    ( -- | @since 0.1
      Bounded,
      -- | @since 0.1
      Enum,
      -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Ord,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | String representation of 'PathType'.
--
-- @since 0.1
displayPathType :: (IsString a) => PathType -> a
displayPathType PathTypeFile = "file"
displayPathType PathTypeDirectory = "directory"
displayPathType PathTypeSymbolicLink = "symlink"
displayPathType PathTypeOther = "other"
