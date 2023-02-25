{-# LANGUAGE CPP #-}

-- | Provides compatibility for filepaths.
--
-- @since 0.1
module Effects.FileSystem.Path
  ( -- * Path
    Path,
    (</>),
  )
where

import Effects.FileSystem.Internal (Path, (</>))
