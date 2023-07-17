{-# LANGUAGE CPP #-}

-- | Convenient module for OsPath type.
--
-- @since 0.1
module Effects.FileSystem.Path
  ( -- * Path
    OsPath,
    (</>),
  )
where

import Effects.FileSystem.Internal (OsPath, (</>))
