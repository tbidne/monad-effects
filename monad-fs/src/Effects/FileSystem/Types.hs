{-# LANGUAGE CPP #-}

-- | Provides general FileSystem types.
--
-- @since 0.1
module Effects.FileSystem.Types
  ( Path,
  )
where

#if MIN_VERSION_directory(1,3,8)
import System.Directory.OsPath (OsPath)

-- | For @directory >= 1.3.8@, 'Path' = 'OsPath'. Below that it is a
-- 'FilePath'.
--
-- @since 0.1
type Path = OsPath
#else
-- | For @directory >= 1.3.8@, 'Path' = 'System.Directory.OsPath.OsPath'.
-- Below that it is a 'FilePath'.
--
-- @since 0.1
type Path = FilePath
#endif
