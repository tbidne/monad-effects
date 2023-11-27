{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Effects.FileSystem.PathReader qualified as PR
import Effects.FileSystem.Utils (OsPath, combineFilePaths, osp, (</>))
import Effects.System.PosixCompat qualified as PC
import Test.Tasty.Bench
  ( bench,
    bgroup,
    defaultMain,
    nfIO,
  )

main :: IO ()
main =
  defaultMain
    [ getPathType
    ]
  where
    getPathType =
      bgroup
        "getPathType"
        [ getFile,
          getDir,
          getFileLink,
          getDirLink
        ]

    getFile =
      bgroup
        "get file"
        [ bench "PathReader: get file" $ nfIO $ PR.getPathType fileOs,
          bench "PosixCompat: get file" $ nfIO $ PC.getPathType fileFp
        ]

    getDir =
      bgroup
        "get dir"
        [ bench "PathReader: get dir" $ nfIO $ PR.getPathType dirOs,
          bench "PosixCompat: get dir" $ nfIO $ PC.getPathType dirFp
        ]

    getFileLink =
      bgroup
        "get file link"
        [ bench "PathReader: get file link" $ nfIO $ PR.getPathType fileLinkOs,
          bench "PosixCompat: get file link" $ nfIO $ PC.getPathType fileLinkFp
        ]

    getDirLink =
      bgroup
        "get dir link"
        [ bench "PathReader: get dir link" $ nfIO $ PR.getPathType dirLinkOs,
          bench "PosixCompat: get dir link" $ nfIO $ PC.getPathType dirLinkFp
        ]

fileFp :: FilePath
fileFp = "bench" `cfp` "data" `cfp` "file"

fileOs :: OsPath
fileOs = [osp|bench|] </> [osp|data|] </> [osp|file|]

dirFp :: FilePath
dirFp = "bench" `cfp` "data" `cfp` "dir"

dirOs :: OsPath
dirOs = [osp|bench|] </> [osp|data|] </> [osp|dir|]

fileLinkFp :: FilePath
fileLinkFp = "bench" `cfp` "data" `cfp` "file-link"

fileLinkOs :: OsPath
fileLinkOs = [osp|bench|] </> [osp|data|] </> [osp|file-link|]

dirLinkFp :: FilePath
dirLinkFp = "bench" `cfp` "data" `cfp` "dir-link"

dirLinkOs :: OsPath
dirLinkOs = [osp|bench|] </> [osp|data|] </> [osp|dir-link|]

cfp :: FilePath -> FilePath -> FilePath
cfp = combineFilePaths
