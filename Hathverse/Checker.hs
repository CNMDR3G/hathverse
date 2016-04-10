{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Hathverse.Checker where

import Data.Text
import System.Process
import System.Timeout
import System.IO
import System.Info
import qualified System.IO.Strict as Strict
import Control.Concurrent.Async
import Data.Unique
import System.Directory
import Hathverse.Db

-- | Well, stolen from 99haskell...
check :: Problem -> String -> IO String
check Problem{..} code = do

  submissionId <- ('h':) . show . hashUnique <$> newUnique
  homeDir <- getHomeDirectory
  let tmp = if os == "linux" then "/tmp" else homeDir
      dir = tmp ++ "/hathverse/" ++ submissionId

  createDirectoryIfMissing True dir
  writeFile (dir ++ "/" ++ unpack problemModuleName ++ ".hs") code
  writeFile (dir ++ "/check.hs") $ unpack problemCheckProgram

  (Just hin, Just hout, Just herr, hproc) <-
    createProcess
      (proc "docker" [ "run"
                     , "--name=" ++ submissionId
                     , "--interactive=true"
                     , "--net=none" -- no network
                     -- TODO: limit CPU/memory
                     , "--volume=" ++ dir ++ ":/mnt"
                     , "scturtle/hathverse:alpine" -- image name
                     ])
      { std_in = CreatePipe , std_out = CreatePipe , std_err = CreatePipe }

  mapM_ (hPutStrLn hin)
    [ "cd /tmp"
    , "cp /mnt/* ."
    , "ghc -Wall -O2 check.hs"
    , "./check"
    ]
  hClose hin

  maybeOutput <- timeout (60 * 1000000) (Strict.hGetContents hout)

  -- TODO: better result analysis
  result <-
    case maybeOutput of
      Nothing -> return "Timeout."
      Just output -> do
        errors <- Strict.hGetContents herr
        return $ "STDERR:\n\n" ++ errors ++ "\n\nSTDOUT:\n\n" ++ output

  hClose hout
  hClose herr
  terminateProcess hproc

  _ <- async $ do
    _ <- readProcess "docker" ["rm", "-f", submissionId] ""
    removeDirectoryRecursive dir

  return result
