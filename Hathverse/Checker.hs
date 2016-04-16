{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module Hathverse.Checker where

import GHC.Generics (Generic)
import qualified Data.Text as T
import System.Process
import System.Timeout
import System.IO
import System.Info
import qualified System.IO.Strict as Strict
import Control.Concurrent.Async
import Data.Unique
import Data.Aeson
import System.Directory
import Hathverse.Db

data CheckResult = CheckResult {
    ok :: Bool
  , output :: String
  } deriving Generic

instance ToJSON CheckResult

-- | Well, stolen from 99haskell...
check :: Problem -> String -> IO CheckResult
check Problem{..} code = do

  submissionId <- ('h':) . show . hashUnique <$> newUnique
  homeDir <- getHomeDirectory
  let tmp = if os == "linux" then "/tmp" else homeDir
      dir = tmp ++ "/hathverse/" ++ submissionId

  createDirectoryIfMissing True dir
  writeFile (dir ++ "/" ++ T.unpack problemModuleName ++ ".hs") code
  writeFile (dir ++ "/check.hs") $ T.unpack problemCheckProgram

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
    , "ghc -Wall -O2 check.hs 2>&1"
    , "./check 2>&1"
    ]
  hClose hin

  maybeOutput <- timeout (60 * 1000000) (Strict.hGetContents hout)

  let (output, ok) =
        case maybeOutput of
          Nothing ->
            ("Timeout.", False)
          Just output ->
            let ok = (not . null) output &&
                  case words . last . lines $ output of
                    [_, _, "0", "failures"] -> True
                    _ -> False
            in (output, ok)

  hClose hout
  hClose herr
  terminateProcess hproc

  _ <- async $ do
    _ <- readProcess "docker" ["rm", "-f", submissionId] ""
    removeDirectoryRecursive dir

  return CheckResult { ok=ok, output=output }
