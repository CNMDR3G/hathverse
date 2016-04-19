{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
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
import Control.Exception
import Data.Functor

data CheckResult = CheckResult {
    ok :: Bool
  , output :: String
  } deriving Generic

instance ToJSON CheckResult

 -- | Well, stolen from 99haskell...
check :: Problem -> String -> IO CheckResult
check Problem{..} code =
    bracket prepareCheck cleanupResource performCheck
  where
    prepareCheck :: IO (String, FilePath, Either IOException _)
    prepareCheck = do
      submissionId <- ('h':) . show . hashUnique <$> newUnique
      homeDir <- getHomeDirectory
      let tmp = if os == "linux" then "/tmp" else homeDir
          dir = tmp ++ "/hathverse/" ++ submissionId

      createDirectoryIfMissing True dir
      writeFile (dir ++ "/" ++ T.unpack problemModuleName ++ ".hs") code
      writeFile (dir ++ "/check.hs") $ T.unpack problemCheckProgram

      let dockerArgs =
            [ "run"
            , "--name=" ++ submissionId
            , "--interactive=true"
            , "--net=none" -- no network
            -- TODO: limit CPU/memory
            , "--volume=" ++ dir ++ ":/mnt"
            , "scturtle/hathverse:alpine" -- image name
            ]
      eResult <- try $
         createProcess
           (proc "docker" dockerArgs)
             { std_in = CreatePipe
             , std_out = CreatePipe
             , std_err = CreatePipe }
      pure (submissionId, dir, eResult)

    cleanupResource :: _ -> IO ()
    cleanupResource (submissionId, dir, eResult) = do
      case eResult of
        Right (Just hIn, Just hOut, Just hErr, hProc) -> do
          -- closing a handler more than once is not considered an error,
          -- while this function does not know which handler has been closed,
          -- let's just close them all.
          mapM_ hClose [hIn, hOut, hErr]
          terminateProcess hProc
        _ -> pure ()
      void $ async $ do
        result <- try $ readProcess "docker" ["rm", "-f", submissionId] ""
        case result of
          Left (e :: IOException) -> putStrLn $ "error while cleaning up: " ++ show e
          _ -> pure ()
        removeDirectoryRecursive dir

    performCheck :: _ -> IO CheckResult
    performCheck (_, _, Left e) = do
      let errorMsg = "server side failure: " ++ show (e :: IOException)
      putStrLn errorMsg
      pure $ CheckResult False errorMsg
    performCheck (_, _, Right handlers) = do
      let (Just hin, Just hout, _, _) = handlers
      mapM_ (hPutStrLn hin)
        [ "cd /tmp"
        , "cp /mnt/* ."
        , "ghc -Wall -O2 check.hs 2>&1"
        , "./check 2>&1"
        ]
      hClose hin
      timeout (60 * 1000000) (Strict.hGetContents hout) >>= pure .
        \case
           Nothing -> CheckResult False "Timeout."
           Just output ->
             let ok | (not . null) output
                    , [_, _, "0", "failures"] <- words . last . lines $ output
                    = True
                    | otherwise = False
             in CheckResult ok output
