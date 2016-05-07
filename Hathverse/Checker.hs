{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}
module Hathverse.Checker where

import GHC.Generics (Generic)
import System.IO
import System.Timeout (timeout)
import System.Process
import System.Directory
import System.Environment (lookupEnv)
import qualified System.IO.Strict as Strict
import Data.Unique (hashUnique, newUnique)
import Data.Aeson (ToJSON)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Control.Monad
import Control.Exception
import Control.Concurrent.Async
import Hathverse.Db

data CheckResult = CheckResult {
    ok :: Bool
  , output :: String
  } deriving Generic

instance ToJSON CheckResult

check :: Problem -> String -> IO CheckResult
check Problem{..} code =
    bracket prepareCheck cleanupResource performCheck
  where
    prepareCheck :: IO (String, FilePath, Either IOException _, Bool)
    prepareCheck = do
      submissionId <- ('h':) . show . hashUnique <$> newUnique
      tempdir <- getTemporaryDirectory

      createDirectoryIfMissing True tempdir
      writeFile (tempdir ++ "/" ++ T.unpack problemModuleName ++ ".hs") code
      writeFile (tempdir ++ "/check.hs") $ T.unpack problemCheckProgram

      let dockerArgs =
            [ "run"
            , "--name=" ++ submissionId
            , "--interactive=true"
            , "--net=none" -- no network
            -- TODO: limit CPU/memory
            , "--volume=" ++ tempdir ++ ":/mnt"
            , "scturtle/hathverse:alpine" -- image name
            ]

      runInDocker <- isJust <$> lookupEnv "RUNINDOCKER"

      eResult <- try $
         createProcess
           (if runInDocker
               then proc "docker" dockerArgs
               else proc "/bin/sh" [])
             { std_in = CreatePipe
             , std_out = CreatePipe
             , std_err = CreatePipe }
      pure (submissionId, tempdir, eResult, runInDocker)

    cleanupResource :: _ -> IO ()
    cleanupResource (submissionId, tempdir, eResult, runInDocker) = do
      case eResult of
        Right (Just hIn, Just hOut, Just hErr, hProc) -> do
          -- closing a handler more than once is not considered an error,
          -- while this function does not know which handler has been closed,
          -- let's just close them all.
          err <- hGetContents hErr
          unless (null err) $
            putStrLn $ "Error in " ++ submissionId ++ ": " ++ err
          mapM_ hClose [hIn, hOut, hErr]
          terminateProcess hProc
        _ -> putStrLn $ "Error in terminating " ++ submissionId
      void $ async $ do
        when runInDocker $ do
          result <- try $ readProcess "docker" ["rm", "-f", submissionId] ""
          case result of
            Left (e :: IOException) -> putStrLn $ "error while cleaning up: " ++ show e
            _ -> pure ()
        removeDirectoryRecursive tempdir

    performCheck :: _ -> IO CheckResult
    performCheck (_, _, Left e, _) = do
      let errorMsg = "server side failure: " ++ show (e :: IOException)
      putStrLn errorMsg
      pure $ CheckResult False errorMsg
    performCheck (_, tempdir, Right handlers, runInDocker) = do
      let (Just hin, Just hout, _, _) = handlers
      mapM_ (hPutStrLn hin)
        [ if runInDocker
             then "cd /tmp && cp /mnt/* ."
             else "cd " ++ tempdir
        , (if runInDocker then "ghc " else "stack ghc -- ") ++
            "-Wall -O2 check.hs 2>&1 && ./check 2>&1"
        ]
      hClose hin

      maybeOutput <- timeout (60 * 1000000) (Strict.hGetContents hout)
      return $ case maybeOutput of
        Nothing -> CheckResult False "Timeout."
        Just output ->
          let ok | (not . null) output
                 , [_, _, "0", "failures"] <- words . last . lines $ output
                 = True
                 | otherwise = False
          in CheckResult ok output
