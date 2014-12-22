#!/opt/ghc/7.8.1/bin/runghc

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Monad
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import           Prelude hiding (FilePath)
import           Shelly
import           System.Environment

import Common

main :: IO ()
main = do
    dir0:refs <- getArgs >>= \case
        [] -> fail "usage: submodchecker <git-dir> [<commit-id>+]"
        x  -> return x

    let dir = fromText (T.pack dir0)

    shelly $ forM_ (map T.pack refs) $ \ref -> do
      (cid,deltas) <- gitDiffTree dir ref

      let smDeltas = [ (smPath, smCid) | (_, (GitTypeGitLink, smCid), smPath) <- deltas ]

      unless (null smDeltas) $ do
          echo $ "Submodule update(s) detected in " <> cid <> ":"

          (_, msg) <- gitCatCommit dir cid

          unless ("submodule" `T.isInfixOf` msg) $ do
              echo "*FAIL* commit message does not contain magic 'submodule' word"
              quietExit 1

          modMapping <- getModules dir ref
          forM_ smDeltas $ \(smPath,smCid) -> do
              echo $ " " <> smPath <> " => " <> smCid
              (smUrl,_) <- maybe (fail "failed to lookup repo-url") return $
                           lookup smPath modMapping

              if not ("." `T.isPrefixOf` smUrl)
               then echo $ "skipping non-relative Git url (" <> smUrl <> ")"
               else do
                  branches <- gitBranchesContain (dir </> smUrl) smCid

                  let branches' = filter (not . ("wip/" `T.isPrefixOf`)) branches
                  when (null branches') $ do
                      echo $ "*FAIL* commit not found in submodule repo ('" <> smUrl <> "')"
                      echo   "       or not reachable from persistent branches"
                      quietExit 1

                  return ()

          echo " OK"
