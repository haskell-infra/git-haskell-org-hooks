#!/opt/ghc/7.8.1/bin/runghc

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Monad
import           Control.Monad.Writer
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import           Prelude hiding (FilePath)
import           Shelly
import           System.Environment

import           Common

hasSuffix :: Text -> Bool
hasSuffix fn = any (`T.isSuffixOf` fn) suffixes
  where
    suffixes = T.words ".hs .hsc .lhs .cabal .c .h .lhs-boot .hs-boot"

main :: IO ()
main = do
    dir0:refs <- getArgs >>= \case
        [] -> fail "usage: validate-whitespace <git-dir> [<commit-id>+]"
        x  -> return x

    let dir = fromText (T.pack dir0)

    stats <- shelly $ forM (map T.pack refs) $ \ref -> do
      (cid,deltas) <- gitDiffTree dir ref

      lintMsgs0 <- forM deltas $ \(origs, (gt, blobId), fname) -> if (gt == GitTypeRegFile && hasSuffix fname)
        then do
          let blobIds0 = [ b0 | (GitTypeRegFile, b0, _) <- origs, b0 /= z40 ]
          blob1  <- gitCatBlob dir blobId
          blobs0 <- mapM (gitCatBlob dir) blobIds0

          -- blobs0 will be empty in case in case of newly added files as well as renames/copies
          -- blobs0 will contain more than one entry for merge-commits

          return [ (fname, msg) | msg <- lintBlob blobs0 blob1 ]
        else return []

      let lintMsgs = concat lintMsgs0
          status = maximum (Nothing : [ Just lvl | (_, LintMsg lvl _ _ _) <- lintMsgs ])
          ok     = status < Just LintLvlErr

      unless (null lintMsgs) $ liftIO $ do
          putStrLn "====================================================================================="
          putStrLn ("commit " <> T.unpack cid <> " has whitespace linter issues:")
          putStrLn ""
          forM_ lintMsgs $ \(fn, LintMsg lvl lno l m) -> do
              let lvls = case lvl of
                      LintLvlErr  -> "*ERROR*"
                      LintLvlWarn -> "Warning"
              putStrLn (" " <> lvls <> " " <> T.unpack fn <> ":" <> show lno <> ": " <> T.unpack m)
              putStrLn (" > " <> show l)
              putStrLn ""
              return ()

      unless ok $ liftIO $
          putStrLn ("Validation FAILED for " <> T.unpack cid)

      return status

    unless (null $ filter isJust stats) $
        putStrLn "====================================================================================="

    let stats1 = maximum (Nothing : stats)

    -- unless (stats1 == Nothing) $ do
    --     putStrLn "There were commit message linter issues! For more information see"
    --     putStrLn ""

    unless (stats1 < Just LintLvlErr) $ do
        putStrLn "Validation FAILED because at least one commit had linter errors!"
        shelly $ quietExit 1

    putStrLn "whitespace validation passed!"


lintBlob :: [Text] -> Text -> [LintMsg]
lintBlob blobs0 blob1 = execWriter $ do
    -- Perform simple invariant-preservation checks
    when (hasTabs blob1 && not (any hasTabs blobs0)) $ do
        tell [ LintMsg LintLvlErr lno l "introduces TAB"
             | (lno,l) <- zip [1..] lns
             , "\t" `T.isInfixOf` l
             ]

    when (hasTrail blob1 && not (any hasTrail blobs0)) $ do
        tell [ LintMsg LintLvlErr lno l "introduces trailing whitespace"
             | (lno,l) <- zip [1..] lns, hasTrail l ]

    when (missingFinalEOL blob1) $ if not (any missingFinalEOL blobs0)
          then tell [LintMsg LintLvlErr  llno lln "lacking final EOL"]
          else tell [LintMsg LintLvlWarn llno lln "lacking final EOL"]

  where
    lns = T.lines blob1
    llno = length lns
    lln = case lns of
        [] -> ""
        _  -> last lns

    hasTabs :: Text -> Bool
    hasTabs = T.any (=='\t')

    hasTrail :: Text -> Bool
    hasTrail t = or [ " \n"    `T.isInfixOf`  t
                    , " \r\n"  `T.isInfixOf`  t
                    , "\t\r\n" `T.isInfixOf`  t
                    , " "      `T.isSuffixOf` t
                    , "\t"     `T.isSuffixOf` t
                    ]

    missingFinalEOL :: Text -> Bool
    missingFinalEOL = not . T.isSuffixOf "\n"

data LintMsg = LintMsg !LintLvl !Int !Text !Text
              deriving Show

data LintLvl = LintLvlWarn | LintLvlErr
              deriving (Show,Eq,Ord)
