#!/opt/ghc/7.8.1/bin/runghc

{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.DeepSeq
import           Control.Monad
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Prelude hiding (FilePath)
import           Shelly
import           System.Environment

main :: IO ()
main = do
    dir0:refs <- getArgs
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

-- | Run @git@ operation
runGit :: FilePath -> Text -> [Text] -> Sh Text
runGit d op args = do
    d' <- toTextWarn d
    silently $ run "git" ("--git-dir=" <> d' : op : args)

gitCatCommit :: FilePath -> Text -> Sh (Text,Text)
gitCatCommit d ref = do
    tmp <- runGit d "cat-file" ["commit", ref ]
    return (fmap (T.drop 2) $ T.breakOn "\n\n" tmp)

-- | wrapper around @git branch --contains@
gitBranchesContain :: FilePath -> Text -> Sh [Text]
gitBranchesContain d ref = do
    tmp <- liftM T.lines $
           errExit False $ print_stderr False $
           runGit d "branch" ["--contains", ref]

    unless (all (\s -> T.take 2 s `elem` ["  ","* "]) tmp) $
        fail "gitBranchesContain: internal error"

    return $!! map (T.drop 2) tmp

-- | returns @[(path, (url, key))]@
--
-- may throw exception
getModules :: FilePath -> Text -> Sh [(Text, (Text, Text))]
getModules d ref = do
    tmp <- runGit d "show" [ref <> ":.gitmodules"]

    setStdin tmp
    res <- liftM T.lines $ runGit d "config" [ "--file", "/dev/stdin", "-l" ]

    let ms  = [ (T.tail key1,(key2, T.tail val))
              | r <- res, "submodule." `T.isPrefixOf` r
              , let (key,val) = T.break (=='=') r
              , let (key',key2) = T.breakOnEnd "." key
              , let (_,key1) = T.break (=='.') (T.init key')
              ]

        ms' = [ (path', (url, k))
              | es@((k,_):_) <- groupBy ((==) `on` fst) ms
              , let props = map snd es
              , let url = fromMaybe (error "getModules1") (lookup "url" props)
              , let path' = fromMaybe (error "getModules2") (lookup "path" props)
              ]

    return $!! ms'


gitDiffTree :: FilePath -> Text -> Sh (Text, [([(GitType, Text, Char)], (GitType, Text), Text)])
gitDiffTree d ref = do
    tmp <- liftM T.lines $ runGit d "diff-tree" ["--root","-c", "-r", ref]
    case tmp of
        cid:deltas -> return $!! (cid, map parseDtLine deltas)
        []         -> return ("", [])

  where
    parseDtLine :: Text -> ([(GitType, Text, Char)], (GitType, Text), Text)
    parseDtLine l
      | sanityCheck = force (zip3 (map cvtMode mode') oid' (T.unpack k),(cvtMode mode,oid),fp)
      | otherwise = error "in parseDtLine"
      where
        sanityCheck = n > 0 && T.length k == n

        n = T.length cols
        (mode',mode:tmp') = splitAt n $ T.split (==' ') l''
        (oid',[oid,k]) = splitAt n tmp'
        [l'',fp] = T.split (=='\t') l'
        (cols,l') = T.span (==':') l

z40 :: Text
z40 = T.pack (replicate 40 '0')

data GitType
    = GitTypeVoid
    | GitTypeRegFile
    | GitTypeExeFile
    | GitTypeTree
    | GitTypeSymLink
    | GitTypeGitLink
    deriving (Show,Eq,Ord,Enum)

instance NFData GitType

cvtMode :: Text -> GitType
cvtMode "000000" = GitTypeVoid
cvtMode "040000" = GitTypeSymLink
cvtMode "100644" = GitTypeRegFile
cvtMode "100755" = GitTypeExeFile
cvtMode "120000" = GitTypeSymLink
cvtMode "160000" = GitTypeGitLink
cvtMode x = error ("cvtMode: " ++ show x)
