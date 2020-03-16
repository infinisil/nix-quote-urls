{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Lib
import           Options

import           Data.Monoid
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet        as IntSet
import           Data.List          (intercalate)
import qualified Data.Text.IO       as TIO
import           System.Directory
import           System.FilePath
import           System.Exit

-- | Returns all Nix files for a path, recursing into directories
nixFiles :: FilePath -> IO [FilePath]
nixFiles path = do
  isDir <- doesDirectoryExist path
  case (isDir, takeExtension path) of
    (False, ".nix") -> return [path]
    (False, _) -> return []
    (True, _) -> do
      contents <- fmap (path </>) <$> listDirectory path
      concat <$> traverse nixFiles contents

-- | Quotes all unquoted urls in the files given as arguments
main :: IO ()
main = do
  Options { optPaths, optMode } <- getOptions
  files <- concat <$> traverse nixFiles optPaths
  All success <- mconcat <$> traverse (processFile optMode) files
  exitWith $ if success then ExitSuccess else ExitFailure 1

-- | Quotes all unquoted urls in a single file
processFile :: Mode -> FilePath -> IO All
processFile mode path = do
  putStrLn $ "Processing file " ++ path
  content <- TIO.readFile path
  case quotesToInsert path content of
    Left err -> do
      putStrLn err
      return (All False)
    Right (QuotePositions quotes) | null quotes -> do
                                      putStrLn "  No quotes needed"
                                      return (All True)
    Right (QuotePositions quotes) -> case mode of
      Fixup -> do
        _ <- flip IntMap.traverseWithKey quotes $ \line columns ->
          putStrLn $ "  Inserting quotes on line " ++ show line ++
            " at columns " ++ intercalate ", " (show <$> IntSet.toAscList columns)
        let newContent = insertQuotes content (QuotePositions quotes)
        TIO.writeFile path newContent
        return (All True)
      Verify | IntMap.null quotes -> return (All True)
      Verify -> do
        _ <- flip IntMap.traverseWithKey quotes $ \line columns ->
          putStrLn $ "  Has unquoted urls on line " ++ show line ++
            " at columns " ++ intercalate ", " (show <$> IntSet.toAscList columns)
        return (All False)

