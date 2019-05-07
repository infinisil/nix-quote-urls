{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Lib
import           Options

import           Control.Monad
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet        as IntSet
import           Data.List          (intercalate)
import qualified Data.Text.IO       as TIO
import           System.Directory
import           System.FilePath

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
  Options { optPaths } <- getOptions
  files <- concat <$> traverse nixFiles optPaths
  forM_ files processFile

-- | Quotes all unquoted urls in a single file
processFile :: FilePath -> IO ()
processFile path = do
  putStrLn $ "Processing file " ++ path
  content <- TIO.readFile path
  case quotesToInsert path content of
    Left err -> putStrLn err
    Right (QuotePositions quotes) | null quotes -> putStrLn "  No quotes needed"
    Right (QuotePositions quotes) -> do
      _ <- flip IntMap.traverseWithKey quotes $ \line columns ->
        putStrLn $ "  Inserting quotes on line " ++ show line ++
          " at columns " ++ intercalate ", " (show <$> IntSet.toAscList columns)
      let newContent = insertQuotes content (QuotePositions quotes)
      TIO.writeFile path newContent

