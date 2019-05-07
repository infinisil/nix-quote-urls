{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Lib
import           Options

import           Control.Monad
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet        as IntSet
import           Data.List          (intercalate)
import qualified Data.Text.IO       as TIO


-- | Quotes all unquoted urls in the files given as arguments
main :: IO ()
main = do
  opts <- getOptions
  forM_ (optFiles opts) processFile

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

