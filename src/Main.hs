{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Options

import           Control.Monad
import           Data.IntMap.Strict        (IntMap)
import qualified Data.IntMap.Strict        as IntMap
import           Data.IntSet               (IntSet)
import qualified Data.IntSet               as IntSet
import           Data.List                 (intercalate)
import           Data.Maybe                (mapMaybe)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.IO              as TIO
import           Data.Text.Prettyprint.Doc (pretty)
import           Nix                       hiding (parse)
import           Text.Megaparsec


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
    Failure err -> print err
    Success quotes | null quotes -> putStrLn "  No quotes needed"
    Success quotes -> do
      _ <- flip IntMap.traverseWithKey quotes $ \line columns ->
        putStrLn $ "  Inserting quotes on line " ++ show line ++
          " at columns " ++ intercalate ", " (show <$> IntSet.toAscList columns)
      let newContent = insertQuotes content quotes
      TIO.writeFile path newContent

-- | A map from line numbers to the set of columns it needs quotes inserted at
type QuotePositions = IntMap IntSet

-- | A parser for a Nix expression that returns source spans where tabs correspond to a single character,
oneWideTabParser :: Parser NExprLoc
oneWideTabParser = do
  updateParserState $ \old -> old { statePosState = (statePosState old) { pstateTabWidth = pos1 } }
  whiteSpace *> nixToplevelForm <* eof

-- | Parses a nix file and returns all quotes that need to be inserted
quotesToInsert :: FilePath -> Text -> Result QuotePositions
quotesToInsert path content = either
  (Failure . pretty . errorBundlePretty)
  (Success . findUnquotedStrings)
  (parse oneWideTabParser path content)

-- | Checks whether a string expression is quoted or not
isUnquoted :: NString NExprLoc -> SrcSpan -> Bool
-- Indented strings can never be unquoted
isUnquoted (Indented _ _) _ = False
isUnquoted (DoubleQuoted list) sp = case list of
  -- Unquoted strings must have exactly one non-antiquoted, plain element
  [Plain text] -> textLength == spanLength where
    -- hnix doesn't directly allow us to differentiate between quoted and unquoted
    -- strings, but we can work around it by comparing the string length with
    -- the span of it, which will only be equal if there aren't any quotes
    textLength = Text.length text
    spanLength = unPos (sourceColumn (spanEnd sp)) - unPos (sourceColumn (spanBegin sp))
  _ -> False

-- | Inserts quotes into the input text according to the given quote positions
insertQuotes :: Text -> QuotePositions -> Text
insertQuotes original quotes = Text.unlines newLines where
  originalLines = Text.lines original
  newLines = uncurry newLine <$> zip [0..] originalLines
  newLine i old = maybe old (insertQuotesLine old) $ IntMap.lookup i quotes

-- | Inserts quotes in a single line at the positions given in the IntMap
insertQuotesLine :: Text -> IntSet -> Text
insertQuotesLine line quotes = Text.intercalate "\"" parts where
  -- Fold over our quote positions from the right, splitting the string wherever a quote
  -- needs to go and accumulating those results. Finally the rest string gets added to the
  -- results as well
  parts = uncurry (:) $ IntSet.foldr' fun (line, []) quotes
  fun pos (rest, result) = (before, after : result) where
    (before, after) = Text.splitAt pos rest

-- | Finds all unquoted strings in a parsed Nix expression and returns the positions
-- the string needs quotes at
findUnquotedStrings :: NExprLoc -> QuotePositions
findUnquotedStrings (AnnE sp (NStr str))
  | isUnquoted str sp = IntMap.singleton line $ IntSet.fromAscList [ firstQuote, secondQuote ]
  | otherwise = IntMap.empty
  where
    -- Subtract 1 because positions in megaparsec are 1-indexed
    line = unPos (sourceLine (spanBegin sp)) - 1
    firstQuote = unPos (sourceColumn (spanBegin sp)) - 1
    secondQuote = unPos (sourceColumn (spanEnd sp)) - 1
findUnquotedStrings (AnnE _ val) = let
    inBinding (NamedVar _ expr _) = findUnquotedStrings expr
    inBinding Inherit {}          = IntMap.empty
  in case val of
    (NList vals) -> IntMap.unions $ map findUnquotedStrings vals
    (NSet bindings) -> IntMap.unions $ map inBinding bindings
    (NRecSet bindings) -> IntMap.unions $ map inBinding bindings
    (NUnary _ expr) -> findUnquotedStrings expr
    (NBinary _ left right) -> findUnquotedStrings left `IntMap.union` findUnquotedStrings right
    (NSelect left _ Nothing) -> findUnquotedStrings left
    (NSelect left _ (Just right)) -> findUnquotedStrings left `IntMap.union` findUnquotedStrings right
    (NAbs params expr) -> findUnquotedStrings expr `IntMap.union` paramStrings where
      paramStrings = case params of
        ParamSet set _ _ -> IntMap.unions $ map findUnquotedStrings $ mapMaybe snd set
        _                -> IntMap.empty
    (NLet bindings expr) -> findUnquotedStrings expr `IntMap.union` IntMap.unions (map inBinding bindings)
    (NIf cond left right) -> findUnquotedStrings cond `IntMap.union` findUnquotedStrings left `IntMap.union` findUnquotedStrings right
    (NWith left right) -> findUnquotedStrings left `IntMap.union` findUnquotedStrings right
    (NAssert left right) -> findUnquotedStrings left `IntMap.union` findUnquotedStrings right
    _ -> IntMap.empty
findUnquotedStrings _ = IntMap.empty

