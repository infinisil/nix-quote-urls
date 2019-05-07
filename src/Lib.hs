{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( QuotePositions(..)
  , quotesToInsert
  , insertQuotes
  , parseNix
  , findUnquotedStrings
  , spanToQuotes
  , Result(..)
  ) where

import           Data.Fix
import           Data.IntMap.Strict        (IntMap)
import qualified Data.IntMap.Strict        as IntMap
import           Data.IntSet               (IntSet)
import qualified Data.IntSet               as IntSet
import           Data.List                 (intercalate)
import           Data.Maybe                (mapMaybe)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.Text.Prettyprint.Doc (pretty)
import           Nix                       hiding (parse)
import           Text.Megaparsec


-- | A map from line numbers to the set of columns it needs quotes inserted at
newtype QuotePositions = QuotePositions (IntMap IntSet) deriving Eq

instance Semigroup QuotePositions where
  (QuotePositions left) <> (QuotePositions right) = QuotePositions $ IntMap.unionWith IntSet.union left right

instance Monoid QuotePositions where
  mempty = QuotePositions IntMap.empty

instance Show QuotePositions where
  show (QuotePositions quotes) = "QuotePositions " ++ intercalate ", " (map (uncurry fromColumns) (IntMap.toAscList quotes)) where
    fromColumns file columns = show file ++ ":{" ++ intercalate "," (map show (IntSet.toAscList columns)) ++ "}"

parseNix :: String -> Text -> Either String NExprLoc
parseNix path input = either
  (Left . show . pretty . errorBundlePretty)
  Right
  (parse oneWideTabParser path input)

-- | A parser for a Nix expression that returns source spans where tabs correspond to a single character,
oneWideTabParser :: Parser NExprLoc
oneWideTabParser = do
  updateParserState $ \old -> old { statePosState = (statePosState old) { pstateTabWidth = pos1 } }
  whiteSpace *> nixToplevelForm <* eof

-- | Parses a nix file and returns all quotes that need to be inserted
quotesToInsert :: FilePath -> Text -> Either String QuotePositions
quotesToInsert path content = findUnquotedStrings <$> parseNix path content

-- | Checks whether a string expression is quoted or not
isUnquoted :: NString a -> SrcSpan -> Bool
  -- Unquoted strings must have exactly one non-antiquoted, plain element
isUnquoted (DoubleQuoted [Plain text]) (SrcSpan begin end) = textLength == spanLength where
  -- hnix doesn't directly allow us to differentiate between quoted and unquoted
  -- strings, but we can work around it by comparing the string length with
  -- the span of it, which will only be equal if there aren't any quotes
  textLength = Text.length text
  spanLength = unPos (sourceColumn end) - unPos (sourceColumn begin)
isUnquoted _ _ = False

-- | Inserts quotes into the input text according to the given quote positions
insertQuotes :: Text -> QuotePositions -> Text
insertQuotes original (QuotePositions quotes) = Text.intercalate "\n" newLines where
  originalLines = Text.splitOn "\n" original
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

-- | Converts a span to a quote positions at the spans begin and end
spanToQuotes :: SrcSpan -> QuotePositions
spanToQuotes (SrcSpan begin end) = quoteAt begin <> quoteAt end
  where quoteAt (SourcePos _ line column) = QuotePositions $
          IntMap.singleton (unPos line - 1) (IntSet.singleton (unPos column - 1))

-- | Finds all unquoted strings in a parsed Nix expression and returns the positions
-- the string needs quotes at
findUnquotedStrings :: NExprLoc -> QuotePositions
findUnquotedStrings = cata $ \case
  Compose (Ann sp value) -> case value of
    NConstant _ -> mempty
    NStr str
      | isUnquoted str sp -> spanToQuotes sp
      | otherwise -> forString str
      where
        forString (DoubleQuoted list) = mconcat $ map forAntiquoted list
        forString (Indented _ list)   = mconcat $ map forAntiquoted list
        forAntiquoted (Antiquoted expr) = expr
        forAntiquoted _                 = mempty
    NSym _ -> mempty
    NList vals -> mconcat vals
    NSet bindings -> mconcat $ map inBinding bindings
    NRecSet bindings -> mconcat $ map inBinding bindings
    NLiteralPath _ -> mempty
    NEnvPath _  -> mempty
    NUnary _ expr -> expr
    NBinary _ left right -> left <> right
    NSelect left _ Nothing -> left
    NSelect left _ (Just right) -> left <> right
    NHasAttr expr _ -> expr
    NAbs (ParamSet set _ _) expr -> mconcat (mapMaybe snd set) <> expr
    NAbs _ expr -> expr
    NLet bindings expr -> mconcat (map inBinding bindings) <> expr
    NIf cond left right -> cond <> left <> right
    NWith left right -> left <> right
    NAssert left right -> left <> right
    NSynHole _ -> mempty
  where
    inBinding (NamedVar _ expr _) = expr
    inBinding Inherit {}          = mempty

