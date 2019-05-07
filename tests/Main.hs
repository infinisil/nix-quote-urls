{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import qualified Data.IntMap                  as IntMap
import qualified Data.IntSet                  as IntSet
import           Data.String.Interpolate
import           Data.String.Interpolate.Util
import           Data.Text                    (pack)
import           Lib
import           Nix
import           Test.Hspec

nix :: String -> NExprLoc
nix text = case parseNix "<string>" (pack text) of
  Right expr -> expr
  Left err   -> error err

nix' :: String -> NExprLoc
nix' = nix . unindent

q :: Int -> Int -> Int -> QuotePositions
q line columnStart columnEnd = q' line columnStart <> q' line columnEnd

q' :: Int -> Int -> QuotePositions
q' line column = QuotePositions
  $ IntMap.singleton line
  $ IntSet.singleton column

main :: IO ()
main = hspec $ do
  describe "findUnquotedStrings" $ do
    context "quote detection" $ do
      it "A standalone url" $
        findUnquotedStrings (nix [i|x://y|]) `shouldBe` q 0 0 5

      it "An url with spacing" $
        findUnquotedStrings (nix [i|  x://y  |]) `shouldBe` q 0 2 7

      it "An url after a tab" $
        findUnquotedStrings (nix ('\t':[i|x://y|])) `shouldBe` q 0 1 6

      it "An url in an attribute set" $
        findUnquotedStrings (nix [i|{ x = x://y; }|]) `shouldBe` q 0 6 11

      it "Multiple urls on a single line" $
        findUnquotedStrings (nix [i|[ x://y x://y ]|]) `shouldBe` q 0 2 7 <> q 0 8 13

      it "Multiple urls on multiple lines" $
        findUnquotedStrings (nix' [i|
                                     [x://y
                                     x://y]
                                    |]) `shouldBe` q 0 1 6 <> q 1 0 5

      it "In default function arguments" $
        findUnquotedStrings (nix [i|{ x ? x://y }: x|]) `shouldBe` q 0 6 11

      it "In ? operation" $
        findUnquotedStrings (nix [i|{ x = x://y; } ? x|]) `shouldBe` q 0 6 11

      it "In antiquotation" $
        findUnquotedStrings (nix [i|"${x://y}"|]) `shouldBe` q 0 3 8


    context "quote non-detection" $ do
      it "In a single comment" $
        findUnquotedStrings (nix [i|null # x://y|]) `shouldBe` mempty

      it "In a multiline comment" $
        findUnquotedStrings (nix' [i|
                                     null
                                     /*
                                       x://y
                                     */
                                    |]) `shouldBe` mempty

      it "In escaped antiquotation in normal strings" $
        -- interpolate quasiquote needs an escaped \, see https://github.com/sol/interpolate/issues/11
        findUnquotedStrings (nix [i|"\\${x://y}"|]) `shouldBe` mempty

      it "In escaped antiquotation in multiline strings" $
        findUnquotedStrings (nix [i|''''${x://y}''|]) `shouldBe` mempty

      it "In normal strings" $
        findUnquotedStrings (nix [i|"x://y"|]) `shouldBe` mempty

      it "In multiline strings" $
        findUnquotedStrings (nix [i|
                                    ''
                                      x://y
                                    ''
                                   |]) `shouldBe` mempty

  describe "insertQuotes" $
    context "Inserting quotes into" $ do
      it "empty lines" $
        insertQuotes "" (q 0 0 0) `shouldBe` "\""

      it "long lines" $
        insertQuotes "hello this is a very long line with lots of words" (q 0 0 5 <> q 0 6 10 <> q 0 14 20)
          `shouldBe` "\"hello\" \"this\" is \"a very\" long line with lots of words"

      it "multiple lines" $
        insertQuotes "line 0\nline 1\nline 2" (q 0 0 6 <> q 1 1 5 <> q 2 2 4)
          `shouldBe` "\"line 0\"\nl\"ine \"1\nli\"ne\" 2"


