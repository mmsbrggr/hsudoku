module Sudoku.TypeSpec (main, spec) where

import           Data.Maybe      (fromJust, isJust)
import           Sudoku.Type
import           Test.Hspec
import           Test.QuickCheck
import           TestData
import           Util

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- -- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "toString" $ do
        it "should return a non empty string for a sudoku" $ property $
            \s -> length (toString s) > 0

    describe "fromString" $ do
        it "should not fail for already existing sudokus" $ property $
            \s -> isJust (fromString $ toString s)

        it "should be inverse to toString" $ property $
            \s -> (toString . fromJust . fromString . toString) s == toString s

