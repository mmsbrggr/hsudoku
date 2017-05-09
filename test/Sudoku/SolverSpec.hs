module Sudoku.SolverSpec (main, spec) where

import           Data.Maybe      (fromJust, isJust)
import           Sudoku.Solver
import           Sudoku.Type
import           Test.Hspec
import           Test.QuickCheck
import           TestData        ()

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- -- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "solveSudoku" $ do
        it "should return something" $ property $
            \s -> isJust $ solveSudoku s

        it "should not contain blank values" $ property $
            \s -> let solutionStrings = map toString (fromJust $ solveSudoku s)
                  in and $ map (all (/= blankval)) solutionStrings


