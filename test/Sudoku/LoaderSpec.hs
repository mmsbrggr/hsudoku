module Sudoku.LoaderSpec (main, spec) where

import           Data.Foldable (forM_)
--import           Data.Maybe
--import           Sudoku.Loader
import           Sudoku.Type
import           Test.Hspec

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- -- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "loadSudoku" $ do
        it "should actually load and return a sudoku" $ forM_ [Easy ..] $ \d -> do
            d `shouldSatisfy` (`elem` [Easy ..])
            --maybeSudoku <- loadSudoku d
            --isJust maybeSudoku `shouldBe` True

