module Sudoku.SolverSpec (main, spec) where

import           Test.Hspec
import           Util

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- -- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Sample test" $ do
        it "The logical laws should not be fucked up" $ do
            True `shouldBe` True

