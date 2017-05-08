module UtilSpec (main, spec) where

import           Test.Hspec
import           Test.QuickCheck
import           Util

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- -- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "nodups" $ do
        it "should return true for the empty list" $ do
            nodups ([] :: [Int]) `shouldBe` True

        it "should return true for lists with no duplicates" $ do
            nodups [1,2,3,4,5] `shouldBe` True

        it "should return false for lists with duplicates" $ do
            nodups [1,1,3,4,5] `shouldBe` False

    describe "groupBy" $ do
        it "should return the empty list if given an empty list" $ property $
            \n -> groupBy n ([] :: [Int]) == ([] :: [[Int]])

        it "should create the right amount of groups" $ do
            (length $ groupBy 2 [1,2,3,4,5]) `shouldBe` 3

        it "should create the right group sizes" $ do
          (and . init . map ((== 2) . length) $ groupBy 2 [1,2,3,4,5]) `shouldBe` True

    describe "ungroup" $ do
        it "should return the empty list if given an empty list" $ property $
            ungroup ([] :: [[Int]]) == ([] :: [Int])

        it "should contain the same elements" $ do
            ungroup [[1,2], [3,4], [5]] `shouldBe` [1,2,3,4,5]

    describe "single" $ do
        it "should be true if and only the list contains one element" $ property $
            \xs -> single (xs :: [Int]) == (length xs == 1)

    describe "delete" $ do
        it "should not contain the deleted elements" $ do
            delete [3,4] [1,2,3,4,5] `shouldBe` [1,2,5]

