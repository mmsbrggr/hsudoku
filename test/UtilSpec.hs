module UtilSpec (main, spec) where

import           Data.List       (nub)
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

        it "should return true just for lists with no duplicates" $ property $
            \xs -> nodups (xs :: [Int]) == (length xs == (length $ nub xs))

    describe "groupBy" $ do
        it "should return the empty list if given an empty list" $ property $
            \n -> groupBy n ([] :: [Int]) `shouldBe` ([] :: [[Int]])

        it "should create the right amount of groups" $ property $
            \xs -> and $ map (\n ->
                                 let groups  = length $ groupBy n (xs :: [Int])
                                     predicted = case xs of
                                                      [] -> 0
                                                      _  -> if length xs `mod` n == 0
                                                              then length xs `div` n
                                                              else 1 + length xs `div` n
                                 in groups == predicted) [1..10]

        it "should create the right group sizes" $ property $
            \xs -> and $ map (\n ->
                                let sizes = map length $ groupBy n (xs :: [Int])
                                in all (<= n) sizes
                             ) [1..10]

    describe "ungroup" $ do
        it "should return the empty list if given an empty list" $ property $
            ungroup ([] :: [[Int]]) `shouldBe` ([] :: [Int])

        it "should contain the same elements" $ property $
            \xxs -> ungroup (xxs :: [[Int]]) == concat xxs

    describe "single" $ do
        it "should be true if and only the list contains one element" $ property $
            \xs -> single (xs :: [Int]) `shouldBe` (length xs == 1)

    describe "delete" $ do
        it "should not contain the deleted elements" $ property $
           \xs ys -> delete xs (xs ++ ys) == (ys :: [Int])

