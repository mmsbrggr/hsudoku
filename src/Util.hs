{-|
Module: Util
Description : Provides generic utility functions on lists
Copyright: (c) Marcel Moosbrugger, 2017
License     : MIT

This modules provides generic utility functions for lists.
-}
module Util (
      nodups
    , groupBy
    , ungroup
    , single
    , delete
) where

import           Data.List ((\\))

-- | Is true iff a list has no duplicate elements.
nodups :: Eq a => [a] -> Bool
nodups []     = True
nodups (x:xs) = x `notElem` xs && nodups xs

-- | Splits a list into multiple lists of a given length.
groupBy :: Int -> [a] -> [[a]]
groupBy n [] = []
groupBy n xs = (take n xs) : groupBy n (drop n xs)

-- | The reverse operation of groupBy.
ungroup :: [[a]] -> [a]
ungroup = concat

-- | Is true iff a given list contains exactly one element.
single :: [a] -> Bool
single [a] = True
single _   = False

-- | Removes the elements of the first list from the second list.
delete :: Eq a => [a] -> [a] -> [a]
delete = flip (\\)

