{-|
Module: Sudoku.Solver
Description : Provides functions to solve a sudoku.
Copyright: (c) Marcel Moosbrugger, 2017
License     : MIT

This module provides functions for solving a sudoku.
-}
module Sudoku.Solver (solveSudoku) where

import           Sudoku.Type
import           Util

-- Internal data types
-- -------------------

-- | Internal type representing a two dimensional matrix.
type Matrix a = [[a]]
-- | Internal type representing a sudoku board.
type Board    = Matrix Char
-- | Internal type represinting the choices for a single field in the sudoku.
type Choices  = [Char]


-- Helper functions
-- ----------------

-- | Is true iff a character represents an empty field.
blank :: Char -> Bool
blank = (==) blankval

-- | Splits a list into multiple list of length boxsize.
group :: [a] -> [[a]]
group = groupBy boxsize

-- Selection functions
-- -------------------

-- | Converts a matrix arranged in rows to a matrix arranged in rows.
rows :: Matrix a -> Matrix a
rows = id

-- | Converts a matrix arranged in rows to a matrix arranged in columns.
cols :: Matrix a -> Matrix a
cols [xs]       = [[x] | x <- xs]
cols (xs : xss) = zipWith (:) xs (cols xss)

-- | Converts a matrix arranged in rows to a matrix arranged in boxes.
boxs :: Matrix a -> Matrix a
boxs = map ungroup . ungroup . map cols . group . map group


-- Functions for generating choices and pruning
-- -------------------------------------------

-- | For a given board associates all possible (really all possible values)
--   with every blank field.
choices :: Board -> Matrix Choices
choices = map (map choose)
    where choose e = if blank e then cellvals else [e]

-- |For a list of choices returns those choices which are fixed
-- i.e. the choices which leave only a single choice
fixed :: [Choices] -> Choices
fixed = concat . filter single

-- |Reduces every choices in a list by the fixed choices
-- Imagine the parameter being the choices for every field in a single row
-- The function removes the fixed values from the choices of every field in the row
reduce :: [Choices] -> [Choices]
reduce css = map (remove (fixed css)) css
    where remove fs cs = if single cs then cs else delete fs cs

-- | Applies reduce to every unit in the matrix. Regarding to which units the
--   matrix gets reduced depends on the first parameter with which the matrix can
--   get transformed.
pruneBy :: (Matrix Choices -> Matrix Choices) -> (Matrix Choices -> Matrix Choices)
pruneBy f = f . map reduce . f

-- | Prunes a matrix by pruning it by rows, columns and boxes.
prune :: Matrix Choices -> Matrix Choices
prune = pruneBy boxs . pruneBy cols . pruneBy rows


-- Validity functions for choices
-- ------------------------------

-- | Is true if any field in the matrix has no choices
--   That would mean the resulting sudoku has no solution.
void :: Matrix Choices -> Bool
void = any (any null)

-- | Is true if no unit in the matrix as duplicated fixed elements.
--   That would mean the the resulting sudoku sultion is invalid.
safe :: Matrix Choices -> Bool
safe cm = all (nodups . fixed) (rows cm) &&
          all (nodups . fixed) (cols cm) &&
          all (nodups . fixed) (boxs cm)

-- | Is true if a matrix of choices is either void or not safe
--   That would mean that with the passed matrix of choices no solution can be
--   found anymore.
blocked :: Matrix Choices -> Bool
blocked cm = void cm || not (safe cm)


-- Functions for expanding and searching for a solution
-- ----------------------------------------------------

-- | Is the minimum number of choices a not fixed field in the matrix has
minchoice :: Matrix Choices -> Int
minchoice = minimum . filter (> 1). concat . map (map length)

-- | Out of a given matrix it creates a list of matrices by fixating every
--   possible choice for one field. The field for which the choices get fixated is
--   the first field with the minimum number of choices.
expand :: Matrix Choices -> [Matrix Choices]
expand cm = [rows1 ++ [row1 ++ [c]:row2]  ++ rows2 | c <- cs]
    where (rows1, row:rows2) = break (any best) cm
          (row1, cs:row2)    = break best row
          best cs            = (length cs == n)
          n                  = minchoice cm

-- | Searches all possible solutions. A solution is a a matrix of choices in which
--   every field has only a single choice. If a matrix is blocked there is no
--   solution. If its already a solution it's the only solution. The non trivial
--   case is handled by expanding the matrix, pruning all children and then
--   recursivly search within them for solutions.
search :: Matrix Choices -> [Matrix Choices]
search cm
    | blocked cm          = []
    | all (all single) cm = [cm]
    | otherwise           = (concat . map (search . prune) . expand) cm

-- | Solving a soduku is now just generating all choices, searching for solutions
--   for this choices and then converting the list of matrices of choices back to
--   a list of matrices of characters (or boards). Pruning the choices before
--   passing them on to the search improves the performance.
solve :: Board -> [Board]
solve = map (map $ map head) . search . prune . choices

-- | Returns a list of all solutions for a given sudoku.
solveSudoku :: Sudoku -> Maybe [Sudoku]
solveSudoku = sequence . map fromString . map concat . solve . groupBy boardsize . toString

