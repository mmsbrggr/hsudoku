{-|
Module: Sudoku.Loader
Description : provides functions to load a sudoku from the network
Copyright: (c) Marcel Moosbrugger, 2017
License     : MIT

This module contains functions to load sudokus from the network
-}
module Sudoku.Loader(loadSudoku) where

import           Data.List         (elemIndex)
import           Data.Maybe        (fromMaybe)
import           Sudoku.Type
import           Text.HandsomeSoup
import           Text.Printf       (printf)
import           Text.XML.HXT.Core

-- |The url pattern of the site from which the sudokus get loaded
urlPattern :: String
urlPattern = "http://show.websudoku.com/?level=%d"

-- |Creates the url from the url pattern and the difficutlty level
url :: Difficulty -> String
url d = printf urlPattern $ param d
    where param :: Difficulty -> Int
          param d = fromMaybe 1 $ elemIndex d [Easy, Medium, Hard, Evil]

-- |Loads a sudoku from the network with a given difficulty level
loadSudoku :: Difficulty -> IO (Maybe Sudoku)
loadSudoku d = do
    let doc = fromUrl (url d)
    values <- runX $ doc >>> css "#puzzle_grid input" ! "value"
    let sudokuString = concat $ map (\v -> if v == "" then blankval:"" else v) values
    pure (fromString sudokuString)

