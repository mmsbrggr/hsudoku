{-|
Module: Sudoku.Loader
Description : Provides functions to load a sudoku from the internet.
Copyright: (c) Marcel Moosbrugger, 2017
License     : MIT

This module contains functions to load sudokus from the internet.
-}
module Sudoku.Loader(loadSudoku) where

import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.List                  (elemIndex, transpose)
import           Data.Maybe                 (fromMaybe)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Sudoku.Type
import           Text.HandsomeSoup
import           Text.Printf                (printf)
import           Text.XML.HXT.Core
import           Util

-- | Converts a difficulty to number which can be plugged into the url.
toNumber :: Difficulty -> Int
toNumber Easy   = 40
toNumber Medium = 33
toNumber Hard   = 26
toNumber Evil   = 17

-- | Creates the url for a given difficullty level from which the sudokus get
--   loaded.
url :: Difficulty -> String
url d = "https://kjell.haxx.se/sudoku/?visade=" ++ (show $ toNumber d) ++ "&seed=%28random+seed%29&action=Create+a+field&hardchange=0"

-- | Loads a sudoku with a given difficulty level from the internet.
loadSudoku :: Difficulty -> IO (Maybe Sudoku)
loadSudoku d = do
    manager <- newManager tlsManagerSettings
    request <- parseRequest (url d)
    response <- httpLbs request manager
    let doc = parseHtml $ unpack $ responseBody response
    values <- runX $ doc >>> css "input.sgrid" ! "value"
    -- in the html the values are aranged block wise and not row wise
    let transposedValues = ungroup . ungroup . transpose . groupBy 3 . groupBy 9
                         . ungroup . ungroup . transpose . groupBy 3 . groupBy 3 $ values
    let sudokuString = concat $ map (\v -> if v == "" then blankval:"" else v) transposedValues
    pure (fromString sudokuString)

