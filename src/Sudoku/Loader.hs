module Sudoku.Loader where

import           Data.List         (elemIndex)
import           Data.Maybe        (fromMaybe)
import           Sudoku.Type
import           Text.HandsomeSoup
import           Text.Printf       (printf)

urlPattern :: String
urlPattern = "http://show.websudoku.com/?level=%d"

url :: Difficulty -> String
url d = printf urlPattern $ param d
    where param :: Difficulty -> Int
          param d = fromMaybe 1 $ elemIndex d [Easy, Medium, Hard, Evil]

loadSudoku d = fromUrl (url d)

