module Main where

import           Control.Monad
import           Sudoku.Loader
import           Sudoku.Solver
import           Sudoku.Type

main :: IO ()
main = do maybeSudoku <- loadSudoku Evil
          putStrLn "Unsolved:"
          mapM_ (putStrLn . show) maybeSudoku
          putStrLn "Solved:"
          let maybeSolutions = join $ fmap solveSudoku maybeSudoku
          let maybeSolution  = fmap head maybeSolutions
          mapM_ (putStrLn . show) maybeSolution
          pure ()

