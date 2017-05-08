module Main where

import           Data.GI.Base
import qualified GI.Gtk        as Gtk
import           Sudoku.Type
import           UserInterface

main :: IO ()
main = do
    Gtk.init Nothing

    ui <- buildSudokuUI "gui/hsudoku.ui"
    cellsBindHandlers (cells ui) (popover ui)
    numbersBindHandlers (numberButtons ui) (popover ui)
    on (inputClear ui) #clicked $ writePopoverRelativeCell (popover ui) $ blankval
    on (inputSolve ui) #clicked $ solvePopoverRelativeCell (popover ui)
    on (solveButton ui) #clicked $ solveAll (cells ui)
    on (checkButton ui) #clicked $ checkAll (cells ui)
    gameButtonsBindHandlers (gameButtons ui) (cells ui) (menu ui)
    on (menuButton ui) #clicked $ showMenu (menu ui) (popover ui)

    #showAll (window ui)
    Gtk.main

