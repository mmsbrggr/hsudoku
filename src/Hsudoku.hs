module Main where

import           Data.GI.Base
import qualified Data.Text     as T
import qualified GI.Gtk        as Gtk
import           Sudoku.Loader
import           Sudoku.Solver
import           Sudoku.Type
import qualified UserInterface as Ui
import           Util

main :: IO ()
main = do
    Gtk.init Nothing

    (window, builder) <- Ui.buildMainWindow "mainWindow" "gui/hsudoku.ui"
    inputPopover <- Ui.builderGetTyped builder "inputPopover" Gtk.Popover

    cells <- Ui.builderGetsTyped builder Ui.cellNames Gtk.Button
    Ui.cellsBindHandlers cells inputPopover

    numberButtons <- Ui.builderGetsTyped builder Ui.numberNames Gtk.Button
    Ui.numbersBindHandlers numberButtons inputPopover
    inputClear <- Ui.builderGetTyped builder "inputClear" Gtk.Button
    on inputClear #clicked $ Ui.writePopoverRelativeCell inputPopover $ blankval

    menu <- Ui.builderGetTyped builder "menu" Gtk.Widget
    gameButtons <- Ui.builderGetsTyped builder Ui.gameButtonNames Gtk.Button
    Ui.gameButtonsBindHandlers gameButtons cells menu
    menuButton <- Ui.builderGetTyped builder "menuButton" Gtk.Button
    on menuButton #clicked (#show menu)

    #showAll window
    Gtk.main


