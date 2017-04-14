module Main where

import           Data.GI.Base
import qualified GI.Gtk                 as Gtk
import qualified GI.Gtk.Objects.Builder as Gtk
import qualified GI.Gtk.Objects.Window  as Gtk
import           Sudoku.Loader
import           Sudoku.Solver
import           Sudoku.Type

main :: IO ()
main = do
    Gtk.init Nothing
    builder <- Gtk.builderNewFromFile "src/hsudoku.ui"
    Just winO <- Gtk.builderGetObject builder "mainWindow"
    win <- Gtk.toWindow winO
    Gtk.main

