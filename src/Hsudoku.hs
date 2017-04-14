module Main where

import           Data.GI.Base
import qualified GI.Gtk        as Gtk
import qualified GtkUtils      as Gtk

import           Sudoku.Loader
import           Sudoku.Solver
import           Sudoku.Type

main :: IO ()
main = do
    Gtk.init Nothing
    builder <- Gtk.builderNewFromFile "src/hsudoku.ui"
    window  <- Gtk.builderGetTyped builder "mainWindow" Gtk.Window
    on window #destroy Gtk.mainQuit
    #showAll window
    Gtk.main

