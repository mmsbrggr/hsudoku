module Main where

import           Data.GI.Base
import qualified GI.Gtk        as Gtk
import qualified GtkUtils      as Gtk

import           Data.Text     hiding (map, zipWith)
import           Sudoku.Loader
import           Sudoku.Solver
import           Sudoku.Type

main :: IO ()
main = do
    Gtk.init Nothing
    (window, builder) <- Gtk.buildMainWindow "mainWindow" "gui/hsudoku.ui"
    Gtk.windowAddCss window "gui/theme.css"
    inputPopover <- Gtk.builderGetTyped builder "inputPopover" Gtk.Popover
    cells <- Gtk.builderGetsTyped builder cellNames Gtk.MenuButton
    Just sudoku <- loadSudoku Easy
    sequence $ zipWith (\c t -> writeCell c t) cells (toString sudoku)
    #showAll window
    Gtk.main

cellNames :: [Text]
cellNames = map (pack . (++) "cell") $ map show [1..81]

writeCell :: Gtk.MenuButton -> Char -> IO ()
writeCell cell char = do
    binCell <- Gtk.toBin cell
    labelO <- Gtk.binGetChild binCell
    label <- Gtk.unsafeCastTo Gtk.Label labelO
    Gtk.labelSetText label (singleton char)

