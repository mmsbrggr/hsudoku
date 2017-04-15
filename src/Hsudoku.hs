module Main where

import           Data.GI.Base
import qualified GI.Gtk        as Gtk
import qualified GtkUtils      as Gtk

import qualified Data.Text as T
import           Sudoku.Loader
import           Sudoku.Solver
import           Sudoku.Type
import           Util

main :: IO ()
main = do
    Gtk.init Nothing
    (window, builder) <- Gtk.buildMainWindow "mainWindow" "gui/hsudoku.ui"
    Gtk.windowAddCss window "gui/theme.css"
    inputPopover <- Gtk.builderGetTyped builder "inputPopover" Gtk.Popover
    cells <- Gtk.builderGetsTyped builder cellNames Gtk.Button
    cellsBindHandlers cells inputPopover
    #showAll window
    Gtk.main

cellNames :: [T.Text]
cellNames = map (T.pack . (++) "cell") $ map show [1..81]

cellsBindHandlers :: [Gtk.Button] -> Gtk.Popover -> IO ()
cellsBindHandlers cells popover = mapM_ (\c -> do
            on c #focusInEvent $ focusInHandler c
            on c #focusOutEvent $ focusOutHandler c
        ) cells
    where focusInHandler c _ = cellShowPopover c popover
          focusOutHandler c _ = maybeHidePopover popover

cellShowPopover :: Gtk.Button -> Gtk.Popover -> IO Bool
cellShowPopover cell popover = do
    popover `set` [#relativeTo := cell]
    #show popover
    pure False

maybeHidePopover :: Gtk.Popover -> IO Bool
maybeHidePopover popover = do
    -- #hide popover
    pure False

