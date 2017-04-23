{-|
Module: UserInterface
Description: Provides various functions for building the user interface
Copyright: (c) Marcel Moosbrugger, 2017
License     : MIT

Provides various functions for building the user interface of the
sudoku application
-}
module UserInterface (
      BuilderCastException(..)
    , cellNames
    , numberNames
    , gameButtonNames
    , builderGetTyped
    , builderGetsTyped
    , buildMainWindow
    , windowAddCss
    , writeCell
    , writePopoverRelativeCell
    , cellsBindHandlers
    , numbersBindHandlers
    , writeSudoku
    , gameButtonsBindHandlers
    , showMenu
    ) where

import           Control.Exception
import           Control.Monad.IO.Class
import           Data.GI.Base
import qualified Data.Text              as T
import           Data.Typeable
import           GI.Gtk
import           Sudoku.Type
import           Sudoku.Loader

-- | Thrown when 'castB' fails get an object
data BuilderCastException = UnknownIdException String deriving (Show, Typeable)

instance Exception BuilderCastException

-- | The ids of the sudoku cells in the ui file
cellNames :: [T.Text]
cellNames = map (T.pack . (++) "cell") $ map show [1..81]

-- | The ids of the inputs for the numbers in the ui file
numberNames :: [T.Text]
numberNames = map (T.pack . (++) "input") $ map show [1..9]

-- | The ids of the buttons which start a new game
gameButtonNames :: [T.Text]
gameButtonNames = map (T.pack . (++) "game" . show) [Easy ..] 

-- | Takes a builder and returns the object with a given name
--   typed as a given gtype
builderGetTyped :: (IsBuilder a, GObject o, MonadIO m) => a -> T.Text -> (ManagedPtr o -> o) -> m o
builderGetTyped builder ident gtype =
    liftIO $ do
        o <- builderGetObject builder ident
        case o of
            Just a  -> unsafeCastTo gtype a
            Nothing -> throw $ UnknownIdException $ T.unpack ident

-- | Same as builderGetTyped for a list of names
builderGetsTyped :: (GObject a, IsBuilder b, MonadIO m) => b -> [T.Text] -> (ManagedPtr a -> a) -> m [a]
builderGetsTyped b is t = sequence $ map (\i -> builderGetTyped b i t) is

-- | Builds the main application window from a xml definition file for which the
--   path is given
buildMainWindow :: MonadIO m => T.Text -> T.Text -> m (Window, Builder)
buildMainWindow name path = liftIO $ do
    builder <- builderNewFromFile path
    window  <- builderGetTyped builder name Window
    on window #destroy mainQuit
    windowAddCss window "gui/theme.css"
    pure (window, builder)

-- | Adds to a given window a css file for which the path is given
windowAddCss :: (MonadIO m, IsWindow a) => a -> T.Text -> m ()
windowAddCss window path = liftIO $ do
    screen <- windowGetScreen window
    cssProvider <- cssProviderNew
    cssProviderLoadFromPath cssProvider path
    styleContextAddProviderForScreen screen cssProvider 1000

-- | Writes a character into a sudoku cell
writeCell :: (IsBin o) => o -> Char -> IO ()
writeCell cell char = do
    binCell <- toBin cell
    labelO <- binGetChild binCell
    label <- unsafeCastTo Label labelO
    labelSetText label (T.singleton char)

-- | Writes a charachter into a cell which is associated to a given popover
--   The popover gets closed afterwards.
writePopoverRelativeCell :: Popover -> Char -> IO ()
writePopoverRelativeCell popover char = do
    widget <- #getRelativeTo popover
    cell   <- unsafeCastTo Button widget
    writeCell cell char
    #hide popover

-- | Binds the signal handlers to buttons
cellsBindHandlers :: [Button] -> Popover -> IO ()
cellsBindHandlers cells popover = mapM_ (\c -> do
            on c #focusInEvent  $ focusInHandler c
        ) cells
    where focusInHandler c _ = do cellShowPopover c popover; pure False

-- | Associates the popover to a given button and shows the popover
cellShowPopover :: Button -> Popover -> IO ()
cellShowPopover cell popover = do
    popover `set` [#relativeTo := cell]
    #show popover

-- | Binds the signal handlers to a list of number buttons
numbersBindHandlers :: [Button] -> Popover -> IO ()
numbersBindHandlers buttons popover = mapM_ (\b -> do
            on b #clicked $ numberButtonInsert b popover
        ) buttons

-- | Inserts the content of a number button to a cell associated to the popover
numberButtonInsert :: Button -> Popover -> IO ()
numberButtonInsert button popover = do
    label <- #getLabel button
    writePopoverRelativeCell popover $ T.head label

-- | Writes a sudoku into a list of buttons
writeSudoku :: [Button] -> Sudoku -> IO ()
writeSudoku buttons sudoku = do
    let sudokuChars = toString sudoku
    sequence_ $ zipWith (\b c -> do
            writeCell b c
            if c == blankval
                then b `set` [#sensitive := True]
                else b `set` [#sensitive := False]
        ) buttons sudokuChars

-- | Binds the signal handlers to the game buttons in the menu
gameButtonsBindHandlers :: [Button] -> [Button] -> Widget -> IO ()
gameButtonsBindHandlers buttons cells menu = do
    mapM_ (\button -> do
            label <- #getLabel button
            let d = read . T.unpack $ label
            on button #clicked $ newGame d cells menu
        ) buttons

-- | Prepares a new game in the UI
newGame :: Difficulty -> [Button] -> Widget -> IO ()
newGame d cells menu = do
    Just sudoku <- loadSudoku d
    writeSudoku cells sudoku
    #hide menu

showMenu :: Widget -> Popover -> IO ()
showMenu menu popover = do
    #hide popover
    popover `set` [#relativeTo := menu]
    #show menu
    
