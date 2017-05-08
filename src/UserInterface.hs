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
    , SudokuUI
    , window
    , menu
    , gameButtons
    , cells
    , popover
    , numberButtons
    , inputClear
    , inputSolve
    , solveButton
    , checkButton
    , menuButton
    , buildSudokuUI
    , writePopoverRelativeCell
    , solveAll
    , solvePopoverRelativeCell
    , checkAll
    , cellsBindHandlers
    , numbersBindHandlers
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
import           Sudoku.Solver
import           Control.Concurrent (forkIO, threadDelay)


-- | Thrown when 'castB' fails get an object
data BuilderCastException = UnknownIdException String deriving (Show, Typeable)

instance Exception BuilderCastException

-- | Alias type for a sudoku cell
type Cell = Button
-- | Alias type for all sudoku cells
type Cells = [Cell]
-- | Alias type for the game menu
type GameMenu = Widget

-- | A type containing all handles of widgets necessary for the user interface.
data SudokuUI = SudokuUI { window        :: Window 
                         , menu          :: GameMenu
                         , gameButtons   :: [Button]
                         , cells         :: Cells
                         , popover       :: Popover
                         , numberButtons :: [Button]
                         , inputClear    :: Button
                         , inputSolve    :: Button
                         , solveButton   :: Button
                         , checkButton   :: Button
                         , menuButton    :: Button
                         }


-- | Builds the sudoku-ui from a gui-file for which the path is given.
buildSudokuUI :: T.Text -> IO SudokuUI
buildSudokuUI guiFilePath = do
    (window, builder) <- buildMainWindow "mainWindow" guiFilePath
    menu              <- builderGetTyped builder "menu" Widget
    gameButtons       <- builderGetsTyped builder gameButtonNames Button
    cells             <- builderGetsTyped builder cellNames Button
    popover           <- builderGetTyped builder "inputPopover" Popover
    numberButtons     <- builderGetsTyped builder numberNames Button
    inputClear        <- builderGetTyped builder "inputClear" Button
    inputSolve        <- builderGetTyped builder "inputSolve" Button
    solveButton       <- builderGetTyped builder "solveButton" Button
    checkButton       <- builderGetTyped builder "checkButton" Button
    menuButton        <- builderGetTyped builder "menuButton" Button
    pure $ SudokuUI window menu gameButtons cells popover 
                    numberButtons inputClear inputSolve solveButton
                    checkButton menuButton

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
writeCell :: Cell -> Char -> IO ()
writeCell cell char = #setLabel cell (T.singleton char)

-- | Writes a charachter into a cell which is associated to a given popover
--   The popover gets closed afterwards.
writePopoverRelativeCell :: Popover -> Char -> IO ()
writePopoverRelativeCell popover char = do
    widget <- #getRelativeTo popover
    cell   <- unsafeCastTo Button widget
    writeCell cell char
    #hide popover

-- | solves a given cell
solveCell :: Cell -> IO ()
solveCell cell = do
    char <- T.head <$> #getName cell
    writeCell cell char

-- | solves all given cells
solveAll :: Cells -> IO ()
solveAll = mapM_ solveCell

-- | Solves the cell currently relative to the popover
solvePopoverRelativeCell :: Popover -> IO ()
solvePopoverRelativeCell popover = do
    cell <- #getRelativeTo popover >>= unsafeCastTo Button
    solveCell cell
    #hide popover

-- | Binds the signal handlers to buttons
cellsBindHandlers :: Cells -> Popover -> IO ()
cellsBindHandlers cells popover = mapM_ (\c -> do
            on c #focusInEvent  $ focusInHandler c
        ) cells
    where focusInHandler c _ = do cellShowPopover c popover; pure False

-- | Checks and returns if a given cell contains the correct value
--   If the value is not correct the cell gets visually marked
checkCell :: Cell -> IO Bool
checkCell cell = do
    solution <- T.head <$> (toWidget cell >>= #getName)
    actual <- T.head <$> #getLabel cell 
    let isCorrect = actual == solution
    style <- #getStyleContext cell
    if not isCorrect
        then #addClass style "incorrect"
        else pure ()
    forkIO $ threadDelay 800000 >> #removeClass style "incorrect"
    pure isCorrect

-- | Checks if all given cells contain the correct value
--   Visually marks the correct or incorrect cells.
checkAll :: Cells -> IO ()
checkAll cells = do
    allAreCorrect <- and <$> mapM checkCell cells
    if allAreCorrect
        then mapM_ (\cell -> do
            style <- #getStyleContext cell
            #addClass style "correct"
            forkIO $ threadDelay 800000 >> #removeClass style "correct"
        ) cells
        else pure ()

-- | Associates the popover to a given button and shows the popover
cellShowPopover :: Cell -> Popover -> IO ()
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
writeSudoku :: Cells -> Sudoku -> IO ()
writeSudoku cells sudoku = do
    let sudokuChars = toString sudoku
    sequence_ $ zipWith (\c sc -> do
            writeCell c sc
            if sc == blankval
                then c `set` [#sensitive := True]
                else c `set` [#sensitive := False]
        ) cells sudokuChars

-- | Stores a given solution in the names of the passed cells
writeSolution :: Cells -> Sudoku -> IO ()
writeSolution cells sudoku = do
    let sudokuChars = toString sudoku
    sequence_ $ zipWith (\c sc -> do
            #setName c (T.singleton sc)
        ) cells sudokuChars

-- | Binds the signal handlers to the game buttons in the menu
gameButtonsBindHandlers :: [Button] -> Cells -> Widget -> IO ()
gameButtonsBindHandlers buttons cells menu = do
    mapM_ (\button -> do
            label <- #getLabel button
            let d = read . T.unpack $ label
            on button #clicked $ newGame d cells menu
        ) buttons

-- | Prepares a new game in the UI
newGame :: Difficulty -> Cells -> GameMenu -> IO ()
newGame d cells menu = do
    Just sudoku <- loadSudoku d
    let Just solution = head <$> solveSudoku sudoku
    writeSudoku cells sudoku
    writeSolution cells solution
    #hide menu

-- | Shows the menu and ensures that the popover is hidden
showMenu :: GameMenu -> Popover -> IO ()
showMenu menu popover = do
    #hide popover
    popover `set` [#relativeTo := menu]
    #show menu
    
