{-|
Module: GtkUtils
Description: Provides generic utility functions regarding gtk functionality
Copyright: (c) Marcel Moosbrugger, 2017
License     : MIT

This modules provides generic utility functions regarding Gtk.
-}
module GtkUtils (
      BuilderCastException(..)
    , builderGetTyped
    , builderGetsTyped
    , buildMainWindow
    , windowAddCss
    , writeCell
    ) where

import           Control.Exception
import           Control.Monad.IO.Class
import           Data.GI.Base
import           Data.Text              hiding (map)
import           Data.Typeable
import           GI.Gtk

-- | Thrown when 'castB' fails get an object
data BuilderCastException = UnknownIdException String deriving (Show, Typeable)

instance Exception BuilderCastException

-- | Takes a builder and returns the object with a given name
--   typed as a given gtype
builderGetTyped :: (IsBuilder a, GObject o, MonadIO m) => a -> Text -> (ManagedPtr o -> o) -> m o
builderGetTyped builder ident gtype =
    liftIO $ do
        o <- builderGetObject builder ident
        case o of
            Just a  -> unsafeCastTo gtype a
            Nothing -> throw $ UnknownIdException $ unpack ident

builderGetsTyped b is t = sequence $ map (\i -> builderGetTyped b i t) is

-- | Builds the main application window from a xml definition file for which the
--   path is given
buildMainWindow :: MonadIO m => Text -> Text -> m (Window, Builder)
buildMainWindow name path = liftIO $ do
    builder <- builderNewFromFile path
    window  <- builderGetTyped builder name Window
    on window #destroy mainQuit
    pure (window, builder)

-- | Adds to a given window a css file for which the path is given
windowAddCss :: (MonadIO m, IsWindow a) => a -> Text -> m ()
windowAddCss window path = liftIO $ do
    screen <- windowGetScreen window
    cssProvider <- cssProviderNew
    cssProviderLoadFromPath cssProvider path
    styleContextAddProviderForScreen screen cssProvider 1000

-- | Writes a character into the label of a menu-button representing a sudoku
--   cell
writeCell :: (IsBin o) => o -> Char -> IO ()
writeCell cell char = do
    binCell <- toBin cell
    labelO <- binGetChild binCell
    label <- unsafeCastTo Label labelO
    labelSetText label (singleton char)

