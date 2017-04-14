{-|
Module: GtkUtils
Description: Provides generic utility functions regarding gtk functionality
Copyright: (c) Marcel Moosbrugger, 2017
License     : MIT

This modules provides generic utility functions regarding Gtk.
-}
module GtkUtils (BuilderCastException(..), builderGetTyped) where

import           Control.Exception
import           Control.Monad.IO.Class
import           Data.GI.Base
import           Data.Text
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

