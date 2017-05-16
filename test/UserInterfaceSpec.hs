module UserInterfaceSpec (main, spec) where

import qualified GI.Gtk        as Gtk
import           Test.Hspec
import           UserInterface

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- -- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "buildSudokuUI" $ do
        it "should actually return a UI data-structure and not fail" $ do
            _ <- Gtk.init Nothing
            ui <- buildSudokuUI
            (length $ cells ui) `shouldSatisfy` (> 0)
            (length $ gameButtons ui) `shouldSatisfy` (> 0)
            (length $ numberButtons ui) `shouldSatisfy` (> 0)

