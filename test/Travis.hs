-- Travis does not support ubuntu 16.04 yet and therefore
-- GTK+ 3.20 cannot be installed. As the user interface
-- uses new features from GTK+ 3.20, these cannot be tested on travis
import           Test.Hspec

import qualified Sudoku.LoaderSpec
import qualified Sudoku.SolverSpec
import qualified Sudoku.TypeSpec
import qualified UtilSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "UtilSpec"          UtilSpec.spec
    describe "Sudoku.LoaderSpec" Sudoku.LoaderSpec.spec
    describe "Sudoku.SolverSpec" Sudoku.SolverSpec.spec
    describe "Sudoku.TypeSpec"   Sudoku.TypeSpec.spec

