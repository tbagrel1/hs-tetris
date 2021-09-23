import Coords
  ( addCoord,
  )
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Coords.addCoords" $ do
    it "adds coordinates of both points" $ do
      addCoord (1, 2) (3, 4) `shouldBe` ((4, 6) :: (Int, Int))
