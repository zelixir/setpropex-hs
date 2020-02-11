import           Test.Hspec
import           Property
import           Helper

main :: IO ()
main = hspec $ do
  describe "property" $ do
    it "find memory region" $ do
      r <- liftIO $ findMemoryRegion' "test/maps" "/dev/__properties__"
      r `shouldBe` 0xb7716000

      
