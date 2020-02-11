{-# LANGUAGE OverloadedStrings, TupleSections #-}
import           Test.Hspec
import           Property
import           Helper
import qualified Data.ByteString               as B

main :: IO ()
main = hspec $ do
  describe "property" $ do
    it "find memory region" $ do
      r <- liftIO $ findMemoryRegion' "test/maps" "/dev/__properties__"
      r `shouldBe` 0xb7716000

    it "find property" $ do
      let file = "test/__properties__"
      Just prop <- liftIO (fromFile file)
      findProperty "ro.debuggable" prop `shouldBe` Just ("0", 0x100c)
