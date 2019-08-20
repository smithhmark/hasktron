module LibSpec
  ( spec
  ) where

import           Test.Hspec
import           Test.QuickCheck

import           Lib

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "someMsg" $ do
    it "returns 'someMsg'" $ do someMsg `shouldBe` "someMsg"
