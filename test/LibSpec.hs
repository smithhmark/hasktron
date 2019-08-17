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
  describe "logistic" $ do
    it "logistic of 0 should be .5" $ do logistic 0 `shouldBe` 0.5
    it "transforms values to the range (-1, 1)" $
      property $ \x -> logistic x <= 1 && logistic x >= -1
