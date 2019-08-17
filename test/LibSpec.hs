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

  describe "perceptron" $ do
    it "be .5 for all zeros" $ do
      let ws = [0.0]
      let b = 0.0
      let as = [0.0]
      perceptron ws b as `shouldBe` 0.5
    it "be .5 for all zero weights" $ do
      let ws = [0.0]
      let b = 0.0
      property $ \as -> perceptron ws b as `shouldBe` 0.5

  describe "free_perceptron" $ do
    context "singleton weights and activations" $ do
      it "be 0.0 for all zeros" $ do
        let ws = [0.0]
        let b = 0.0
        let as = [0.0]
        free_perceptron ws b as `shouldBe` 0.0
      it "be 0.0 for all zero weights and 0.0 bias" $ do
        let ws = [0.0]
        let b = 0.0
        property $ \as -> free_perceptron ws b as `shouldBe` 0.0
      it "be -b for all zero weights" $ do
        let ws = [0.0]
        let as = [1.0]
        property $ \b -> free_perceptron ws b as `shouldBe` -b
    context "length-2 weights and activations" $ do
      it "be 0.0 for zero weights and bias" $ do
        let ws = [0.0, 0.0]
        let b = 0.0
        let as = [fromIntegral a | a <- [1 .. 2]]
        free_perceptron ws b as `shouldBe` 0.0
      it "be -b for zero weights" $ do
        let ws = [0.0, 0.0]
        let as = [fromIntegral a | a <- [1 .. 2]]
        property $ \b -> free_perceptron ws b as `shouldBe` -b
