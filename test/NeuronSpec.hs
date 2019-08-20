module NeuronSpec
  ( spec
  ) where

import           Test.Hspec
import           Test.QuickCheck

import           Neuron

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
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
  describe "freePerceptron" $ do
    context "singleton weights and activations" $ do
      it "be 0.0 for all zeros" $ do
        let ws = [0.0]
        let b = 0.0
        let as = [0.0]
        freePerceptron ws b as `shouldBe` 0.0
      it "be 0.0 for all zero weights and 0.0 bias" $ do
        let ws = [0.0]
        let b = 0.0
        property $ \as -> freePerceptron ws b as `shouldBe` 0.0
      it "be -b for all zero weights" $ do
        let ws = [0.0]
        let as = [1.0]
        property $ \b -> freePerceptron ws b as `shouldBe` -b
    context "length-2 weights and activations" $ do
      it "be 0.0 for zero weights and bias" $ do
        let ws = [0.0, 0.0]
        let b = 0.0
        let as = [fromIntegral a | a <- [1 .. 2]]
        freePerceptron ws b as `shouldBe` 0.0
      it "be -b for zero weights" $ do
        let ws = [0.0, 0.0]
        let as = [fromIntegral a | a <- [1 .. 2]]
        property $ \b -> freePerceptron ws b as `shouldBe` -b
  describe "nextLayer" $ do
    it "plays nice" $ do
      let as = [1.0, 2.0]
      let wss = [[1.0, 2.0], [3.0, 4.0]]
      let bs = [5.0, 6.0]
      nextLayer (freePerceptron) as wss bs `shouldBe` [0.0, 5.0]
