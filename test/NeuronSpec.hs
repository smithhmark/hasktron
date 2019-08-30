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
  describe "softmax" $ do
    it "works for the wikipedia example" $ do
      let is = [1.0, 2.0, 3.0, 4.0, 1.0, 2.0, 3.0]
      let expt =
            [ 0.02364054
            , 0.06426166
            , 0.1746813
            , 0.474833
            , 0.02364054
            , 0.06426166
            , 0.1746813
            ]
      map (truncate . (* 10000)) (softmax is) `shouldBe`
        map (truncate . (* 10000)) expt
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
    let ws = [0.0]
    let b = 0.0
    let as = [0.0]
    context "singleton weights and activations" $ do
      it "be 0.0 for all zeros" $
        --let ws = [0.0]
        --let b = 0.0
        --let as = [0.0]
       do freePerceptron ws b as `shouldBe` 0.0
      it "be 0.0 for all zero weights and 0.0 bias" $
        --let ws = [0.0]
        --let b = 0.0
       do property $ \as -> freePerceptron ws b as `shouldBe` 0.0
      it "be -b for all zero weights" $
        --let ws = [0.0]
       do
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
  describe "quadraticCost" $ do
    it "find zero cost when expected = rcvd" $ do
      let as = [1.0, 2.0]
      let expected = 0.0
      quadraticCost as as `shouldBe` expected
    it "find 0.5 cost when one position differs by 1" $ do
      let ys = [0.0, 0.0]
      let as = [1.0, 0.0]
      let expected = 0.5
      quadraticCost ys as `shouldBe` expected
    it "find 1.0 cost when two positions differ by 1 each" $ do
      let ys = [0.0, 0.0]
      let as = [1.0, 1.0]
      let expected = 1.0
      quadraticCost ys as `shouldBe` expected
