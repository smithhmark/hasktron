module FeedForwardSpec
  ( spec
  ) where

import           Test.Hspec
import           Test.QuickCheck

import           FeedForward
import           Neuron
import           NNTypes

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let normalize ds = map (truncate . (* 1000000)) ds
  let norm ds = map (truncate . (* 1000)) ds
  describe "nextLayer" $ do
    it "calculates a handbuilt example correctly" $ do
      let as = [1.0, 2.0]
      let wss = [[1.0, 2.0], [3.0, 4.0]]
      let bs = [-5.0, -6.0]
      nextLayer (freePerceptron) as wss bs `shouldBe` [0.0, 5.0]
    it "calculates the BP handbuilt example correctly" $ do
      let as = [0.05, 0.1]
      let wss = [[0.15, 0.20], [0.25, 0.3]]
      let bs = [0.35,0.35]
      norm (nextLayer (freePerceptron) as wss bs) `shouldBe` norm [0.3775, 0.3925]
      normalize ( nextLayer (perceptron) as wss bs) `shouldBe` normalize [0.5932699921, 0.5968843783]
  describe "feedForward" $ do
    it "processes multi layer network correctly" $ do
      let net =
            [ Layer
                { weights = [[1, 2, 3], [2, 3, 1], [3, 1, 2]]
                , biases = [-10.0, -12.0, -14.0]
                }
            , Layer {weights = [[1, 2, 3], [3, 2, 1]], biases = [-2, -3]}
            ]
      let inputs = [3.0, 2.0, 1.0]
      let expts = [0.6832918574, 0.5575090141]
      let rcvd = feedForward net inputs
      --let normalize ds = map (truncate . (* 1000000)) ds
      normalize rcvd `shouldBe` normalize expts
    it "processed the BP example" $ do
      let net =
            [ Layer
                {weights = [[0.15, 0.2], [0.25, 0.3]], biases = [0.35, 0.35]}
            , Layer {weights = [[0.4, 0.45], [0.5, 0.55]], biases = [0.6, 0.6]}
            ]
      let is = [0.05, 0.1]
      let rcvd = feedForward net is
      let oos = [0.7513650696, 0.7729284653]
      normalize rcvd `shouldBe` normalize oos
