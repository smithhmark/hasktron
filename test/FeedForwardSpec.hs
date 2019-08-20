module FeedForwardSpec
  ( spec
  ) where

import           Test.Hspec
import           Test.QuickCheck

import           FeedForward
import           Neuron

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "nextLayer" $ do
    it "calculates a handbuilt example correctly" $ do
      let as = [1.0, 2.0]
      let wss = [[1.0, 2.0], [3.0, 4.0]]
      let bs = [5.0, 6.0]
      nextLayer (freePerceptron) as wss bs `shouldBe` [0.0, 5.0]
