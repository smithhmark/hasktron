module BackPropSpec
  ( spec
  ) where

import           Test.Hspec
import           Test.QuickCheck

import           BackProp
import           LearningZipper
import           Neuron
import qualified NNTypes         as NNT

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let normalize ds = map (truncate . (* 1000000)) ds
  describe "forwardPhase" $ do
    it "processes an input" $ do
      let net =
            [ NNT.Layer
                { NNT.weights = [[1, 2, 3], [2, 3, 1], [3, 1, 2]]
                , NNT.biases = [10.0, 12.0, 14.0]
                }
            , NNT.Layer
                {NNT.weights = [[1, 2, 3], [3, 2, 1]], NNT.biases = [2, 3]}
            ]
      let zs = [0.7689414214, 0.2310585786]
      let os = [0.6832918574, 0.5575090141]
      let is = [3, 2, 1] :: [Double]
      let z1 = fromList net
      let z2 = forwardPhase z1 is
      let finallayer = cursor . goShallower $ end z2
      normalize (nets finallayer) `shouldBe` normalize zs
      normalize (acts finallayer) `shouldBe` normalize os
  describe "outputNeuronGradient" $ do
    context "computes d(bias) and d(weights)" $ do
      let prev = [0.5932699921, 0.5968843783]
      --let weights = [0.4, 0.45]
      --let bias = 0.6
      let output = 0.7513650696
      let target = 0.01
      it "is using the correct cost derivative" $ do
        normalize [dCost output target] `shouldBe` normalize [0.7413650696]
      it "is using the correct sigmoidPrime" $ do
        normalize [logisticPrime output] `shouldBe` normalize [0.1868156018]
      let (dB, dWs) = outputNeuronGradient output target prev
      it "calculated the correct Bias" $ do
        normalize [dB] `shouldBe` normalize [0.1384985616]
      it "calculated the correct weights" $ do
        normalize dWs `shouldBe` normalize [0.08216704056, 0.08266762785]
  describe "outputLayerGradient" $ do
    it "computes the gradient across the whole final layer" $ do
      let oLayer =
            LLayer
              { weights = [[0.4, 0.45], [0.5, 0.55]]
              , biases = [0, 6, 0.6]
              , outs =
                  [(1.105905967, 0.7513650696), (1.224921404, 0.7729284653)]
              }
      let pLayer =
            LLayer
              { weights = [[0.15, 0.2], [0.25, 0.3]]
              , biases = [0.35, 0.35]
              , outs = [(0.3775, 0.5932699921), (0.3925, 0.5968843783)]
              }
      let targets = [0.01, 0.99]
      let biasDs = [0.1384985616, -0.03809823652]
      let weightDs =
            [[0.08216704056, 0.08266762785], [-0.02260254048, -0.02274024222]]
      let ol = outputLayerGradient oLayer pLayer targets
      normalize (NNT.biases ol) `shouldBe` normalize biasDs
