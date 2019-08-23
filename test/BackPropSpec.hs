module BackPropSpec
  ( spec
  ) where

import           Test.Hspec
import           Test.QuickCheck

import           BackProp
import           LearningZipper
import qualified NNTypes         as NNT

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "forwardPhase" $ do
    it "processes an input" $ do
      let normalize ds = map (truncate . (* 1000000)) ds
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
      let is = [3,2,1] :: [Double]
      let z1 = fromList net
      let z2 = forwardPhase z1 is
      let finallayer = cursor. goShallower $ end z2
      normalize (nets finallayer) `shouldBe` normalize zs
      normalize (acts finallayer) `shouldBe` normalize os
