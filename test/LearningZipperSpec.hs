module LearningZipperSpec
  ( spec
  ) where

import           Test.Hspec
import           Test.QuickCheck

import           LearningZipper
import qualified NNTypes         as NNT

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "NetZip" $ do
    context "using a simple 2 level example" $ do
      let twoLayerExample =
            [ NNT.Layer
                { NNT.weights = [[1, 2, 3], [2, 3, 1], [3, 1, 2]]
                , NNT.biases = [10.0, 12.0, 14.0]
                }
            , NNT.Layer
                {NNT.weights = [[1, 2, 3], [3, 2, 1]], NNT.biases = [2, 3]}
            ]
      it "can be built from a list of NNT.Layers" $ do
        let net = twoLayerExample
        let rcvd = fromList net
        length (shallower rcvd) `shouldBe` 0
        length (deeper rcvd) `shouldBe` length net
        weights (head (deeper rcvd)) `shouldBe` NNT.weights (head net)
        biases (head (deeper rcvd)) `shouldBe` NNT.biases (head net)
        weights (head (tail (deeper rcvd))) `shouldBe`
          NNT.weights (head (tail net))
        biases (head (tail (deeper rcvd))) `shouldBe`
          NNT.biases (head (tail net))
      it "can be navigated forward" $ do
        let nz1 = fromList twoLayerExample
        let nzd = goDeeper nz1
        let nzdd = goDeeper nzd
        beginp nz1 `shouldBe` True
        endp nz1 `shouldBe` False
        beginp nzd `shouldBe` False
        endp nzd `shouldBe` False
        length (shallower nzd) `shouldBe` 1
        beginp nzdd `shouldBe` False
        endp nzdd `shouldBe` True
        length (shallower nzdd) `shouldBe` 2
      it "can retrieve layers" $ do
              pendingWith "tests not implemented"
      it "can transform layers" $ do
              pendingWith "tests not implemented"
