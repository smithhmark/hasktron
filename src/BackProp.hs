module BackProp
  ( learnFromExample
  , forwardPhase
  , outputNeuronGradient
  , outputLayerGradient
  , dCost
  ) where

import           LearningZipper
import           Neuron
import qualified NNTypes        as NNT

type Network = [NNT.Layer]

type NetworkDiff = Network

type Input = [Double]

type Targets = [Double]

learningZipperFromNetwork :: Network -> NetZip
learningZipperFromNetwork = fromList

learnFromExample :: Network -> Input -> Targets -> NetworkDiff
learnFromExample = undefined

forwardPhase :: NetZip -> Input -> NetZip
forwardPhase nz is = forwardPhase' nz2
  where
    nz1 = start nz
    nz2 = procLayerD nz1 op
    op = flip fwdEvalLayer is

forwardPhase' :: NetZip -> NetZip
forwardPhase' z@NetZip {deeper = []} = z
forwardPhase' z@NetZip {shallower = s:ss, deeper = d:ds} =
  forwardPhase' $ z {shallower = s2 : s : ss, deeper = ds}
  where
    s2 = fwdEvalLayer d $ acts s

fwdEvalLayer :: LLayer -> NNT.Activations -> LLayer
fwdEvalLayer layer@LLayer {weights = wss, biases = bs} is =
  layer {outs = foldr op [] $ zip wss bs}
  where
    op (ws, b) acc = (z, logistic z) : acc
      where
        z = freePerceptron ws b is

dCost :: NNT.Activation -> Double -> Double
dCost o x = o - x

outputNeuronGradient ::
     NNT.Activation -> Double -> NNT.Activations -> (NNT.Bias, NNT.Weights)
outputNeuronGradient o e ps = (deltaB, deltaWs)
  where
    dC_do = dCost o e
    do_dz = logisticPrime o
    deltaB = dC_do * do_dz
    deltaWs = map (\p -> deltaB * p) ps

outputLayerGradient :: LLayer -> LLayer -> [Double] -> NNT.Layer
outputLayerGradient l p ts = NNT.Layer { NNT.weights = map snd deltas, NNT.biases= map fst deltas }
  where
          deltas = map (\ (o, t) -> outputNeuronGradient o t (acts p) ) $ zip (acts l) ts
