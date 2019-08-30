module BackProp
  ( learnFromExample
  , forwardPhase
  , outputNeuronGradient
  , outputLayerGradient
  , hiddenNeuronGradient
  , hiddenLayerGradient
  , dCost
  ) where

import           Data.List      (transpose)
import           LearningZipper
import           Neuron
import qualified NNTypes        as NNT

type Network = [NNT.Layer]

type NetworkDiff = Network

type Inputs = [Double]

type Targets = [Double]

learningZipperFromNetwork :: Network -> NetZip
learningZipperFromNetwork = fromList

learnFromExample :: Network -> Inputs -> Targets -> NetworkDiff
learnFromExample = undefined

-- | forwardPhase computes the feed forward phase of network
forwardPhase :: NetZip -> Inputs -> NetZip
forwardPhase nz is = forwardPhase' nz2
  where
    nz1 = start nz
    nz2 = procLayerD nz1 op
    op = flip evalLayerFwd is

-- | forwardPhase' is the recursive worker fuction that transforms a layer
-- and moves to the next
forwardPhase' :: NetZip -> NetZip
forwardPhase' z@NetZip {deeper = []} = z
forwardPhase' z@NetZip {shallower = s:ss, deeper = d:ds} =
  forwardPhase' $ z {shallower = s2 : s : ss, deeper = ds}
  where
    s2 = evalLayerFwd d $ acts s

-- | evalLayerFwd does the work off transforming a layer based on prior
-- activiations
evalLayerFwd ::
     LLayer -- ^ the current layer being processed
  -> NNT.Activations -- ^ the activations from the previous layer
  -> LLayer -- ^ the input layer transformed with the results
evalLayerFwd layer@LLayer {weights = wss, biases = bs} is =
  layer {outs = foldr op [] $ zip wss bs}
  where
    op (ws, b) acc = (z, logistic z) : acc
      where
        z = freePerceptron ws b is

-- | the derivative of the cost with respect to a neuron's output
dCost ::
     NNT.Activation -- ^ a neuron's output activation
  -> Double -- ^ the target value
  -> Double -- ^ d(C)/d(o_i)
dCost o x = o - x

-- | computes the corrections for the bias and the weights of a neuron
outputNeuronGradient ::
     NNT.Activation -- ^ output of the neuron
  -> Double -- ^ the target output
  -> NNT.Activations -- ^ activations from prior level
  -> (NNT.Bias, NNT.Weights)
outputNeuronGradient o t ps = (deltaB, deltaWs)
  where
    dC_do = dCost o t
    do_dz = logisticPrime o
    deltaB = dC_do * do_dz
    deltaWs = map (\p -> deltaB * p) ps

outputLayerGradient :: LLayer -> LLayer -> [Double] -> NNT.Layer
outputLayerGradient l p ts =
  NNT.Layer {NNT.weights = map snd deltas, NNT.biases = map fst deltas}
  where
    deltas =
      map (\(o, t) -> outputNeuronGradient o t (acts p)) $ zip (acts l) ts

evalBackwards :: NetZip -> Inputs -> Targets -> NetworkDiff
evalBackwards nz is ts = evalBackwards' (end nz) is ts []

-- | compute the gradients for a single hidden neuron
hiddenNeuronGradient ::
     NNT.Activation -- ^ the neuron's current activation
  -> NNT.Activations -- ^ the activiations from the shallower layer
  -> [Double] -- ^ deltas from deeper
  -> NNT.Weights -- ^ weights connecting this neuron to the next deeper layer
  -> (NNT.Bias, NNT.Weights)
hiddenNeuronGradient o as ds ws = (newDelta, dDotActs)
  where
    sigPrime = logisticPrime o
    newDelta = (* sigPrime) $ sum $ zipWith (*) ds ws
    dDotActs = map (* newDelta) as

hiddenLayerGradient ::
     Inputs -- ^ outputs from prior loyer or inputs
  -> [Double] -- ^ the deltas from the deeper level
  -> [NNT.Weights] -- ^ the weights connecting this layer to the deeper layer
  -> NNT.Activations
  -> [(NNT.Bias, NNT.Weights)]
hiddenLayerGradient is ds wss os = foldr op [] $ zip os wsst
  where
    wsst = transpose wss
    op (o, ws) acc = hiddenNeuronGradient o is ds ws : acc

evalBackwards' :: NetZip -> Inputs -> Targets -> NetworkDiff -> NetworkDiff
evalBackwards' nz is ts ac
  | beginp nz = ac
  | otherwise = undefined