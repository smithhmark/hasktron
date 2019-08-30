module BackProp
  ( learnFromExample
  , forwardPhase
  , outputNeuronGradient
  , outputLayerGradient
  , hiddenNeuronGradient
  , hiddenLayerGradient
  , dCost
  , gradientFromExample
  , evalBackwards
  ) where

import           Data.List      (transpose)
import           LearningZipper
import           Neuron
import qualified NNTypes        as NNT

type Network = [NNT.Layer]

type NetworkDiff = [NNT.Layer]

type Inputs = [Double]

type Targets = [Double]

learnFromExample ::
     Network
  -> Inputs
  -> Float -- ^ mu, learning rate
  -> Targets
  -> NetworkDiff
learnFromExample = undefined

gradientFromExample :: Network -> Inputs -> Targets -> NetworkDiff
gradientFromExample net is ts = evalBackwards nz1 is ts 
        where nz = fromList net
              nz1 = forwardPhase nz is

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

outputLayerGradient ::
     LLayer -- ^ the output layer
  -> LLayer -- ^ the prior layer
  -> [Double] -- ^ the output targets
  -> NNT.Layer
outputLayerGradient l p ts =
  NNT.Layer {NNT.weights = map snd deltas, NNT.biases = map fst deltas}
  where
    deltas =
      map (\(o, t) -> outputNeuronGradient o t (acts p)) $ zip (acts l) ts

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
  -> NNT.Layer
hiddenLayerGradient is ds wss os =
  NNT.Layer {NNT.biases = bds, NNT.weights = wdss}
  where
    wsst = transpose wss
    op (o, ws) acc = hiddenNeuronGradient o is ds ws : acc
    (bds, wdss) = unzip . foldr op [] $ zip os wsst

evalBackwards' :: NetZip -> Inputs -> NetworkDiff -> NetworkDiff
evalBackwards' nz is acc@(lg:_)
  | beginp nz = hiddenLayerGradient is deltas wss as : acc
  | otherwise =
    let pas = acts $ scursor nz
        nxtZ = goShallower nz
        grad = hiddenLayerGradient pas deltas wss as
     in evalBackwards' nxtZ is (grad : acc)
  where
    deltas = NNT.biases lg
    wss = weights $ dcursor nz
    as = acts $ cursor nz

evalBackwards :: NetZip -> Inputs -> Targets -> NetworkDiff
evalBackwards nz is ts = evalBackwards' (goShallower nz1) is [olg]
  where
    nz1 = goShallower $ end nz
    ol = cursor nz1
    pl = scursor nz1
    olg = outputLayerGradient ol pl ts
