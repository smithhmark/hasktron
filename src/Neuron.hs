module Neuron
  ( logistic
  , softmax
  , logisticPrime
  , perceptron
  , freePerceptron
  , Neuron
  , quadraticCost
  ) where

import           NNTypes

type Neuron = Weights -> Bias -> Activations -> Double

logisticFull :: Double -> Double -> Double -> Double -> Double
logisticFull max k x0 x = max / d
  where
    e = exp (-1 * k * (x - x0))
    d = 1 + e

softmax :: [Double] -> [Double]
softmax xs = map (/ s) es
  where
    es = map exp xs
    s = sum es

logistic :: Double -> Double
logistic x = logisticFull 1.0 1.0 0.0 x

logisticPrime :: Double -> Double
logisticPrime x = x * (1 - x)

perceptron :: Neuron
perceptron ws b as = logistic $ freePerceptron ws b as

freePerceptron :: Neuron
freePerceptron ws b as = s - b
  where
    s = sum $ zipWith (*) ws as

quadraticCost :: [Double] -> Activations -> Double
quadraticCost ys as = ss / 2.0
  where
    ss = sum . map (** 2) $ zipWith (-) ys as
