module FeedForward
  ( nextLayer
  , feedForward
  ) where

import           Neuron
import           NNTypes

nextLayer :: Neuron -> Activations -> [Weights] -> [Bias] -> Activations
nextLayer pfn as wss bs = foldr op [] $ zip wss bs
  where
    op (ws, b) acc = pfn ws b as : acc

feedForward' :: Neuron -> Activations -> [Layer] -> Activations
feedForward' n as [] = as
feedForward' n as (Layer {weights = w, biases = b}:ls) = feedForward' n next ls
  where
    next = nextLayer n as w b

feedForward :: [Layer] -> [Double] -> [Double]
feedForward ls is = feedForward' perceptron is ls
