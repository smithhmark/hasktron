module FeedForward
  ( nextLayer
  ) where

import           Neuron
import           NNTypes

nextLayer :: Neuron -> Activations -> [Weights] -> [Bias] -> Activations
nextLayer pfn as wss bs = foldr op [] $ zip wss bs
  where
    op (ws, b) acc = pfn ws b as : acc
