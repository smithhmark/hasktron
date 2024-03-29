module NNTypes
  ( Weight
  , Weights
  , Bias
  , Biases
  , Activation
  , Activations
  , Layer(..)
  ) where

type Weight = Double

type Activation = Double

type Weights = [Double]

type Activations = [Double]

type Bias = Double

type Biases = [Double]

data Layer = Layer
  { weights :: [Weights]
  , biases  :: Biases
  } deriving (Show)
