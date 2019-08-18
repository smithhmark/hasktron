module Lib
  ( someFunc
  , someMsg
  , smoosh
  , logistic
  , perceptron
  , freePerceptron
  , nextLayer
  ) where

someMsg :: String
someMsg = "someMsg"

someFunc :: IO ()
someFunc = putStrLn someMsg

logisticFull :: Double -> Double -> Double -> Double -> Double
logisticFull max k x0 x = max / d
  where
    e = exp (-1 * k * (x - x0))
    d = 1 + e

logistic :: Double -> Double
logistic x = logisticFull 1.0 1.0 0.0 x

smoosh :: Double -> Double
smoosh = undefined

type Neuron = [Double] -> Double -> [Double] -> Double

perceptron :: Neuron
perceptron ws b as = logistic $ freePerceptron ws b as

freePerceptron :: Neuron
freePerceptron ws b as = s - b
  where
    s = sum $ zipWith (*) ws as

nextLayer :: Neuron -> [Double] -> [[Double]] -> [Double] -> [Double]
nextLayer pfn as wss bs = foldr op [] $ zip wss bs
  where
    op (ws, b) acc = pfn ws b as : acc
