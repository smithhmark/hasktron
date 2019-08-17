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

logistic :: Double -> Double
logistic x = e / d
  where
    e = exp x
    d = e + 1

smoosh :: Double -> Double
smoosh = undefined

perceptron :: [Double] -> Double -> [Double] -> Double
perceptron ws b as = logistic $ freePerceptron ws b as

freePerceptron :: [Double] -> Double -> [Double] -> Double
freePerceptron ws b as = s - b
  where
    s = sum $ zipWith (*) ws as


nextLayer :: ([Double] -> Double -> [Double] -> Double) -> [Double] -> [[Double]] -> [Double] -> [Double]
nextLayer pfn as wss bs = foldr op [] $ zip wss bs
  where
          op (ws, b) acc = pfn ws b as : acc
