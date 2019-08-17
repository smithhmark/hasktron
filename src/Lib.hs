module Lib
  ( someFunc
  , someMsg
  , smoosh
  , logistic
  , perceptron
  , freePerceptron
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
