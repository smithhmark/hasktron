module Lib
  ( someFunc
  , someMsg
  , smoosh
  , logistic
  , perceptron
  , free_perceptron
  ) where

someMsg :: String
someMsg = "someMsg"

someFunc :: IO ()
someFunc = putStrLn $ someMsg

logistic :: Double -> Double
logistic x = e / d
  where
    e = exp x
    d = e + 1

smoosh :: Double -> Double
smoosh = undefined

perceptron :: [Double] -> Double -> [Double] -> Double
perceptron ws b as = logistic $ free_perceptron ws b as

free_perceptron :: [Double] -> Double -> [Double] -> Double
free_perceptron ws b as = s - b
  where
    s = sum $ zipWith (*) ws as
