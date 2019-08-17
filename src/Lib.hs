module Lib
  ( someFunc
  , someMsg
  , smoosh
  , logistic
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
perceptron ws b as = b - s
  where
    s = sum $ zipWith (*) ws as
