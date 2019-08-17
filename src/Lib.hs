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

logistic :: Float -> Float
logistic x = e / d
  where
    e = exp x
    d = e + 1

smoosh :: Float -> Float
smoosh = undefined

perceptron :: [Float] -> Float -> [Float] -> Float
perceptron ws b as = b - s
  where
    s = sum $ zipWith (*) ws as
