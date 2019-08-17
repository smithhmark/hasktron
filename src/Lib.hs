module Lib
  ( someFunc
  , someMsg
  , smoosh
  ) where

someMsg :: String
someMsg = "someMsg"

someFunc :: IO ()
someFunc = putStrLn $ someMsg

smoosh :: Float -> Float
smoosh = undefined

perceptron :: [Float] -> Float -> [Float] -> Float
perceptron ws b as = b - s
  where
    s = sum $ zipWith (*) ws as
