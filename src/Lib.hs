module Lib
    ( someFunc
    ) where

someMsg :: String
someMsg = "someMsg"

someFunc :: IO ()
someFunc = putStrLn $ someMsg
