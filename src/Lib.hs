module Lib
    ( someFunc
    , someMsg
    ) where

someMsg :: String
someMsg = "someMsg"

someFunc :: IO ()
someFunc = putStrLn $ someMsg
