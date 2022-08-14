module Main where

import qualified MyLib (someFunc)

main :: IO ()
main = do
  putStrLn "Main, checking in!"
  MyLib.someFunc
