module Main where

import qualified Data.Foldable

main :: IO ()
main = Data.Foldable.forM_ [1..100] (putStrLn . fizzBuzz)

fizzBuzz :: Int -> String
fizzBuzz i
    | i `mod` 15 == 0 = "FizzBuzz"
    | i `mod` 5 == 0 = "Buzz"
    | i `mod` 3 == 0 = "Fizz"
    | otherwise = show i
