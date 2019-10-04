module Main where

import Lib
import System.Random

main :: IO ()
main = do
    g <- getStdGen
    let list = take 20 (randomRs (0, 20) g :: [Int])
    print list
    let quicksorted = quickSort list
    print quicksorted
    let mergesorted = mergeSort list
    print mergesorted
