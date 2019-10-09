module Main where

import Lib
import System.Random

main :: IO ()
main = do
    g <- getStdGen
    let list = take 10000 (randomRs (0, 1000) g :: [Int])
    print list
    let sorted = mergeSort list
    print sorted

-- ./.stack-work/dist/x86_64-linux-tinfo6/Cabal-2.4.0.1/build/assignment1-exe/assignment1-exe +RTS -ls -N4
