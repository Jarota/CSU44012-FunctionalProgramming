module Main where

import Lib

main :: IO ()
main = do
    let dimensions = (5, 5)
    let bombs = makeBombList dimensions 5
    print bombs
    let grid = buildGrid dimensions bombs
    print grid
