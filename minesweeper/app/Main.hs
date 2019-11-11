module Main where

import Lib

main :: IO ()
main = do
    let dimensions = (2, 2)
    let bombs = makeBombList dimensions 1
    let board = Board dimensions bombs [] [] -- dimensions, bombs, flags, cleared
    play board
