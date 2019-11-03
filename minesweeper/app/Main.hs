module Main where

import Lib

main :: IO ()
main = do
    let bombs = genBombCoords (10, 10) 0
    let grid = buildGrid (10, 10) bombs
    print grid
