module Main where

import Lib

main :: IO ()
main = do
    putStrLn "How many rows?"
    rows <- getLine
    let r = read rows

    putStrLn "How many columns?"
    cols <- getLine
    let c = read cols

    putStrLn "How many bombs?"
    bombs <- getLine
    let numBombs = read bombs

    let board = makeBoard (r, c) numBombs
    let game = GS board numBombs Playing

    play game
