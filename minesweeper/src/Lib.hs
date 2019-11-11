module Lib ( BoardState (..), play, gameOver, printBoard, validMove, makeMove, makeBombList, genBombCoords, convertToCoords ) where

import Data.Array
import Data.List
import Data.Char
import System.Random


data BoardState = Board (Int, Int) [(Int, Int)] [(Int, Int)] [(Int, Int)]

play :: BoardState -> IO ()
play board | gameOver board = putStrLn "Game Over!"

play board = do
    printBoard board
    putStrLn "What is your move? f(i,j) / c(i,j)"
    move <- getLine
    if not (validMove move)
        then do
            putStrLn "That is not a valid move."
            play board
        else
            play (makeMove board move)


printBoard :: BoardState -> IO ()
printBoard (Board dims bombs flags cleared) = do
    putStrLn "Here is the board..."


validMove :: String -> Bool
validMove move  | len /= 6 = False
                | action == 'f' = True
                | action == 'c' = True
                | otherwise = False
                where
                    action = head move
                    len = length move


makeMove :: BoardState -> String -> BoardState
makeMove (Board dimensions bombs flags cleared) move
            | action == 'f' = Board dimensions bombs (coord:flags) cleared
            | action == 'c' = Board dimensions bombs flags (coord:cleared)
            where
                action = head move
                coord = ((digitToInt (move!!2)), (digitToInt (move!!4)))


gameOver :: BoardState -> Bool
gameOver (Board _ bombs _ cleared)  | length (intersect bombs cleared) /= 0 = True
                                    | otherwise = False



makeBombList :: (Int, Int) -> Int -> [(Int, Int)]
{-
    Input:
        A tuple with the number of rows and columns in the grid
        The number of bomb coordinates to generate

    Output:
        A list of bomb coordinates
-}
makeBombList dimensions numBombs = genBombCoords dimensions numBombs []

genBombCoords :: (Int, Int) -> Int -> [(Int, Int)] -> [(Int, Int)]
{-
    Input:
        A tuple with the number of rows and columns in the grid
        The number of bombs left to generate
        A list of the bombs so far

    Output:
        A list of bomb coordinates
-}
genBombCoords (r, c) n bombsSoFar
                            | numBombs >= n = take n bombsSoFar
                            | otherwise = genBombCoords (r, c) n ( nub (newBombs ++ bombsSoFar) )
                            where
                                numBombs = length bombsSoFar
                                g = mkStdGen numBombs
                                max = (r*c) - 1
                                newBombs = convertToCoords (take n (randomRs (0, max) g)) (r, c)

convertToCoords :: [Int] -> (Int, Int) -> [(Int, Int)]
{-
    Input:
        A list of numbers
        A tuple containing the bounds of the grid

    Output:
        A list of tuples that are the coordinates the input list corresponds to
-}
convertToCoords [] _ = []
convertToCoords (x:xs) (r, c) = (i, j):(convertToCoords xs (r, c))
                        where
                            i = 1 + div x c
                            j = 1 + rem x r
