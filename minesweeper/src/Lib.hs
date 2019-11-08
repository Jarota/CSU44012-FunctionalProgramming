module Lib ( buildGrid, makeBombList, genBombCoords ) where

import Data.Array
import Data.List
import System.Random


buildGrid :: (Int, Int) -> [(Int, Int)] -> Array (Int, Int) Bool
{-
    Input:
        A tuple with the number of rows and columns to be in the grid
        A list of bomb coordinates

    Output:
        A grid of booleans, where True denotes the presence of a bomb
-}
buildGrid (r, c) bombs = array ((1, 1), (r, c)) [ ((i, j), ( (i, j) `elem` bombs )) | i <- [1 .. r], j <- [1 .. c]]


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
convertToCoords [] _ = []
convertToCoords (x:xs) (r, c) = (i, j):(convertToCoords xs (r, c))
                        where
                            i = 1 + div x c
                            j = 1 + rem x r
