module Lib ( buildGrid, genBombCoords ) where

import Data.Array


buildGrid :: (Int, Int) -> [(Int, Int)] -> Array (Int, Int) Bool
{-
    Input:
        A tuple with the number of rows and columns to be in the grid
        A list of bomb coordinates

    Output:
        A grid of booleans, where True denotes the presence of a bomb
-}
-- buildGrid r c = take r (repeat (take c (repeat False)))
buildGrid (r, c) bombs = array ((1, 1), (r, c)) [ ((i, j), ( (i, j) `elem` bombs )) | i <- [1 .. r], j <- [1 .. c]]


genBombCoords :: (Int, Int) -> Int -> [(Int, Int)]
{-
    Input:
        A tuple with the number of rows and columns in the grid
        The number of bomb coordinates to generate

    Output:
        A list of bomb coordinates
-}
genBombCoords _ _ = [(4, 4)]
