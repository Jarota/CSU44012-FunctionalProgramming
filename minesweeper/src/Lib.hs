module Lib ( Square (..), GameState (..), BoardState (..), play, printBoard, validMove, makeMove, makeBombList) where

import Data.Array
import Data.List
import Data.Char
import System.Random

data Square = Bomb      -- a bomb is in the square
            | Flag      -- a flag has been placed in the square
            | Clear Int -- the square has been cleared and has Int number of bombs around it
            | Empty     -- the square is empty
            deriving (Eq, Show)

data GameState  = Playing
                | Win
                | Lose

-- The dimensions of the board and a list of Squares, as well as the current state of the game
data BoardState = Board (Int, Int) [Square] GameState

play :: BoardState -> IO ()
play (Board _ _ Lose) = putStrLn "You Lose!"
play (Board _ _ Win) = putStrLn "You Win!"

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
printBoard (Board (r,c) squares _) = do
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
makeMove (Board (r, c) squares _) move = 
            | action == 'c' = Board (r, c) (clearSquare squares index) (Playing)
            | action == 'f' = Board (r, c) (flagSqaure squares index) (Playing)
            where
                action = head move
                (i, j) = ((digitToInt (move!!2)), (digitToInt (move!!4)))
                index = (i*r)+j

clearSquare :: [Square] -> Int -> [Square]


setSquare :: [Square] -> Int -> Square -> [Square]
{-
    Input:
        A list of Squares
        An index
        A new Square

    Output:
        A list of Squares with the square at the index has been set to the new Square
-}
setSquare squares i newSquare = let (ys,zs) = splitAt i-1 squares in
                                ys ++ newSquare ++ tail zs


makeBombList :: (Int, Int) -> Int -> [Int]
{-
    Input:
        A tuple with the number of rows and columns in the grid
        The number of bombs to generate

    Output:
        A list of bomb indices
-}
makeBombList (r, c) numBombs = makeBombs (r*c) numBombs []

makeBombs :: Int -> Int -> [Int] -> [Int]
{-
    Input:
        A maximum index
        The number of bombs left to generate
        A list of the bombs so far

    Output:
        A list of bomb indices
-}
genBombCoords maxIndex n bombsSoFar
                            | numBombs >= n = take n bombsSoFar
                            | otherwise = genBombCoords maxIndex n ( nub (newBombs ++ bombsSoFar) )
                            where
                                numBombs = length bombsSoFar
                                g = mkStdGen numBombs
                                newBombs =  take n (randomRs (0, maxIndex) g)
