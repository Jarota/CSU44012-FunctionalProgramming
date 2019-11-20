module Lib ( GameState (..), Condition (..), play, makeBoard ) where

import Data.Char
import Board

data Condition  = Playing
                | Win
                | Lose
{-
    GameState comprises of:
        The Board
        Number of flags left
        The current condition of the game
-}
data GameState = GS Board Int Condition

data Move   = ClearSquare Int
            | FlagSqaure Int
            | Invalid
            deriving Eq

play :: GameState -> IO ()
play (GS _ _ Lose) = putStrLn "You Lose!"
play (GS _ _ Win) = putStrLn "You Win!"

play (GS board flagsLeft _) = do
        printGame (GS board flagsLeft Playing)
        putStrLn "What is your move? f(i,j) / c(i,j)"
        moveString <- getLine
        let move = stringToMove board moveString
        if move == Invalid
            then do
                putStrLn "That is not a valid move."
                play (GS board flagsLeft Playing)
            else
                play (makeMove (GS board flagsLeft Playing) move)

printGame :: GameState -> IO ()
printGame (GS board flagsLeft _) = putStrLn gameString
        where
            boardString = buildBoardString board
            gameString = "You have " ++ (show flagsLeft) ++ " flags left.\n" ++ boardString


stringToMove :: Board -> String -> Move
stringToMove (Board (r, c) _) moveString
        | (length moveString) /= 6  = Invalid
        | index == 0                = Invalid
        | action == 'f'             = FlagSqaure index
        | action == 'c'             = ClearSquare index
        | otherwise                 = Invalid
        where
            action = head moveString
            i = digitToInt (moveString!!2)
            j = digitToInt (moveString!!4)
            index = validIndex (r, c) (i, j)

validIndex :: (Int, Int) -> (Int, Int) -> Int
validIndex (r, c) (i, j)
        | i <= r && i > 0 && j <= c && j > 0    = (i*r) + j
        | otherwise                             = 0

makeMove :: GameState -> Move -> GameState
makeMove game (ClearSquare i) = clearSquare game i
makeMove game (FlagSqaure i)  = flagSquare game i

clearSquare :: GameState -> Int -> GameState
clearSquare (GS board flagsLeft _) i
        | prevSquare == Bomb      = GS board flagsLeft Lose
        | prevSquare == Covered   = GS (setSquare board i newSquare) flagsLeft Playing
        | otherwise         = GS board flagsLeft Playing
        where
            prevSquare = squareAt board i
            newSquare = Clear (bombsTouching board i)

flagSquare :: GameState -> Int -> GameState
flagSquare (GS board flagsLeft _) i
        | prevSquare == Bomb    = newGameState
        | prevSquare == Covered = newGameState
        | otherwise             = GS board flagsLeft Playing
        where
            prevSquare = squareAt board i
            newGameState = GS (setSquare board i Flag) (flagsLeft-1) Playing
