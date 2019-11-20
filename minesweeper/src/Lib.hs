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

play (GS board flagsLeft _)
        | checkForWin board = play (GS board flagsLeft Win)
        | otherwise         = do
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
            gameString  = "You have " ++ (show flagsLeft) ++ " flags left.\n" ++ boardString


stringToMove :: Board -> String -> Move
stringToMove (Board (r, c) _) moveString
        | (length moveString) /= 6  = Invalid
        | index == (r*c)            = Invalid
        | action == 'f'             = FlagSqaure index
        | action == 'c'             = ClearSquare index
        | otherwise                 = Invalid
        where
            action  = head moveString
            i       = digitToInt (moveString!!2)
            j       = digitToInt (moveString!!4)
            index   = validIndex (r, c) (i, j)

validIndex :: (Int, Int) -> (Int, Int) -> Int
validIndex (r, c) (i, j)
        | i < r && i >= 0 && j < c && j >= 0  = (i*c) + j
        | otherwise                           = r*c

makeMove :: GameState -> Move -> GameState
makeMove game (ClearSquare i) = clearSquare game i
makeMove game (FlagSqaure i)  = flagSquare game i

clearSquare :: GameState -> Int -> GameState
clearSquare (GS (Board (r, c) squares) flagsLeft _) i
        | prevSquare == Bomb        = GS board flagsLeft Lose
        | alreadyClear              = passGameState
        | prevSquare == FlagB       = passGameState
        | prevSquare == FlagE       = passGameState
        | adjBombs == 0             = clearAdjSquares newGameState adjIndices
        | otherwise                 = newGameState
        where
            board           = Board (r, c) squares
            prevSquare      = squareAt board i
            passGameState   = GS board flagsLeft Playing
            alreadyClear    = any (prevSquare==) [(Clear 0), (Clear 1), (Clear 2), (Clear 3), (Clear 4), (Clear 5), (Clear 6), (Clear 7), (Clear 8)]
            adjIndices      = adjI (r, c) i
            adjBombs        = bombsInList board adjIndices
            newSquare       = Clear adjBombs
            newGameState    = GS (setSquare board i newSquare) flagsLeft Playing

clearAdjSquares :: GameState -> [Int] -> GameState
clearAdjSquares game []     = game
clearAdjSquares game (i:is) = clearAdjSquares (newGameState) is
        where
            newGameState = clearSquare game i



flagSquare :: GameState -> Int -> GameState
flagSquare (GS board 0 _) i
        | prevSquare == FlagB   = GS (setSquare board i Bomb) 1 Playing
        | prevSquare == FlagE   = GS (setSquare board i Empty) 1 Playing
        | otherwise             = GS board 0 Playing
        where
            prevSquare = squareAt board i

flagSquare (GS board flagsLeft _) i
        | prevSquare == Bomb    = GS (setSquare board i FlagB) (flagsLeft-1) Playing
        | prevSquare == Empty   = GS (setSquare board i FlagE) (flagsLeft-1) Playing
        | prevSquare == FlagB   = GS (setSquare board i Bomb) (flagsLeft+1) Playing
        | prevSquare == FlagE   = GS (setSquare board i Empty) (flagsLeft+1) Playing
        | otherwise             = GS board flagsLeft Playing
        where
            prevSquare = squareAt board i
