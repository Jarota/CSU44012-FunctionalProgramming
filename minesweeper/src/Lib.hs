{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib ( GameState (..), Move (..), play, coordToIndex, gameStateToHtml, makeBoard ) where

import Data.Char
import Data.Aeson.TH (deriveJSON, defaultOptions)

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes

import Board

data GameState  = GS Board Int  -- a board with a number of flags left
                | Won           -- the game has been won
                | Lost          -- the game has been lost

$(deriveJSON defaultOptions ''GameState)

data Move   = ClearSquare Int
            | FlagSquare Int
            | Invalid
            deriving (Eq, Show)

play :: GameState -> Move -> GameState
play game move
        | checkForWin newGame   = Won
        | otherwise             = newGame
        where
            newGame = makeMove game move

checkForWin (GS board flagsLeft)    = checkForWin' board
checkForWin Won                     = True
checkForWin Lost                    = False

coordToIndex :: GameState -> (Int, Int) -> Int
coordToIndex (GS (Board dims _) _) coord = coordToIndex' dims coord

coordToIndex' :: (Int, Int) -> (Int, Int) -> Int
coordToIndex' (r, c) (i, j) = (i*c) + j


makeMove :: GameState -> Move -> GameState
makeMove game (ClearSquare i)   = clearSquare game i
makeMove game (FlagSquare i)    = flagSquare game i
makeMove game Invalid           = game

clearSquare :: GameState -> Int -> GameState
clearSquare (GS (Board (r, c) squares) flagsLeft) i
        | prevSquare == Bomb        = Lost
        | alreadyClear              = passGameState
        | prevSquare == FlagB       = passGameState
        | prevSquare == FlagE       = passGameState
        | adjBombs == 0             = clearAdjSquares newGameState adjIndices
        | otherwise                 = newGameState
        where
            board           = Board (r, c) squares
            prevSquare      = squareAt board i
            passGameState   = GS board flagsLeft
            alreadyClear    = any (prevSquare==) [(Clear 0), (Clear 1), (Clear 2), (Clear 3), (Clear 4), (Clear 5), (Clear 6), (Clear 7), (Clear 8)]
            adjIndices      = adjI (r, c) i
            adjBombs        = bombsInList board adjIndices
            newSquare       = Clear adjBombs
            newGameState    = GS (setSquare board i newSquare) flagsLeft

clearAdjSquares :: GameState -> [Int] -> GameState
clearAdjSquares game []     = game
clearAdjSquares game (i:is) = clearAdjSquares (newGameState) is
        where
            newGameState = clearSquare game i



flagSquare :: GameState -> Int -> GameState
flagSquare (GS board 0) i
        | prevSquare == FlagB   = GS (setSquare board i Bomb) 1
        | prevSquare == FlagE   = GS (setSquare board i Empty) 1
        | otherwise             = GS board 0
        where
            prevSquare = squareAt board i

flagSquare (GS board flagsLeft) i
        | prevSquare == Bomb    = GS (setSquare board i FlagB) (flagsLeft-1)
        | prevSquare == Empty   = GS (setSquare board i FlagE) (flagsLeft-1)
        | prevSquare == FlagB   = GS (setSquare board i Bomb) (flagsLeft+1)
        | prevSquare == FlagE   = GS (setSquare board i Empty) (flagsLeft+1)
        | otherwise             = GS board flagsLeft
        where
            prevSquare = squareAt board i

-- gameStateToHtml :: GameState ->
gameStateToHtml Lost                    = h1 "No Game State"
gameStateToHtml Won                     = h1 "No Game State"
gameStateToHtml (GS board flagsLeft)    = H.div $ do
    "Flags left: "
    toHtml flagsLeft
    br
    boardToHtml board
