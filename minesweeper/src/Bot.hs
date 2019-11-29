
module Bot (autoMove) where

import Data.List

import Board
import Lib

boardToBotBoard :: Board -> Board
boardToBotBoard (Board dims squares) = Board dims newSquares
    where
        newSquares = map hideSquare squares

hideSquare :: Square -> Square
hideSquare (Clear n)            = Clear n
hideSquare s    | s == FlagB    = Flag
                | s == FlagE    = Flag
                | otherwise     = Unknown


autoMove :: GameState -> Move
autoMove (GS board _)
        | move /= Invalid   = move
        | otherwise         = ClearSquare guess
        where
            hiddenBoard = boardToBotBoard board
            move        = autoMove' hiddenBoard 0
            guess       = findUnknown' hiddenBoard

autoMove' :: Board -> Int -> Move
autoMove' (Board (r, c) squares) i
        | i == err                                          = Invalid
        | (squares!!i == Flag) || (squares!!i == Unknown)   = recurse
        | obviousFlags squares i adjIndices                 = FlagSquare nextUnknown
        | obviousClears squares i adjIndices                = ClearSquare nextUnknown
        | oneOneIndex /= err                                = ClearSquare oneOneIndex
        | oneTwoIndex /= err                                = FlagSquare oneTwoIndex
        | otherwise                                         = recurse
        where
            recurse     = autoMove' (Board (r, c) squares) (i+1)
            err         = length squares
            adjIndices  = adjI (r, c) i
            nextUnknown = findUnknown squares adjIndices
            oneOneIndex = basicPattern (Board (r, c) squares) i 1
            oneTwoIndex = basicPattern (Board (r, c) squares) i 2


findUnknown' :: Board -> Int
findUnknown' (Board _ squares) = findUnknown squares is
        where
            is = take (length squares) [0..]

findUnknown :: [Square] -> [Int] -> Int
findUnknown squares [] = 0
findUnknown squares (i:is)  | squares!!i == Unknown = i
                            | otherwise             = findUnknown squares is


obviousFlags :: [Square] -> Int -> [Int] -> Bool
obviousFlags squares i is = (us == x - fs) && us /= 0
        where
            x   = clearNum (squares!!i)
            us  = (length $ filter (\j -> squares!!j == Unknown) is)
            fs  = (length $ filter (\j -> squares!!j == Flag) is)

obviousClears :: [Square] -> Int -> [Int] -> Bool
obviousClears squares i is
        | (squares!!i /= Flag) || (squares!!i /= Unknown) = (x == fs) && fs /= 0 && us /= 0
        where
            x   = clearNum (squares!!i)
            us  = (length $ filter (\j -> squares!!j == Unknown) is)
            fs  = (length $ filter (\j -> squares!!j == Flag) is)

-- Check for the one-one pattern
basicPattern :: Board -> Int -> Int -> Int
basicPattern (Board (r, c) squares) i k
        | not isBasic       = err
        | (length us /= 3)  = err
        | not twoUsAdj      = err
        | j == err          = err
        | not oneOneFound   = err
        | otherwise         = index
        where
            err             = length squares
            is              = adjI (r, c) i
            fs              = getFlags squares is
            n               = clearNum (squares!!i)
            isBasic         = n - (length fs) == k
            us              = getUnknowns squares is
            twoUsAdj        = twoAdj us c
            is'             = filter (\j -> isAdj i j c) is
            j               = halfBasic (Board (r, c) squares) is' k
            js              = adjI (r, c) j
            us'             = getUnknowns squares js
            oneOneFound     = length ( intersect us us' ) == 2
            index           = getOutlier us us'


halfBasic :: Board -> [Int] -> Int -> Int
halfBasic (Board _ squares) [] _ = length squares
halfBasic board (i:is) k
        | halfBasic' board i k  = i
        | otherwise             = halfBasic board is k

halfBasic' :: Board -> Int -> Int -> Bool
halfBasic' (Board (r, c) squares) i k
        | not isBasic                   = False
        | (k == 1) && (length us /= 2)  = False
        | not (isAdj x y c)             = False
        | otherwise                     = True
        where
            is      = adjI (r, c) i
            fs      = getFlags squares is
            n       = clearNum (squares!!i)
            isBasic = n - (length fs) == 1
            us      = getUnknowns squares is
            x       = head us
            y       = last us

twoAdj :: [Int] -> Int -> Bool
twoAdj (x:y:z:[]) c
        | isAdj x y c = True
        | isAdj x z c = True
        | isAdj y z c = True
        | otherwise = False

twoAdj _ _ = False

isAdj :: Int -> Int -> Int -> Bool
isAdj x y c = diff == 1 || diff == c
        where
            diff = abs (x-y)

getUnknowns :: [Square] -> [Int] -> [Int]
getUnknowns squares is = filter (\j -> squares!!j == Unknown) is

getFlags :: [Square] -> [Int] -> [Int]
getFlags squares is = filter (\j -> squares!!j == Flag) is

getOutlier :: [Int] -> [Int] -> Int
getOutlier is js = head ( is \\ js )
