module Board (Square (..), Board (..), buildBoardString, bombsTouching, makeBoard, setSquare, squareAt) where

import System.Random
import Data.List

data Square = Bomb      -- a bomb is in the square
            | Covered     -- the square is covered i.e. untouched so far
            | Flag      -- a flag has been placed in the square
            | Clear Int -- the square has been cleared and has Int number of bombs around it
            deriving Eq

-- board dimensions and corresponding list of squares
data Board = Board (Int, Int) [Square]

buildBoardString :: Board -> String
buildBoardString (Board (r, c) squares) = buildBoardString' c (r*c) squares

{-
    Takes in the length of a row, the current index, and a list of squares
    Outputs a string
-}
buildBoardString' :: Int -> Int -> [Square] -> String
buildBoardString' _ 0 _ = "\n"
buildBoardString' c i squares   | i `div` c == 0    = next ++ s ++ "\n"
                                | otherwise         = next ++ s
                                where
                                    next = buildBoardString' c (i-1) squares
                                    s = squareString (squares!!i)

squareString :: Square -> String
squaresString (Clear x)   = show x
squareString Flag       = "F"
squareString _          = "."

{-
    Takes in a board and an index
    Outputs the number of bombs touching the square at that index
-}
bombsTouching :: Board -> Int -> Int
bombsTouching (Board (r, c) squares) i = length (filter (\s -> s == Bomb) adjacentSquares)
        where
            adjacentIndices = adjI (r, c) i
            adjacentSquares = adjS squares adjacentIndices

adjI :: (Int, Int) -> Int -> [Int]
adjI (r, c) i   = prevRow ++ left ++ right ++ nextRow
                where
                    prevRow = appendRowIf (i - c > 0) c (i-c)
                    nextRow = appendRowIf (i + c > r * c) c (i+c)
                    left = appendElemIf (i `div` c == 1) (i-1)
                    right = appendElemIf (i `div` c == 0) (i+1)

appendRowIf :: Bool -> Int -> Int -> [Int]
appendRowIf inBounds c i
    | inBounds  = left ++ [i] ++ right
    | otherwise = []
    where
        left = appendElemIf (i `div` c == 1) (i-1)
        right = appendElemIf (i `div` c == 0) (i+1)

appendElemIf :: Bool -> Int -> [Int]
appendElemIf inBounds i
    | inBounds  = [i]
    | otherwise = []

adjS :: [Square] -> [Int] -> [Square]
adjS _ [] = []
adjS squares (i:is) = (squares!!i):(adjS squares is)


squareAt :: Board -> Int -> Square
squareAt (Board dims squares) i = squares!!i

{-
    Takes in a board, an integer, and a new square
    Outputs a new board where the square at i is the newSquare
-}
setSquare :: Board -> Int -> Square -> Board
setSquare (Board dims squares) i newSquare =
        let (ys,zs) = splitAt (i-1) squares in
            Board dims (ys ++ [newSquare] ++ tail zs)

{-
    Takes in the dimensions and a number of bombs
    Outputs a board with the bombs randomly placed on it
-}
makeBoard :: (Int, Int) -> Int -> Board
makeBoard dims numBombs = Board dims squares
            where
                bombs = makeBombList dims numBombs
                squares = makeSquareList dims bombs

makeSquareList :: (Int, Int) -> [Int] -> [Square]
makeSquareList (r, c) bombs = makeSquareList' (r*c) bombs

makeSquareList' :: Int -> [Int] -> [Square]
makeSquareList' 0 _     = []
makeSquareList' i bombs | i `elem` bombs    = Bomb:(makeSquareList' (i-1) bombs)
                        | otherwise         = Covered:(makeSquareList' (i-1) bombs)

makeBombList :: (Int, Int) -> Int -> [Int]
makeBombList (r, c) numBombs = makeBombList' (r*c) numBombs []

makeBombList' :: Int -> Int -> [Int] -> [Int]
makeBombList' maxIndex n bombsSoFar
            | numBombs >= n    = take n bombsSoFar
            | otherwise                 = makeBombList' maxIndex n ( nub (newBombs ++ bombsSoFar) )
            where
                numBombs = length bombsSoFar
                g = mkStdGen numBombs
                newBombs =  take n (randomRs (1, maxIndex+1) g)
