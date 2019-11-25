{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Board (Square (..), Board (..), boardToHtml, bombsInList, adjI, makeBoard, setSquare, squareAt, checkForWin') where

import System.Random
import Data.List
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes

data Square = Bomb      -- a bomb is in the square
            | Empty     -- the square is Empty i.e. untouched so far
            | FlagB     -- a flag has been placed on a bomb
            | FlagE     -- a flag has been placed on an empty square
            | Clear Int -- the square has been cleared and has Int number of bombs around it
            deriving (Eq, Show)

$(deriveJSON defaultOptions ''Square)

-- board dimensions and corresponding list of squares
data Board = Board (Int, Int) [Square]

$(deriveJSON defaultOptions ''Board)


-- boardToHtml :: Board ->
boardToHtml (Board (r, c) squares) = table $ rowsToHtml (r, c) squares 0

-- rowsToHtml :: dimension squares currentIndex
rowsToHtml (r, c) squares i | i == r*c  = return ()
                            | otherwise = do
                                            tr $ dataToHtml c squares i
                                            rowsToHtml (r, c) squares (i+c)

dataToHtml c squares i  | i `mod` c == c-1 = s
                        | otherwise         = do
                                                s
                                                dataToHtml c squares (i+1)
                        where
                            s = squareToHtml (squares!!i)

squareToHtml (Clear x)
    | x /= 0            = td ! class_ "clearNum" $ toHtml $ show x
    | otherwise         = td ! class_ "clear" $ toHtml $ (" " :: String)
squareToHtml FlagE      = td ! class_ "flag" $ toHtml $ ("F" :: String)
squareToHtml FlagB      = td ! class_ "flag" $ toHtml $ ("F" :: String)
squareToHtml _          = td ! class_ "empty" $ toHtml $ (" " :: String)

{-
    Takes in a board and an index
    Outputs the number of bombs touching the square at that index
-}
bombsInList :: Board -> [Int] -> Int
bombsInList (Board (r, c) squares) indices = length (filter (\s -> s == Bomb || s == FlagB) adjacentSquares)
        where
            adjacentSquares = subsetOfSquares squares indices

adjI :: (Int, Int) -> Int -> [Int]
adjI (r, c) i   = prevRow ++ left ++ right ++ nextRow
                where
                    prevRow = appendRowIf (i-c >= 0) c (i-c)
                    nextRow = appendRowIf (i+c < r*c) c (i+c)
                    left    = appendElemIf (i `mod` c /= 0) (i-1)
                    right   = appendElemIf (i `mod` c /= c-1) (i+1)

appendRowIf :: Bool -> Int -> Int -> [Int]
appendRowIf inBounds c i
    | inBounds  = left ++ [i] ++ right
    | otherwise = []
    where
        left  = appendElemIf (i `mod` c /= 0) (i-1)
        right = appendElemIf (i `mod` c /= c-1) (i+1)

appendElemIf :: Bool -> Int -> [Int]
appendElemIf inBounds i
    | inBounds  = [i]
    | otherwise = []

subsetOfSquares :: [Square] -> [Int] -> [Square]
subsetOfSquares _ []            = []
subsetOfSquares squares (i:is)  = (squares!!i):(subsetOfSquares squares is)


squareAt :: Board -> Int -> Square
squareAt (Board dims squares) i = squares!!i

{-
    Takes in a board, an integer, and a new square
    Outputs a new board where the square at i is the newSquare
-}
setSquare :: Board -> Int -> Square -> Board
setSquare (Board dims squares) i newSquare =
        let (ys,zs) = splitAt i squares in
            Board dims (ys ++ [newSquare] ++ tail zs)

{-
    Takes in the dimensions and a number of bombs
    Outputs a board with the bombs randomly placed on it
-}
makeBoard :: (Int, Int) -> Int -> Board
makeBoard dims numBombs = Board dims squares
            where
                bombs   = makeBombList dims numBombs
                squares = makeSquareList dims bombs

makeSquareList :: (Int, Int) -> [Int] -> [Square]
makeSquareList (r, c) bombs = makeSquareList' ((r*c)-1) bombs

makeSquareList' :: Int -> [Int] -> [Square]
makeSquareList' i bombs | i < 0             = []
                        | i `elem` bombs    = Bomb:(makeSquareList' (i-1) bombs)
                        | otherwise         = Empty:(makeSquareList' (i-1) bombs)

makeBombList :: (Int, Int) -> Int -> [Int]
makeBombList (r, c) numBombs = makeBombList' (r*c) numBombs []

makeBombList' :: Int -> Int -> [Int] -> [Int]
makeBombList' maxIndex n bombsSoFar
            | numBombs >= n    = take n bombsSoFar
            | otherwise        = makeBombList' maxIndex n ( nub (newBombs ++ bombsSoFar) )
            where
                numBombs = length bombsSoFar
                g        = mkStdGen numBombs
                newBombs =  take n (randomRs (0, maxIndex-1) g)


checkForWin' :: Board -> Bool
checkForWin' (Board _ squares) = (Empty `notElem` squares) && (FlagE `notElem` squares)
