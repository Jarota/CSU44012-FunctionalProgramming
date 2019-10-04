module Lib
    ( merge,
    split,
    mergeSort,
    quickSort
    ) where

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys)
    | (x < y) = x:merge xs (y:ys)
    | otherwise = y:merge (x:xs) ys

split :: [a] -> ([a], [a])
split xs = (take n xs, drop n xs)
    where n = (length xs) `div` 2

mergeSort :: (Ord a) => [a] -> [a]
mergeSort xs
    | (length xs) > 1 = merge (mergeSort ls) (mergeSort rs)
    | otherwise = xs
    where (ls, rs) = split xs

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort [y | y <- xs, y <= x] ++ [x] ++ quickSort [y | y <- xs, y > x]
