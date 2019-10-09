module Lib
    ( merge,
    split,
    mergeSort,
    mergeSortPar,
    mergeSortParPseq,
    quickSort,
    quickSortPar,
    quickSortParPseq
    ) where

import Control.Parallel

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

mergeSortPar :: (Ord a) => [a] -> [a]
mergeSortPar xs
    | (length xs) > 1 = par lows (merge (lows) (highs))
    | otherwise = xs
    where lows = mergeSortPar ls
          highs = mergeSortPar rs
          (ls, rs) = split xs

mergeSortParPseq :: (Ord a) => [a] -> [a]
mergeSortParPseq xs
    | (length xs) > 1 = par lows (pseq highs (merge (lows) (highs)))
    | otherwise = xs
    where lows = mergeSortParPseq ls
          highs = mergeSortParPseq rs
          (ls, rs) = split xs

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort [y | y <- xs, y <= x] ++ [x] ++ quickSort [y | y <- xs, y > x]

quickSortPar :: (Ord a) => [a] -> [a]
quickSortPar [] = []
quickSortPar (x:xs) = par lows (lows ++ [x] ++ highs)
                    where lows = quickSortPar [y | y <- xs, y <= x]
                          highs = quickSortPar [y | y <- xs, y > x]

quickSortParPseq :: (Ord a) => [a] -> [a]
quickSortParPseq [] = []
quickSortParPseq (x:xs) = par lows (pseq highs (lows ++ [x] ++ highs))
                       where lows = quickSortParPseq [y | y <- xs, y <= x]
                             highs = quickSortParPseq [y | y <- xs, y > x]
