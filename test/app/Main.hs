{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Aeson (decode, encode)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import qualified Data.ByteString.Lazy.Char8 as BL

data Square = Bomb      -- a bomb is in the square
            | Empty     -- the square is Empty i.e. untouched so far
            | FlagB     -- a flag has been placed on a bomb
            | FlagE     -- a flag has been placed on an empty square
            | Clear Int -- the square has been cleared and has Int number of bombs around it
            deriving Eq

$(deriveJSON defaultOptions ''Square)

main :: IO ()
main = do
    let stuff = Clear 3
    let encodedStuff = encode stuff
    BL.writeFile "stuff.json" encodedStuff
    let x = 0
    if x == 1
        then putStrLn "X is 1"
        else putStrLn "X is not 1"

    putStrLn "What are the odds?"
