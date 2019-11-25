{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import qualified Layout

import Control.Monad.IO.Class

import Web.Scotty as S
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text

import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe

blaze = S.html . renderHtml


main :: IO ()
main = scotty 3000 $ do
    get "/" $ do
        blaze Layout.root

    get "/play" $ do
        rStr <- param "rows"
        cStr <- param "cols"
        bombsStr <- param "bombs"
        let r = read rStr
        let c = read cStr
        let bombs = read bombsStr
        let game = GS (makeBoard (r, c) bombs) bombs
        let gameHtml = gameStateToHtml game
        blaze (Layout.play gameHtml) -- pass the html for displaying the grid
        storeGameState game

    get "/play/flag" $ do
        iStr <- param "i"
        jStr <- param "j"
        let i = read iStr
        let j = read jStr
        gameJSON <- loadGameState
        let game = fromMaybe Lost (decode gameJSON :: Maybe GameState)
        let index = coordToIndex game (i, j)
        let move = FlagSquare index
        let newGame = play game move
        renderGame newGame

    get "/play/clear" $ do
        iStr <- param "i"
        jStr <- param "j"
        let i = read iStr
        let j = read jStr
        gameJSON <- loadGameState
        let game = fromMaybe Lost (decode gameJSON :: Maybe GameState)
        let index = coordToIndex game (i, j)
        let move = ClearSquare index
        let newGame = play game move
        renderGame newGame



loadGameState = liftIO ( BL.readFile "game.json" )
storeGameState game = liftIO ( BL.writeFile "game.json" (encode game) )

-- renderGame :: GameState ->
renderGame Lost = blaze Layout.lost
renderGame Won  = blaze Layout.won
renderGame game = do
    let gameHtml = gameStateToHtml game
    blaze (Layout.play gameHtml)
    storeGameState game
