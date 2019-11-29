{-# LANGUAGE OverloadedStrings #-}
module Main where

import Bot
import Lib
import qualified Layout

import Control.Monad.IO.Class (liftIO)
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger
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
    middleware logStdoutDev
    middleware $ staticPolicy (addBase "static")
    get "/" $ do
        blaze Layout.root

    get "/new" $ do
        rStr <- param "rows"
        cStr <- param "cols"
        bombsStr <- param "bombs"
        let dims = parseCoord rStr cStr
        let bombs = read bombsStr
        let game = GS (makeBoard dims bombs) bombs
        let gameHtml = gameStateToHtml game
        blaze (Layout.play gameHtml) -- pass the html for displaying the grid
        storeGameState game

    get "/flag" $ do
        iStr <- param "i"
        jStr <- param "j"
        let coord = parseCoord iStr jStr
        gameJSON <- loadGameState
        let game = jsonToData gameJSON
        let index = coordToIndex game coord
        let move = FlagSquare index
        let newGame = play game move
        renderGame newGame
        storeGameState newGame

    get "/clear" $ do
        iStr <- param "i"
        jStr <- param "j"
        let coord = parseCoord iStr jStr
        gameJSON <- loadGameState
        let game = jsonToData gameJSON
        let index = coordToIndex game coord
        let move = ClearSquare index
        let newGame = play game move
        renderGame newGame
        storeGameState newGame

    get "/auto" $ do
        gameJSON <- loadGameState
        let game = jsonToData gameJSON
        let auto = autoMove game
        -- liftIO ( putStrLn $ show auto ) -- for debugging
        let newGame = play game auto
        renderGame newGame
        storeGameState newGame


loadGameState = liftIO ( BL.readFile "game.json" )
storeGameState game = liftIO ( BL.writeFile "game.json" (encode game) )

parseCoord iStr jStr = (i, j)
    where
        i = read iStr
        j = read jStr

jsonToData gameJSON = fromMaybe Lost (decode gameJSON :: Maybe GameState)

renderGame game = do
    let gameHtml = gameStateToHtml game
    case game of
        Lost    -> blaze Layout.lost
        Won     -> blaze Layout.won
        _       -> blaze (Layout.play gameHtml)
