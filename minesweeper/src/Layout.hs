{-# LANGUAGE OverloadedStrings #-}
module Layout where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

root = do
    docType
    html $ do
        linkStylesheet
        body $ do
            h1 "Minesweeper"
            H.form ! action "/play" $ do
                "Number of Rows:"
                br
                textField "rows"
                br
                "Number of Columns"
                br
                textField "cols"
                br
                "Number of Bombs"
                br
                textField "bombs"
                br
                submitButton "Play"


play gridHtml = do
    docType
    html $ do
        linkStylesheet
        body $ do
            h1 "Minesweeper"
            newGameButton
            br
            H.form $ do
                "Row"
                br
                textField "i"
                br
                "Column"
                br
                textField "j"
                br
                submitButton "Clear" ! formaction "/play/clear"
                submitButton "Flag" ! formaction "/play/flag"
            gridHtml

lost = do
    docType
    html $ do
        linkStylesheet
        body $ do
            h1 "Minesweeper"
            newGameButton
            br
            h3 "You Lose."

won = do
    docType
    html $ do
        linkStylesheet
        body $ do
            h1 "Minesweeper"
            newGameButton
            br
            h3 "You Win!"


linkStylesheet = H.head $ do
    link ! rel "stylesheet" ! type_ "text/css" ! href "style.css"

textField s = input ! type_ "text" ! name s
submitButton s = input ! type_ "submit" ! value s

newGameButton = button ! onclick "window.location.href='/'" ! type_ "button" $ "New Game"
