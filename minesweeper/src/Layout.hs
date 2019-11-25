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
            H.div ! class_ "main" $ do
                newGameForm


play gridHtml = do
    docType
    html $ do
        linkStylesheet
        body $ do
            h1 "Minesweeper"
            H.div ! class_ "main" $ do
                newGameButton
                br
                controlsForm
                gridHtml

lost = do
    docType
    html $ do
        linkStylesheet
        body $ do
            h1 "Minesweeper"
            H.div ! class_ "main" $ do
                newGameButton
                br
                h3 "You Lose."

won = do
    docType
    html $ do
        linkStylesheet
        body $ do
            h1 "Minesweeper"
            H.div ! class_ "main" $ do
                H.div ! class_ "controls" $ do
                    newGameButton
                    br
                    h3 "You Win!"


linkStylesheet = H.head $ link ! rel "stylesheet" ! type_ "text/css" ! href "style.css"

divRow = H.div ! class_ "row"
divCol = H.div ! class_ "col"

textField s = input ! type_ "text" ! name s
submitButton s = input ! type_ "submit" ! value s

newGameButton = button ! onclick "window.location.href='/'" ! type_ "button" $ "New Game"

newGameForm = H.form ! action "/new" $ do
    "Number of Rows"
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

controlsForm = H.form $ do
    "Row"
    br
    textField "i"
    br
    "Column"
    br
    textField "j"
    br
    submitButton "Clear" ! formaction "/clear"
    submitButton "Flag" ! formaction "/flag"
