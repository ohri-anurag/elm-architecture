{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

import Data.Colour.SRGB
import qualified Data.Text as T
import ElmArchitecture

type Model = String

data Msg
    = ReadClicked
    | FileContents String
    deriving Eq

data Action = ReadFile

view :: Model -> App Msg
view str =
    app  1920 1080 "FileReader" (color 150 150 150)
        [ textBox (T.pack str) [] $ defaultStyles
            { position = point 800 0
            , fontSize = 32
            }
        , textBox "Read" [onClick ReadClicked] $ defaultStyles
            { position = point 800 700
            , fontSize = 32
            , width = 80
            , height = 40
            }
        ]

update :: Msg -> Model -> (Model, Maybe Action)
update ReadClicked str = (str, Just ReadFile)
update (FileContents fc) _ = (fc, Nothing)

action :: Action -> IO Msg
action ReadFile = readFile "text/Name.txt" >>= pure . FileContents

main :: IO ()
main = elmArchitecture  "" view update action
