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
        [ textBox (T.pack str) alignment [] $ defaultStyles
            { position = point 460 200
            , width = 1000
            , fontSize = 32
            }
        , textBox "Read" alignment [onClick ReadClicked] $ defaultStyles
            { position = point 910 700
            , fontSize = 32
            , width = 100
            , height = 50
            }
        ]
        where alignment = Just (Alignment Center Middle)

update :: Msg -> Model -> (Model, Maybe Action)
update ReadClicked str = (str, Just ReadFile)
update (FileContents fc) _ = (fc, Nothing)

action :: Action -> IO Msg
action ReadFile = readFile "text/Name.txt" >>= pure . FileContents

main :: IO ()
main = elmArchitecture  "" view update action
