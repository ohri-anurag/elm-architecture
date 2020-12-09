{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

import Data.Colour.SRGB
import qualified Data.Text as T
import ElmArchitecture
import ElmArchitecture.Types

type Model = String

data Msg
    = ReadClicked
    | FileContents String
    deriving Eq

data Action = ReadFile

view :: Model -> App Msg
view str =
    App
        (AppProps 1920 1080 "FileReader" (sRGB24 150 150 150))
        [ Element (TextBoxElement $ T.pack str) [] $ defaultStyles
            { position = Point 800 0
            , fontColor = (sRGB24 0 0 0)
            , backgroundColor = (sRGB24 255 255 255)
            , fontSize = 32
            }
        , Element (TextBoxElement "Read") [OnClick ReadClicked] $ defaultStyles
            { position = Point 800 700
            , fontColor = (sRGB24 0 0 0)
            , backgroundColor = (sRGB24 255 255 255)
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
main = elmArchitecture $ Requirements
    { initModel = ""
    , viewFn = view
    , updateFn = update
    , actionFn = action
    }
