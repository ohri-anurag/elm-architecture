{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Colour.SRGB

import Lib
import Types

data Msg
    = Rect1
    | Rect2
    deriving (Eq, Show)

data Model
    = Model1
    | Model2
    deriving (Eq, Show)

view :: Model -> App Msg
view model =
    App
        (AppProps 1920 1080 "Anurag" (sRGB24 100 149 237))
        [ Element RectangleElement [OnClick Rect1] $ defaultStyles
            { position = Point 100 100
            , width = 600
            , height = 200
            , backgroundColor = c1
            }
        , Element (TextBoxElement t1) [] $ defaultStyles
            { position = Point 200 200
            , fontColor = c2
            , backgroundColor = c1
            , fontSize = 32
            }
        , Element RectangleElement [OnClick Rect2] $ defaultStyles
            { position = Point 800 100
            , width = 600
            , height = 200
            , backgroundColor = c2
            }
        , Element (TextBoxElement t2) [] $ defaultStyles
            { position = Point 900 200
            , fontColor = c1
            , backgroundColor = c2
            , fontSize = 32
            }
        ]
    where
        c1 = if model == Model1 then sRGB24 0 0 0 else sRGB24 237 149 100
        c2 = if model == Model1 then sRGB24 237 149 100 else sRGB24 0 0 0

        t1 = if model == Model1 then "Black Box" else "Orange Box"
        t2 = if model == Model1 then "Orange Box" else "Black Box"

update :: Msg -> Model -> Model
update _ model =
    case model of
        Model1 -> Model2
        Model2 -> Model1

main :: IO ()
main = elmArchitecture $ Requirements
    { initModel = Model1
    , viewFn = view
    , updateFn = update
    }
