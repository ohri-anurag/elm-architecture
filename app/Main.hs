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
        [ Element (RectangleElement $ Rectangle (Point 100 100) 600 200 c1 (sRGB 0 0 0)) [OnClick Rect1]
        , Element (TextBoxElement $ TextBox t1 (Point 200 200) c2 c1 (Font "" 32)) []
        , Element (RectangleElement $ Rectangle (Point 800 100) 600 200 c2 (sRGB 0 0 0)) [OnClick Rect2]
        , Element (TextBoxElement $ TextBox t2 (Point 900 200) c1 c2 (Font "" 32)) []
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
