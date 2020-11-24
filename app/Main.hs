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

view :: Model -> Window Msg
view model =
    (Window
        (WindowProps 1920 1080 "Anurag" (sRGB24 100 149 237))
        [ Element (RectangleElement $ Rectangle (Point 100 100) 600 200 (if model == Model1 then c1 else c2) (sRGB 0 0 0)) [OnClick Rect1]
        , Element (RectangleElement $ Rectangle (Point 800 100) 600 200 (if model == Model2 then c1 else c2) (sRGB 0 0 0)) [OnClick Rect2]
        ]
    )
    where
        c1 = sRGB24 0 0 0
        c2 = sRGB24 237 149 100

update :: Msg -> Model -> Model
update _ model =
    case model of
        Model1 -> Model2
        Model2 -> Model1

main :: IO ()
main = elmArchitecture Model1 view update
