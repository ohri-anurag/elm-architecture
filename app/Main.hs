{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Colour.SRGB
import qualified Data.Text as T

import ElmArchitecture

data Msg
    = Input T.Text
    | NoOp
    deriving (Eq, Show)

newtype Model
    = Model T.Text
    deriving (Eq, Show)

type Action = ()

view :: Model -> App Msg
view (Model text) =
    app 1920 1080 "Anurag" black
        [ inputBox text [onInput Input] $ defaultStyles
            { position = point 900 200
            , fontColor = c2
            , backgroundColor = c1
            , fontSize = 32
            , width = 300
            , height = 50
            }
        ]
    where
        c1 = sRGB24 255 192 203
        c2 = sRGB24 215 72 154

update :: Msg -> Model -> (Model, Maybe Action)
update (Input text) _ = (Model text, Nothing)
update NoOp model = (model, Nothing)

main :: IO ()
main = elmArchitecture (Model "") view update (const $ pure NoOp)
