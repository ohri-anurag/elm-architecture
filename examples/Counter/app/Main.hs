{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

import Data.Colour.SRGB
import qualified Data.Text as T
import ElmArchitecture

type Model = Int

data Msg
    = Inc
    | Dec
    | NoOp
    deriving Eq

type Action = ()

update :: Msg -> Model -> (Model, Maybe Action)
update msg model =
    case msg of
        Inc -> (model + 1, Nothing)
        Dec -> (model - 1, Nothing)
        NoOp -> (model, Nothing)

view :: Model -> App Msg
view model =
    app 1920 1080 "Counter" grey
        [ textBox "+" alignment [onClick Inc] $ defaultStyles
            { position = point 800 500
            , fontColor = white
            , backgroundColor = green
            , fontSize = 32
            , width = 50
            , height = 50
            }
        , textBox (T.pack $ show model) alignment [] $ defaultStyles
            { position = point 900 500
            , fontColor = white
            , backgroundColor = if model == 0 then black else if model > 0 then green else red
            , fontSize = 32
            , width = 50
            , height = 50
            }
        , textBox "-" alignment [onClick Dec] $ defaultStyles
            { position = point 1000 500
            , fontColor = white
            , backgroundColor = red
            , fontSize = 32
            , width = 50
            , height = 50
            }
        ]
    where alignment = Just (Alignment Center Middle)

main :: IO ()
main = elmArchitecture 0 view update (const $ pure NoOp)
