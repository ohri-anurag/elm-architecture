{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

import Data.Colour.SRGB
import qualified Data.Text as T
import ElmArchitecture
import ElmArchitecture.Types

type Model = Int

data Msg = Inc | Dec
    deriving Eq

update :: Msg -> Model -> Model
update msg model =
    case msg of
        Inc -> model + 1
        Dec -> model - 1

view :: Model -> App Msg
view model =
    App
        (AppProps 1920 1080 "Counter" (sRGB24 0 0 0))
        [ Element (TextBoxElement "+") [OnClick Inc] $ defaultStyles
            { position = Point 800 500
            , fontColor = (sRGB24 0 0 0)
            , backgroundColor = (sRGB24 255 255 255)
            , fontSize = 32
            , width = 25
            , height = 40
            }
        , Element (TextBoxElement (T.pack $ show model)) [] $ defaultStyles
            { position = Point 900 500
            , fontColor = (sRGB24 0 0 0)
            , backgroundColor = (sRGB24 255 255 255)
            , fontSize = 32
            , width = 25
            , height = 40
            }
        , Element (TextBoxElement "-") [OnClick Dec] $ defaultStyles
            { position = Point 1000 500
            , fontColor = (sRGB24 0 0 0)
            , backgroundColor = (sRGB24 255 255 255)
            , fontSize = 32
            , width = 25
            , height = 40
            }
        ]

main :: IO ()
main = elmArchitecture $ Requirements
    { initModel = 0
    , viewFn = view
    , updateFn = update
    }
