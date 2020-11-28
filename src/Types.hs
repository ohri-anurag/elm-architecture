{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens
import Data.Colour.Names
import Data.Colour.SRGB
import qualified Data.Text as T

data App a = App
    { appProps :: AppProps
    , elems :: [Element a]
    }

type Color = Colour Double

data AppProps = AppProps
    { _appWidth :: Int
    , _appHeight :: Int
    , _name :: T.Text
    , _appBackgroundColor :: Color
    }

data Handler a
    = OnClick a
    | OnInput a

data Element a = Element
    { _viewElement :: ViewElement
    , _handlers :: [Handler a]
    , _styles :: Styles
    }

data Styles = Styles
    { fontFamily :: String
    , fontColor :: Color
    , backgroundColor :: Color
    , borderColor :: Color
    , fontSize :: Int
    , position :: Point
    , width :: Int
    , height :: Int
    }

defaultStyles :: Styles
defaultStyles = Styles
    { fontFamily = "font/FreeSans.ttf"
    , fontColor = black
    , backgroundColor = black
    , borderColor = black
    , fontSize = 24
    , position = Point 0 0
    , width = 0
    , height = 0
    }

data ViewElement
    = RectangleElement
    | TextBoxElement T.Text
    | InputBox

data Point = Point
    { _x :: Int
    , _y :: Int
    }

data Requirements msg model = Requirements
    { initModel :: model
    , updateFn :: msg -> model -> model
    , viewFn :: model -> App msg
    }

makeLenses ''AppProps
makeLenses ''Element
makeLenses ''Point
