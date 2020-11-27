{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens
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
    }

data ViewElement
    = RectangleElement Rectangle
    | TextBoxElement TextBox
    | InputBox

data Rectangle = Rectangle
    { _topLeft :: Point
    , _rectangleWidth :: Int
    , _rectangleHeight :: Int
    , _rectangleBackgroundColor :: Color
    , _borderColor :: Color
    }

data TextBox = TextBox
    { _text :: T.Text
    , _textTopLeft :: Point
    , _textColor :: Color
    , _textBackgroundColor :: Color
    , _font :: Font
    }

data Font = Font
    { _family :: String
    , _size :: Int
    }

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
makeLenses ''Rectangle
makeLenses ''TextBox
makeLenses ''Font
makeLenses ''Point
