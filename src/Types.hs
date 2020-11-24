{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens
import Data.Colour.SRGB
import qualified Data.Text as T

data Window a = Window WindowProps [Element a]
type Color = Colour Double

data WindowProps = WindowProps
    { _windowWidth :: Int
    , _windowHeight :: Int
    , _name :: T.Text
    , _windowBackgroundColor :: Color
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
    | InputBox
    | TextBox

data Rectangle = Rectangle
    { _topLeft :: Point
    , _rectangleWidth :: Int
    , _rectangleHeight :: Int
    , _rectangleBackgroundColor :: Color
    , _borderColor :: Color
    }

data Point = Point
    { _x :: Int
    , _y :: Int
    }

makeLenses ''WindowProps
makeLenses ''Element
makeLenses ''Rectangle
makeLenses ''Point