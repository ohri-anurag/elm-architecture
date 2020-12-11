module ElmArchitecture.API where

import Data.Word (Word8)
import qualified Data.Text as T
import Data.Colour.SRGB (sRGB24)
import Data.Colour.Names (black, white)

import ElmArchitecture.Types

-- VIEW ELEMENTS
textBox :: T.Text -> Maybe Alignment -> [Handler a] -> Styles -> Element a
textBox text alignment = Element (TextBoxElement text alignment)

inputBox :: T.Text -> [Handler a] -> Styles -> Element a
inputBox text = Element (InputBox text)

-- TYPES
point :: Int -> Int -> Point
point = Point

-- HANDLERS
onClick :: a -> Handler a
onClick = OnClick

onInput :: (T.Text -> a) -> Handler a
onInput = OnInput

-- COLOR
color :: Word8 -> Word8 -> Word8 -> Color
color = sRGB24

-- FONT
font :: FilePath -> FontPath
font = UserFont

-- STYLES
defaultStyles :: Styles
defaultStyles = Styles
    { fontFamily = DefaultFont
    , fontColor = black
    , backgroundColor = white
    , borderColor = black
    , fontSize = 24
    , position = Point 0 0
    , width = 0
    , height = 0
    , zIndex = 0
    }

-- APP
app :: Int -> Int -> T.Text -> Color -> [Element a] -> App a
app w h appName bgColor = App (AppProps w h appName bgColor)