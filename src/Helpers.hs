{-# LANGUAGE LambdaCase #-}

module Helpers where

import Control.Lens hiding (element)
import Data.Colour.SRGB
import Data.Int
import Data.Maybe
import Data.Word
import Safe

import Foreign.C.Types

import qualified SDL.Event as SDL
import qualified SDL.Vect as SDL
import qualified SDL.Video as SDL

import Types

intToCInt :: Int -> CInt
intToCInt = CInt . fromIntegral

fromColor :: Color -> SDL.V4 Word8
fromColor color = SDL.V4 (channelRed c) (channelGreen c) (channelBlue c) 0
    where c = toSRGB24 color

toSDLPoint :: Point -> SDL.Point SDL.V2 CInt
toSDLPoint (Point x1 y1) = SDL.P (SDL.V2 (CInt $ fromIntegral x1) (CInt $ fromIntegral y1))

toSDLRectangle :: Point -> Int -> Int -> SDL.Rectangle CInt
toSDLRectangle p w h =
    SDL.Rectangle
        (toSDLPoint p)
        (SDL.V2 (CInt $ fromIntegral w) (CInt $ fromIntegral h))

containsPoint :: SDL.Point SDL.V2 Int32 -> Element msg -> Bool
containsPoint (SDL.P (SDL.V2 x1 y1)) (Element element _ styleCollection) =
    case element of
        RectangleElement ->
            let
                point = position styleCollection
                w = fromIntegral $ width styleCollection
                h = fromIntegral $ height styleCollection
                x2 = fromIntegral $ point ^. x
                y2 = fromIntegral $ point ^. y
            in
            x1 > x2 && x1 < (x2 + w) && y1 > y2 && y1 < (y2 + h)
        TextBoxElement _ -> False
        InputBox -> False

getMessageForEventPayload :: App msg -> (msg -> IO ()) -> SDL.EventPayload -> IO ()
getMessageForEventPayload app processMessage eventPayload = do
    case eventPayload of
        SDL.MouseButtonEvent eventData ->
            if SDL.mouseButtonEventMotion eventData == SDL.Released
                then
                    case getClickMessage app $ SDL.mouseButtonEventPos eventData of
                        Just msg -> processMessage msg
                        Nothing -> pure ()
                else
                    pure ()
        _ ->
            pure ()

getClickMessage :: App msg -> SDL.Point SDL.V2 Int32 -> Maybe msg
getClickMessage app click = do
    let clickedElements = filter (containsPoint click) $ elems app
    topElement <- headMay clickedElements
    headMay $ mapMaybe isOnClick $ view handlers topElement
    where
        isOnClick = \case
            OnClick msg -> Just msg
            _           -> Nothing
