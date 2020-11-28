{-# LANGUAGE LambdaCase #-}

module Helpers where

import Control.Concurrent.MVar
import Control.Lens hiding (element)
import Control.Monad
import Data.Colour.SRGB
import Data.Int
import Data.Maybe
import qualified Data.Text as T
import Data.Word
import Safe

import Foreign.C.Types

import qualified SDL.Event as SDL
import qualified SDL.Input as SDL
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
        TextBoxElement _ -> False
        _ ->
            let
                point = position styleCollection
                w = fromIntegral $ width styleCollection
                h = fromIntegral $ height styleCollection
                x2 = fromIntegral $ point ^. x
                y2 = fromIntegral $ point ^. y
            in
            x1 > x2 && x1 < (x2 + w) && y1 > y2 && y1 < (y2 + h)

getMessageForEventPayload :: App msg -> MVar (Maybe (T.Text, T.Text -> msg)) -> (msg -> IO ()) -> SDL.EventPayload -> IO ()
getMessageForEventPayload app currentInputBoxMVar processMessage eventPayload = do
    case eventPayload of
        SDL.MouseButtonEvent eventData ->
            if SDL.mouseButtonEventMotion eventData == SDL.Released
                then do
                    case getClickedElement app $ SDL.mouseButtonEventPos eventData of
                        Just element -> do
                            -- If user clicked on a input, select it
                            case element ^. viewElement of
                                InputBox t ->
                                    case mapMaybe isOnInput $ view handlers element of
                                        (f : _) -> void $ swapMVar currentInputBoxMVar $ Just (t, f)
                                        _ -> pure ()
                                _ -> pure ()
                            
                            -- Also fire the onClick msg
                            case mapMaybe isOnClick $ view handlers element of
                                (msg : _) -> processMessage msg
                                _ -> pure ()
                        Nothing -> pure ()
                else
                    pure ()

        SDL.TextInputEvent eventData -> do
            let inputText = SDL.textInputEventText eventData

            currentInputBox <- readMVar currentInputBoxMVar

            case currentInputBox of
                Just (currentText, msg) -> do
                    let newText = T.append currentText inputText
                    void $ swapMVar currentInputBoxMVar $ Just (newText, msg)
                    processMessage $ msg newText
                Nothing -> pure ()

        SDL.KeyboardEvent eventData -> do
            if SDL.keyboardEventKeyMotion eventData == SDL.Pressed
                then do
                    case SDL.keysymKeycode $ SDL.keyboardEventKeysym eventData of
                        SDL.KeycodeBackspace -> do
                            currentInputBox <- readMVar currentInputBoxMVar

                            case currentInputBox of
                                Just (currentText, msg) -> do
                                    let newText = if T.null currentText then T.empty else T.init currentText
                                    void $ swapMVar currentInputBoxMVar $ Just (newText, msg)
                                    processMessage $ msg newText
                                Nothing -> pure ()

                        _ -> pure ()
                else pure ()

        _ ->
            pure ()
    where
        isOnClick = \case
            OnClick msg -> Just msg
            _           -> Nothing

        isOnInput = \case
            OnInput f -> Just f
            _         -> Nothing

getClickedElement :: App msg -> SDL.Point SDL.V2 Int32 -> Maybe (Element msg)
getClickedElement app click = do
    let clickedElements = filter (containsPoint click) $ elems app
    headMay clickedElements
