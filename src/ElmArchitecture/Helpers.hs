{-# LANGUAGE LambdaCase #-}

module ElmArchitecture.Helpers where

import Control.Concurrent.MVar
import Control.Lens hiding (element)
import Control.Monad (void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ask)
import Data.Colour.SRGB
import Data.Int
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Data.Word
import Safe

import Foreign.C.Types

import qualified SDL.Event as SDL
import qualified SDL.Input as SDL
import qualified SDL.Vect as SDL
import qualified SDL.Video as SDL

import ElmArchitecture.Types

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
containsPoint (SDL.P (SDL.V2 x1 y1)) (Element _ _ styleCollection) =
    let
        point = position styleCollection
        w = fromIntegral $ width styleCollection
        h = fromIntegral $ height styleCollection
        x2 = fromIntegral $ point ^. x
        y2 = fromIntegral $ point ^. y
    in
    x1 > x2 && x1 < (x2 + w) && y1 > y2 && y1 < (y2 + h)

getMessageForEventPayload
    :: SDL.EventPayload
    -> (msg -> MyReader model msg ())
    -> MyReader model msg ()
getMessageForEventPayload eventPayload processMessage = do
    appState <- ask
    case eventPayload of
        SDL.MouseButtonEvent eventData ->
            if SDL.mouseButtonEventMotion eventData == SDL.Released
                then do
                    appView <- lift $ readMVar $ currentView appState
                    case getClickedElement appView $ SDL.mouseButtonEventPos eventData of
                        Just element -> do
                            -- If user clicked on a input, select it
                            case element ^. viewElement of
                                InputBox t ->
                                    case mapMaybe isOnInput $ view handlers element of
                                        (f : _) -> void $ lift $ swapMVar (currentInputBox appState) $ Just (t, f)
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

            inputBox <- lift $ readMVar (currentInputBox appState)

            case inputBox of
                Just (currentText, msg) -> do
                    let newText = T.append currentText inputText
                    void $ lift $ swapMVar (currentInputBox appState) $ Just (newText, msg)
                    processMessage $ msg newText
                Nothing -> pure ()

        SDL.KeyboardEvent eventData -> do
            if SDL.keyboardEventKeyMotion eventData == SDL.Pressed
                then do
                    case SDL.keysymKeycode $ SDL.keyboardEventKeysym eventData of
                        SDL.KeycodeBackspace -> do
                            inputBox <- lift $ readMVar (currentInputBox appState)

                            case inputBox of
                                Just (currentText, msg) -> do
                                    let newText = if T.null currentText then T.empty else T.init currentText
                                    void $ lift $ swapMVar (currentInputBox appState) $ Just (newText, msg)
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
    let clickedElements = sortOn (zIndex . view styles) $ filter (containsPoint click) $ elems app
    headMay clickedElements
