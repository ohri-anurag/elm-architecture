{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Lib where

import Control.Lens hiding (element)
-- import Control.Monad
import Data.Colour.SRGB
import Data.Int
import Data.StateVar
import Data.Word
import Safe

import Foreign.C.Types

import qualified SDL.Init as SDL
import qualified SDL.Event as SDL
import qualified SDL.Time as SDL
import qualified SDL.Vect as SDL
import qualified SDL.Video as SDL

import Types

someFunc :: IO ()
someFunc = do
    SDL.initialize [SDL.InitVideo]

    window <- SDL.createWindow "Test Window" SDL.defaultWindow

    surface <- SDL.getWindowSurface window
    SDL.surfaceFillRect surface Nothing $ SDL.V4 255 0 0 0

    SDL.updateWindowSurface window

    SDL.delay 500

    waitForCloseButton window

waitForCloseButton :: SDL.Window -> IO ()
waitForCloseButton window = do
    event <- SDL.pollEvent
    case event of
        Just (SDL.Event _ eventPayload) ->
            if eventPayload == SDL.QuitEvent
                then do
                    SDL.destroyWindow window
                    SDL.quit
                else waitForCloseButton window
        Nothing ->
            waitForCloseButton window

fromColor :: Color -> SDL.V4 Word8
fromColor color = SDL.V4 (channelRed c) (channelGreen c) (channelBlue c) 0
    where c = toSRGB24 color

toSDLPoint :: Point -> SDL.Point SDL.V2 CInt
toSDLPoint (Point x1 y1) = SDL.P (SDL.V2 (CInt $ fromIntegral x1) (CInt $ fromIntegral y1))

draw :: SDL.Renderer -> ViewElement -> IO ()
draw renderer element = case element of
    RectangleElement rect -> drawRectangle renderer rect
    InputBox -> pure ()
    TextBox -> pure ()

containsPoint :: SDL.Point SDL.V2 Int32 -> ViewElement -> Bool
containsPoint (SDL.P (SDL.V2 x1 y1)) element =
    case element of
        RectangleElement rectangle ->
            let
                x2 = fromIntegral $ rectangle ^. topLeft . x
                y2 = fromIntegral $ rectangle ^. topLeft . y
            in
            x1 > x2 && x1 < (x2 + fromIntegral (rectangle ^. rectangleWidth)) &&
            y1 > y2 && y1 < (y2 + fromIntegral (rectangle ^. rectangleHeight))
        TextBox -> False
        InputBox -> False

toSDLRectangle :: Point -> Int -> Int -> SDL.Rectangle CInt
toSDLRectangle p w h =
    SDL.Rectangle
        (toSDLPoint p)
        (SDL.V2 (CInt $ fromIntegral w) (CInt $ fromIntegral h))

drawRectangle :: SDL.Renderer -> Rectangle -> IO ()
drawRectangle renderer rectangleProps = do
    SDL.rendererDrawColor renderer $= fromColor (rectangleProps ^. borderColor)
    SDL.drawRect renderer (Just (toSDLRectangle
                                    (rectangleProps ^. topLeft)
                                    (rectangleProps ^. rectangleWidth)
                                    (rectangleProps ^. rectangleHeight)))
    SDL.rendererDrawColor renderer $= fromColor (rectangleProps ^. rectangleBackgroundColor)
    SDL.fillRect renderer (Just (toSDLRectangle
                                    (rectangleProps ^. topLeft)
                                    (rectangleProps ^. rectangleWidth)
                                    (rectangleProps ^. rectangleHeight)))
    SDL.present renderer

elmArchitecture :: (Eq b, Show b) => a -> (a -> Window b) -> (b -> a -> a) -> IO ()
elmArchitecture initModel viewFn updateFn = do
    let Window wp elems = viewFn initModel
    SDL.initialize [SDL.InitVideo]

    window <- SDL.createWindow (wp^.name)
              $ SDL.defaultWindow
              { SDL.windowInitialSize = SDL.V2 (CInt $ fromIntegral $ wp ^. windowWidth) (CInt $ fromIntegral $ wp ^. windowHeight) }

    

    -- SDL.surfaceFillRect surface Nothing $ fromColor $ backgroundColor wp

    -- SDL.updateWindowSurface window

    -- SDL.delay 1000

    renderer <- SDL.createRenderer window (CInt (-1)) $ SDL.defaultRenderer { SDL.rendererType = SDL.UnacceleratedRenderer }
    SDL.rendererDrawColor renderer $= fromColor (wp^.windowBackgroundColor)
    SDL.clear renderer

    -- SDL.fillRect renderer (Just (SDL.Rectangle (SDL.P (SDL.V2 0 0)) (SDL.V2 (CInt $ fromIntegral $ windowWidth wp) (CInt $ fromIntegral $ windowHeight wp))))
    -- SDL.drawRect renderer (Just (SDL.Rectangle (SDL.P (SDL.V2 500 500)) (SDL.V2 300 100)))

    SDL.present renderer

    mapM_ (draw renderer . view viewElement) elems
    -- surface <- SDL.getWindowSurface window
    -- SDL.updateWindowSurface window

    -- SDL.delay 500

    handleEvents window renderer elems initModel viewFn updateFn

handleEvents :: (Eq b, Show b) => SDL.Window -> SDL.Renderer -> [Element b] -> a -> (a -> Window b) -> (b -> a -> a) -> IO ()
handleEvents window renderer elems initModel viewFn updateFn = do
    event <- SDL.pollEvent
    case event of
        Just (SDL.Event _ eventPayload) ->
            if eventPayload == SDL.QuitEvent
                then do
                    SDL.destroyWindow window
                    SDL.quit
                else do
                    newModel <- case eventPayload of
                        SDL.MouseButtonEvent eventData ->
                            if SDL.mouseButtonEventMotion eventData == SDL.Released
                                then do
                                    putStrLn "Click!"
                                    let 
                                        click = SDL.mouseButtonEventPos eventData
                                        clickedElements = filter (containsPoint click . view viewElement) elems
                                        topElement = headMay clickedElements
                                    case topElement of
                                        Just (Element _ attrs) -> do
                                            let t = headMay $ filter (\case
                                                        OnClick _ -> True
                                                        _ -> False) attrs
                                            case t of
                                                Just (OnClick cmd) -> do
                                                    putStrLn $ "Rectangle clicked - " ++ show cmd 
                                                    let
                                                        newModel = updateFn cmd initModel
                                                        Window _ elems' = viewFn newModel
                                                    mapM_ (draw renderer . view viewElement) elems'
                                                    SDL.present renderer
                                                    pure newModel
                                                _ ->
                                                    pure initModel
                                        Nothing -> pure initModel
                                else
                                    pure initModel
                        _ ->
                            pure initModel
                    handleEvents window renderer elems newModel viewFn updateFn
        Nothing ->
            handleEvents window renderer elems initModel viewFn updateFn
