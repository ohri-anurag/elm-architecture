module Render where

import Control.Lens hiding (element)
import Control.Exception
import Control.Monad
import Data.StateVar

import Foreign.C.Types
import Foreign.Storable

import qualified SDL.Exception as SDL
import qualified SDL.Init as SDL
import qualified SDL.Video.Renderer as SDL
import qualified SDL.Video as SDL

import qualified SDL.Raw.Types as Raw

import qualified SDL.Font as Font

import Types
import Helpers

-- This function takes an app that is the output of our viewFn
-- and renders it to the screen
renderApp :: App msg -> SDL.Renderer -> IO ()
renderApp app renderer = do
    -- Paint the app background
    SDL.rendererDrawColor renderer $= fromColor (appProps app ^. appBackgroundColor)
    SDL.clear renderer

    -- Render all the elements in the app
    mapM_ (render renderer . view viewElement) $ elems app

    -- Display everything to the screen
    SDL.present renderer

render :: SDL.Renderer -> ViewElement -> IO ()
render renderer element = case element of
    RectangleElement rect -> renderRectangle renderer rect
    TextBoxElement textBox -> renderTextBox renderer textBox
    InputBox -> pure ()

renderRectangle :: SDL.Renderer -> Rectangle -> IO ()
renderRectangle renderer rectangleProps = do
    void $ SDL.rendererDrawColor renderer $= fromColor (rectangleProps ^. borderColor)
    SDL.drawRect renderer (Just (toSDLRectangle
                                    (rectangleProps ^. topLeft)
                                    (rectangleProps ^. rectangleWidth)
                                    (rectangleProps ^. rectangleHeight)))
    void $ SDL.rendererDrawColor renderer $= fromColor (rectangleProps ^. rectangleBackgroundColor)
    SDL.fillRect renderer (Just (toSDLRectangle
                                    (rectangleProps ^. topLeft)
                                    (rectangleProps ^. rectangleWidth)
                                    (rectangleProps ^. rectangleHeight)))

renderTextBox :: SDL.Renderer -> TextBox -> IO ()
renderTextBox renderer textBox = do
    let fontSize = textBox ^. font . size

    loadedFont <- handle
        -- Default font, in case the user specified font didn't load
        (const $ Font.load "font/FreeSans.ttf" fontSize :: (SDL.SDLException -> IO Font.Font))
        (Font.load (textBox ^. font . family) fontSize)

    textSurface <- Font.shaded
        loadedFont
        (fromColor $ textBox ^. textColor)
        (fromColor $ textBox ^. textBackgroundColor)
        (textBox ^. text)

    let
        SDL.Surface rawSurfacePtr _ = textSurface
    rawSurface <- peek rawSurfacePtr
    let
        (CInt sw) = Raw.surfaceW rawSurface
        (CInt sh) = Raw.surfaceH rawSurface
    textTexture <- SDL.createTextureFromSurface renderer textSurface

    SDL.copy renderer textTexture Nothing (Just $ toSDLRectangle (textBox ^. textTopLeft) (fromIntegral sw) (fromIntegral sh))

    SDL.destroyTexture textTexture
    SDL.freeSurface textSurface

exitWindow :: SDL.Window -> IO ()
exitWindow window = do
    Font.quit

    SDL.destroyWindow window
    SDL.quit
