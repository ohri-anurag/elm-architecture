module Render where

import Control.Lens hiding (element)
import Control.Monad
import Data.StateVar
import qualified Data.Text as T

import Foreign.C.Types
import Foreign.Storable

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
    mapM_ (render renderer) $ elems app

    -- Display everything to the screen
    SDL.present renderer

render :: SDL.Renderer -> Element msg -> IO ()
render renderer (Element viewElem _ styleCollection) = case viewElem of
    RectangleElement -> renderRectangle renderer styleCollection
    TextBoxElement str -> renderTextBox renderer str styleCollection
    InputBox str -> renderInputBox renderer str styleCollection

renderRectangle :: SDL.Renderer -> Styles -> IO ()
renderRectangle renderer styleCollection = do
    void $ SDL.rendererDrawColor renderer $= fromColor (borderColor styleCollection)
    SDL.drawRect renderer (Just (toSDLRectangle
                                    (position styleCollection)
                                    (width styleCollection)
                                    (height styleCollection)))
    void $ SDL.rendererDrawColor renderer $= fromColor (backgroundColor styleCollection)
    SDL.fillRect renderer (Just (toSDLRectangle
                                    (position styleCollection)
                                    (width styleCollection)
                                    (height styleCollection)))

renderTextBox :: SDL.Renderer -> T.Text -> Styles -> IO ()
renderTextBox renderer str styleCollection = unless (T.null str) $ do
    let fs = fontSize styleCollection

    loadedFont <- Font.load (fontFamily styleCollection) fs

    textSurface <- Font.shaded
        loadedFont
        (fromColor $ fontColor styleCollection)
        (fromColor $ backgroundColor styleCollection)
        str

    let
        SDL.Surface rawSurfacePtr _ = textSurface
    rawSurface <- peek rawSurfacePtr
    let
        (CInt sw) = Raw.surfaceW rawSurface
        (CInt sh) = Raw.surfaceH rawSurface
    textTexture <- SDL.createTextureFromSurface renderer textSurface

    SDL.copy renderer textTexture Nothing (Just $ toSDLRectangle (position styleCollection) (fromIntegral sw) (fromIntegral sh))

    SDL.destroyTexture textTexture
    SDL.freeSurface textSurface

renderInputBox :: SDL.Renderer -> T.Text -> Styles -> IO ()
renderInputBox renderer str styleCollection = do
    renderRectangle renderer styleCollection
    renderTextBox renderer str styleCollection

exitWindow :: SDL.Window -> IO ()
exitWindow window = do
    Font.quit

    SDL.destroyWindow window
    SDL.quit
