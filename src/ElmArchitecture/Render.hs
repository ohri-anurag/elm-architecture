module ElmArchitecture.Render where

import Paths_elm_architecture

import Control.Lens hiding (element)
import Control.Monad
import Data.StateVar
import qualified Data.Text as T
import System.FilePath

import Foreign.C.Types
import Foreign.Storable

import qualified SDL.Init as SDL
import qualified SDL.Video.Renderer as SDL
import qualified SDL.Video as SDL

import qualified SDL.Raw.Types as Raw

import qualified SDL.Font as Font

import ElmArchitecture.Types
import ElmArchitecture.Helpers

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
renderTextBox renderer str styleCollection = do
    unless (T.null str) $ do
        let
            fs = fontSize styleCollection
            cw = width styleCollection
            ch = height styleCollection

        loadedFont <- case fontFamily styleCollection of
            DefaultFont -> do
                defaultDir <- getDataDir
                Font.load (defaultDir </> "font/FreeSans.ttf") fs
            UserFont path -> Font.load path fs

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

        let
            croppingRectangle =
                if cw /= 0 && ch /= 0
                    then Just $ toSDLRectangle (Point 0 0) (fromIntegral cw) (fromIntegral ch)
                    else Nothing
            w = if cw == 0 then sw else min sw $ fromIntegral cw
            h = if ch == 0 then sh else min sh $ fromIntegral ch

        renderRectangle renderer $ styleCollection
            { width = if cw == 0 then fromIntegral sw else cw
            , height = if ch == 0 then fromIntegral sh else ch
            }
        SDL.copy
            renderer
            textTexture
            croppingRectangle
            (Just $ toSDLRectangle (position styleCollection) (fromIntegral w) (fromIntegral h))

        SDL.destroyTexture textTexture
        SDL.freeSurface textSurface

    when (T.null str) $
        renderRectangle renderer styleCollection

renderInputBox :: SDL.Renderer -> T.Text -> Styles -> IO ()
renderInputBox renderer str styleCollection = do
    renderRectangle renderer styleCollection
    renderTextBox renderer str styleCollection

exitWindow :: SDL.Window -> IO ()
exitWindow window = do
    Font.quit

    SDL.destroyWindow window
    SDL.quit
