module ElmArchitecture.Render where

import Paths_elm_architecture

import Control.Lens hiding (element)
import Control.Monad
import Data.StateVar
import qualified Data.Text as T
import System.FilePath

import Foreign.C.Types
import Foreign.Storable (peek)

import qualified SDL.Init as SDL
import qualified SDL.Video.Renderer as SDL
import qualified SDL.Video as SDL

import qualified SDL.Raw.Types as Raw

import qualified SDL.Font as Font

import ElmArchitecture.Types as T
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
    TextBoxElement str alignment -> renderTextBox renderer str alignment styleCollection
    InputBox str -> renderInputBox renderer str styleCollection

renderBorder :: SDL.Renderer -> Color -> Point -> (Int, Int) -> IO ()
renderBorder renderer color point (w, h) =
    unless (w == 0 || h ==0) $ do
        void $ SDL.rendererDrawColor renderer $= fromColor color
        SDL.drawRect renderer (Just (toSDLRectangle
                                        point
                                        w
                                        h))

renderFill :: SDL.Renderer -> Color -> Point -> (Int, Int) -> IO ()
renderFill renderer color point (w, h) = do
    void $ SDL.rendererDrawColor renderer $= fromColor color
    SDL.fillRect renderer (Just (toSDLRectangle
                                    point
                                    w
                                    h))

renderRectangle :: SDL.Renderer -> Styles -> IO ()
renderRectangle renderer styleCollection = do
    let
        point = position styleCollection
        w = width styleCollection
        h = height styleCollection
    renderFill renderer (backgroundColor styleCollection) point (w, h)

    renderBorder renderer (borderColor styleCollection) point (w, h)

renderTextBox :: SDL.Renderer -> T.Text -> Maybe Alignment -> Styles -> IO ()
renderTextBox renderer str alignment styleCollection = do
    unless (T.null str) $ do
        let
            fs = fontSize styleCollection
            userWidth = width styleCollection
            userHeight = height styleCollection

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
            (CInt surfaceWidth) = Raw.surfaceW rawSurface
            (CInt surfaceHeight) = Raw.surfaceH rawSurface
        textTexture <- SDL.createTextureFromSurface renderer textSurface

        let
            croppingRectangle =
                if userWidth /= 0 && userHeight /= 0
                    then Just
                        $ toSDLRectangle (Point 0 0) (fromIntegral userWidth) (fromIntegral userHeight)
                    else Nothing
            textWidth = if userWidth == 0 then surfaceWidth else min surfaceWidth $ fromIntegral userWidth
            textHeight = if userHeight == 0 then surfaceHeight else min surfaceHeight $ fromIntegral userHeight

            boxWidth = if userWidth == 0 then fromIntegral surfaceWidth else userWidth
            boxHeight = if userHeight == 0 then fromIntegral surfaceHeight else userHeight

            alignedPoint = getTextBoxPosition
                (userWidth, userHeight)
                (fromIntegral surfaceWidth, fromIntegral surfaceHeight)
                alignment
                (position styleCollection)
            
            bgColor = backgroundColor styleCollection
            brdColor = borderColor styleCollection
            point = position styleCollection

        renderFill renderer bgColor point (boxWidth, boxHeight)
        SDL.copy
            renderer
            textTexture
            croppingRectangle
            (Just $ toSDLRectangle alignedPoint (fromIntegral textWidth) (fromIntegral textHeight))
        renderBorder renderer brdColor point (boxWidth, boxHeight)

        SDL.destroyTexture textTexture
        SDL.freeSurface textSurface

    when (T.null str) $
        renderRectangle renderer styleCollection

renderInputBox :: SDL.Renderer -> T.Text -> Styles -> IO ()
renderInputBox renderer str styleCollection = do
    renderRectangle renderer styleCollection
    renderTextBox renderer str Nothing styleCollection

getTextBoxPosition :: (Int, Int) -> (Int, Int) -> Maybe Alignment -> Point -> Point
getTextBoxPosition (uw, uh) (sw, sh) maybeAlignment (Point x1 y1) =
    case maybeAlignment of
        Just (Alignment hAlign vAlign) ->
            let
                dx =
                    -- If user width is 0 or less than surface width, no alignment is possible
                    if uw == 0 || uw <= sw
                        then 0
                        else case hAlign of
                            T.Left -> 0
                            Center -> (uw - sw) `div` 2
                            T.Right -> uw - sw

                dy =
                    -- If user height is 0 or less than surface height, no alignment is possible
                    if uh == 0 || uh <= sh
                        then 0
                        else case vAlign of
                            Top -> 0
                            Middle -> (uh - sh) `div` 2
                            Bottom -> uh - sh
            in
                Point (x1 + dx) (y1 + dy)

        -- If no alignment is given, default to top left
        Nothing -> Point x1 y1

exitWindow :: SDL.Window -> IO ()
exitWindow window = do
    Font.quit

    SDL.destroyWindow window
    SDL.quit
