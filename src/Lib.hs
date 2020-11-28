{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Concurrent.MVar
import Control.Monad
import Control.Lens hiding (element)
-- import Control.Monad

import Foreign.C.Types

import qualified SDL.Init as SDL
import qualified SDL.Event as SDL
import qualified SDL.Vect as SDL
import qualified SDL.Video as SDL

import qualified SDL.Font as Font

import Types
import Helpers
import Render

initialise :: AppProps -> IO (SDL.Window, SDL.Renderer)
initialise props = do
    -- Initialise SDL in video mode
    SDL.initialize [SDL.InitVideo]

    Font.initialize

    -- Create a new SDL Window according to the window props provided
    window <- SDL.createWindow (props ^. name)
                 $ SDL.defaultWindow
                 { SDL.windowInitialSize = SDL.V2
                                            (intToCInt $ props ^. appWidth)
                                            (intToCInt $ props ^. appHeight)
                 }

    -- Create a renderer with default renderer options for the above window
    renderer <- SDL.createRenderer window (CInt (-1)) SDL.defaultRenderer

    pure (window, renderer)

refreshApp :: Requirements msg model -> SDL.Renderer -> MVar model -> MVar (App msg) -> msg -> IO ()
refreshApp requirements renderer modelMVar appMVar msg = do
    currentModel <- readMVar modelMVar
    let
        newModel = updateFn requirements msg currentModel
        newApp = viewFn requirements newModel

    void $ swapMVar modelMVar newModel
    void $ swapMVar appMVar newApp

    renderApp newApp renderer

elmArchitecture :: (Eq msg, Eq model) => Requirements msg model -> IO ()
elmArchitecture requirements = do
    let
        model = initModel requirements
        app = viewFn requirements model

    modelMVar <- newMVar model
    appMVar <- newMVar app

    -- Initialise SDL objects
    (window, renderer) <- initialise $ appProps app

    currentInputBoxMVar <- newMVar Nothing

    SDL.present renderer

    renderApp app renderer

    handleEvents
        window
        (refreshApp requirements renderer modelMVar appMVar)
        (getMessageForEventPayload app currentInputBoxMVar)

handleEvents :: (Eq msg)
    => SDL.Window
    -> (msg -> IO ())
    -> (
        (msg -> IO ())
        -> SDL.EventPayload
        -> IO ()
        )
    -> IO ()
handleEvents window processMessage processEvent = do
    event <- SDL.pollEvent
    case event of
        Just (SDL.Event _ eventPayload) -> do
            case eventPayload of
                SDL.QuitEvent ->
                    exitWindow window
                _ -> do
                    processEvent processMessage eventPayload
                    handleEvents window processMessage processEvent
        Nothing ->
            handleEvents window processMessage processEvent
