{-# LANGUAGE OverloadedStrings #-}

module ElmArchitecture (
    elmArchitecture,

    module ElmArchitecture.API,

    App,
    Styles(..),

    module Data.Colour.Names
) where

import Control.Concurrent.MVar
import Control.Monad
import Control.Lens hiding (element, view)

import Foreign.C.Types

import qualified SDL.Init as SDL
import qualified SDL.Event as SDL
import qualified SDL.Vect as SDL
import qualified SDL.Video as SDL

import qualified SDL.Font as Font

import ElmArchitecture.Types
import ElmArchitecture.Helpers
import ElmArchitecture.Render
import ElmArchitecture.API

import Data.Colour.Names

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

refreshApp :: Requirements msg model action -> SDL.Renderer -> MVar model -> MVar (App msg) -> msg -> IO ()
refreshApp requirements renderer modelMVar appMVar msg = do
    currentModel <- readMVar modelMVar
    let
        (newModel, maybeAction) = updateFn requirements msg currentModel
        newApp = viewFn requirements newModel

    void $ swapMVar modelMVar newModel
    void $ swapMVar appMVar newApp

    renderApp newApp renderer

    case maybeAction of
        Just action -> do
            newMsg <- actionFn requirements action

            refreshApp requirements renderer modelMVar appMVar newMsg

        Nothing -> pure ()

elmArchitecture :: (Eq msg, Eq model)
    => model                                    -- Initial Model
    -> (model -> App msg)                       -- The view function
    -> (msg -> model -> (model, Maybe action))  -- The update function
    -> (action -> IO msg)                       -- The action handler, for side-effects
    -> IO ()
elmArchitecture model view update action = do
    let
        appView = view model
        requirements = Requirements model update view action

    modelMVar <- newMVar model
    appMVar <- newMVar appView

    -- Initialise SDL objects
    (window, renderer) <- initialise $ appProps appView

    currentInputBoxMVar <- newMVar Nothing

    SDL.present renderer

    renderApp appView renderer

    handleEvents
        window
        (refreshApp requirements renderer modelMVar appMVar)
        (getMessageForEventPayload appView currentInputBoxMVar)

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
