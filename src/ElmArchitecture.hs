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
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ask, runReaderT)
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

initialiseSDL :: AppProps -> IO (SDL.Window, SDL.Renderer)
initialiseSDL props = do
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

initialiseState :: model -> App msg -> IO (AppState model msg)
initialiseState model appView = AppState
    <$> newMVar model
    <*> newMVar appView
    <*> newMVar Nothing

refreshApp
    :: Requirements msg model action
    -> SDL.Renderer
    -> msg
    -> MyReader model msg ()
refreshApp requirements renderer msg = do
    appState <- ask
    currentModel <- lift $ readMVar $ currentState appState
    let
        (newModel, maybeAction) = updateFn requirements msg currentModel
        newApp = viewFn requirements newModel

    void $ lift $ swapMVar (currentState appState) newModel
    void $ lift $ swapMVar (currentView appState) newApp

    lift $ renderApp newApp renderer

    case maybeAction of
        Just action -> do
            -- This needs to be made async
            newMsg <- lift $ actionFn requirements action

            refreshApp requirements renderer newMsg

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

    appState <- initialiseState model appView

    -- Initialise SDL objects
    (window, renderer) <- initialiseSDL $ appProps appView

    SDL.present renderer

    renderApp appView renderer

    let processMessage = refreshApp requirements renderer
    runReaderT (
            handleEvents
            window
            (`getMessageForEventPayload` processMessage)
        ) appState

handleEvents
    :: SDL.Window
    -> (SDL.EventPayload -> MyReader model msg ())
    -> MyReader model msg ()
handleEvents window processEvent = do
    event <- SDL.pollEvent
    case event of
        Just (SDL.Event _ eventPayload) -> do
            case eventPayload of
                SDL.QuitEvent ->
                    lift $ exitWindow window
                _ -> do
                    processEvent eventPayload
                    handleEvents window processEvent
        Nothing ->
            handleEvents window processEvent
