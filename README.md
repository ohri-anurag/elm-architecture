# elm-architecture

Hello there, fellow Haskeller! This library is an attempt to build an [Elm](https://elm-lang.org/)-like framework in Haskell.

It uses [sdl2](https://github.com/haskell-game/sdl2) and [sdl-ttf](https://github.com/haskell-game/sdl2-ttf) for rendering.

## Basics

The app is built on 4 pillars. They define everything that the does, and displays.
1. Model - This defines the state of the app
2. View - This defines how the app looks for a particular instance of Model.
3. Update - This defines how the app updates itself in response to user interactions.
4. Action - This defines how an effectful(having side-effects) task can be triggered from the UI.

## Flow

![Flow Diagram](doc/Flow.svg)

## Types

The function that is responsible for creating and displaying your entire app is this one:

```haskell
elmArchitecture :: (Eq msg, Eq model)
    => model                                    -- Initial Model
    -> (model -> App msg)                       -- The view function
    -> (msg -> model -> (model, Maybe action))  -- The update function
    -> (action -> IO msg)                       -- The action handler, for side-effects
    -> IO ()
```

As you can see, most of the types above are user-defined, the only exception being `App`.

Users familiar with `Elm` will also notice the similarity between the generic `action` type used here and Cmd type used in `Elm`. My main rationale for not following the Elm way here is the difference between a browser app and an actual app.

Browser applications are restricted in many ways, for example, writing to a file on the user's system. Haskell applications, on the other hand, are not restricted, Haskell being a full fledged programming language. Actions can be thought of as analogous to subscriptions.

## TODO

- [ ] Implement border width (make sure font doesn't override border, change the cropping rectangle)
- [ ] Add documentation for view types, including `App`.
- [ ] Add documentation for examples, inside `examples` directory.
- [ ] Add more rendering capabilities (scrolling etc).
- [ ] Make action handling async.
- [ ] Add a font cache.
- [ ] Make all data types strict.
- [ ] Add as a library to Hackage/Stackage.
