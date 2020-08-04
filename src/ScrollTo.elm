module ScrollTo exposing
    ( Status, Msg
    , init, update, subscriptions
    , toPosition, toPositionOf
    , withDelay, withDuration, withEasing
    )

{-|

@docs Status, Msg


# Setup

@docs init, update, subscriptions


# Run the Scroll Animation

@docs toPosition, toPositionOf


# Animation Customizations

@docs withDelay, withDuration, withEasing

-}

import Browser.Dom
import Browser.Events
import ScrollTo.Animation as Animation
import Task


{-| Represent the state of the scroll; the status will remain waiting until a
scroll command is issued.

    type alias Model =
        { scrollToStatus : ScrollTo.Status }}

-}
type Status
    = Waiting Animation.Animation
    | Animating Target Data


type Target
    = Window
    | Element String


type alias Data =
    { scene : Dimensions
    , to : Position
    , from : Position
    , animation : Animation.Animation
    , elapsed : Float
    }


type alias Dimensions =
    { height : Float
    , width : Float
    }


type alias Position =
    { x : Float
    , y : Float
    }


{-| Setup a basic scroll command.

    ScrollTo.init

-}
init : Status
init =
    Waiting Animation.init


{-| Add a delay (in ms) to your scroll command.

    -- default: 0
    ScrollTo.withDelay 1000

-}
withDelay : Float -> Status -> Status
withDelay delay scroll =
    case scroll of
        Waiting animation ->
            Waiting <| Animation.withDelay delay animation

        _ ->
            scroll


{-| Add a duration (in ms) to your scroll command.

    -- default: 1000
    ScrollTo.withDuration 5000

-}
withDuration : Float -> Status -> Status
withDuration duration scroll =
    case scroll of
        Waiting animation ->
            Waiting <| Animation.withDuration duration animation

        _ ->
            scroll


{-| Add an easing function
([elm-community/easing-functions](https://package.elm-lang.org/packages/elm-community/easing-functions/latest))
to your scroll command.

    -- default: identity (linear)
    ScrollTo.withEasing Ease.inOutQuint

-}
withEasing : (Float -> Float) -> Status -> Status
withEasing easing scroll =
    case scroll of
        Waiting animation ->
            Waiting <| Animation.withEasing easing animation

        _ ->
            scroll


{-| Scroll to a position offset on the screen.

    -- to the top!
    ScrollTo.toPosition { x = 0, y = 0 }

    -- to x offset
    ScrollTo.toPosition { x = 1080, y = 0 }

    -- to y offset
    ScrollTo.toPosition { x = 0, y = 540 }

    -- to x,y offset
    ScrollTo.toPosition { x = 1080, y = 540 }

-}
toPosition : Position -> Cmd Msg
toPosition to =
    Task.perform (GotViewport to) Browser.Dom.getViewport


{-| Scroll to a position offset within a nested scollable element.

    -- to the top!
    ScrollTo.toPositionOf "child" { x = 0, y = 0 }

    -- to x offset
    ScrollTo.toPositionOf "element" { x = 10, y = 0 }

    -- to y offset
    ScrollTo.toPositionOf "that" { x = 0, y = 54 }

    -- to x,y offset
    ScrollTo.toPositionOf "scrolls" { x = 10, y = 54 }

-}
toPositionOf : String -> Position -> Cmd Msg
toPositionOf el to =
    Task.attempt (GotViewportOf el to) <| Browser.Dom.getViewportOf el


{-| Track scroll messages.

    type Msg
        = ScrollToMsg ScrollTo.Msg

-}
type Msg
    = GotViewport Position Browser.Dom.Viewport
    | GotViewportOf String Position (Result Browser.Dom.Error Browser.Dom.Viewport)
    | StartAnimation Target Position Browser.Dom.Viewport
    | Step Float
    | NoOp


{-| Handle updates from the scroll animation.

    ScrollTo.update scrollToMsg model.scrollToStatus

-}
update : Msg -> Status -> ( Status, Cmd Msg )
update msg scroll =
    case msg of
        GotViewport to viewport ->
            case scroll of
                Waiting animation ->
                    ( scroll
                    , Animation.wait animation <|
                        StartAnimation Window to viewport
                    )

                Animating _ { animation } ->
                    ( scroll
                    , Animation.wait animation <|
                        StartAnimation Window to viewport
                    )

        GotViewportOf el to result ->
            case result of
                Ok viewport ->
                    case scroll of
                        Waiting animation ->
                            ( scroll
                            , Animation.wait animation <|
                                StartAnimation (Element el) to viewport
                            )

                        Animating _ { animation } ->
                            ( scroll
                            , Animation.wait animation <|
                                StartAnimation (Element el) to viewport
                            )

                Err _ ->
                    ( scroll, Cmd.none )

        StartAnimation target to { scene, viewport } ->
            case scroll of
                Waiting animation ->
                    ( Animating target
                        { scene = { height = scene.height, width = scene.width }
                        , to = to
                        , from = { x = viewport.x, y = viewport.y }
                        , animation = animation
                        , elapsed = 0
                        }
                    , Cmd.none
                    )

                Animating _ { animation } ->
                    ( Animating target
                        { scene = { height = scene.height, width = scene.width }
                        , to = to
                        , from = { x = viewport.x, y = viewport.y }
                        , animation = animation
                        , elapsed = 0
                        }
                    , Cmd.none
                    )

        Step delta ->
            case scroll of
                Animating target data ->
                    let
                        time =
                            delta + data.elapsed
                    in
                    if Animation.isFinished data.animation time then
                        ( Waiting data.animation, finish target data )

                    else
                        ( Animating target { data | elapsed = time }
                        , next target time data
                        )

                _ ->
                    ( scroll, Cmd.none )

        NoOp ->
            ( scroll, Cmd.none )


finish : Target -> Data -> Cmd Msg
finish target { to } =
    case target of
        Window ->
            Task.perform (\_ -> NoOp) <| Browser.Dom.setViewport to.x to.y

        Element el ->
            Task.attempt (\_ -> NoOp) <| Browser.Dom.setViewportOf el to.x to.y


next : Target -> Float -> Data -> Cmd Msg
next target time { scene, to, from, animation } =
    let
        step =
            Animation.step time animation

        x =
            clamp 0 scene.width (from.x + (to.x - from.x) * step)

        y =
            clamp 0 scene.height (from.y + (to.y - from.y) * step)
    in
    case target of
        Window ->
            Task.perform (\_ -> NoOp) <| Browser.Dom.setViewport x y

        Element el ->
            Task.attempt (\_ -> NoOp) <| Browser.Dom.setViewportOf el x y


{-| Subscribe to scroll animation updates.

    ScrollTo.subscriptions ScrollToMsg model.scrollToStatus

-}
subscriptions : (Msg -> msg) -> Status -> Sub msg
subscriptions msg scroll =
    case scroll of
        Animating _ _ ->
            Browser.Events.onAnimationFrameDelta (\delta -> msg <| Step delta)

        _ ->
            Sub.none
