module ScrollTo exposing
    ( Status, Msg
    , init, update, subscriptions
    , toPosition
    , withDelay, withDuration, withEasing
    )

{-|

@docs Status, Msg


# Setup

@docs init, update, subscriptions


# Run the Scroll Animation

@docs toPosition


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
    | Animating
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


{-| Track scroll messages.

    type Msg
        = ScrollToMsg ScrollTo.Msg

-}
type Msg
    = GotViewport Position Browser.Dom.Viewport
    | StartAnimation Position Browser.Dom.Viewport
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
                    , Animation.wait animation <| StartAnimation to viewport
                    )

                Animating { animation } ->
                    ( scroll
                    , Animation.wait animation <| StartAnimation to viewport
                    )

        StartAnimation to { scene, viewport } ->
            case scroll of
                Waiting animation ->
                    ( Animating
                        { scene = { height = scene.height, width = scene.width }
                        , to = to
                        , from = { x = viewport.x, y = viewport.y }
                        , animation = animation
                        , elapsed = 0
                        }
                    , Cmd.none
                    )

                Animating { animation } ->
                    ( Animating
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
                Animating ({ to, from, animation, elapsed, scene } as data) ->
                    let
                        time =
                            delta + elapsed

                        step =
                            Animation.step time animation
                    in
                    if Animation.isFinished animation time then
                        ( Waiting animation
                        , Task.perform (\_ -> NoOp) <|
                            Browser.Dom.setViewport to.x to.y
                        )

                    else
                        ( Animating { data | elapsed = time }
                        , Task.perform (\_ -> NoOp) <|
                            Browser.Dom.setViewport
                                (clamp 0
                                    scene.width
                                    (from.x + (to.x - from.x) * step)
                                )
                                (clamp 0
                                    scene.height
                                    (from.y + (to.y - from.y) * step)
                                )
                        )

                _ ->
                    ( scroll, Cmd.none )

        NoOp ->
            ( scroll, Cmd.none )


{-| Subscribe to scroll animation updates.

    ScrollTo.subscriptions ScrollToMsg model.scrollToStatus

-}
subscriptions : (Msg -> msg) -> Status -> Sub msg
subscriptions msg scroll =
    case scroll of
        Animating _ ->
            Browser.Events.onAnimationFrameDelta (\delta -> msg <| Step delta)

        _ ->
            Sub.none
