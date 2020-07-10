module ScrollTo exposing
    ( Status, Msg
    , init, initFor, update, subscriptions
    , toPosition
    , withDelay, withDuration, withEasing
    )

{-|

@docs Status, Msg


# Setup

@docs init, initFor, update, subscriptions


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
    = Waiting 
        { animation : Animation.Animation
        , target : Target
        }
    | Animating
        { scene : Dimensions
        , to : Position
        , from : Position
        , animation : Animation.Animation
        , elapsed : Float
        , target : Target
        }


type alias Dimensions =
    { height : Float
    , width : Float
    }


type alias Position =
    { x : Float
    , y : Float
    }


type Target
    = Window
    | Element String


{-| Setup a basic scroll command.

    ScrollTo.init

-}
init : Status
init =
    Waiting 
        { animation = Animation.init
        , target = Window
        }


{-| Setup a scroll command for a specific scrollable element 
    (not the whole window)

    ScrollTo.initFor "element-id"

-}
initFor : String -> Status
initFor id =
    Waiting
        { animation = Animation.init
        , target = Element id
        }


{-| Add a delay (in ms) to your scroll command.

    -- default: 0
    ScrollTo.withDelay 1000

-}
withDelay : Float -> Status -> Status
withDelay delay scroll =
    case scroll of
        Waiting ({ animation } as data) ->
            Waiting <| 
                { data | animation = Animation.withDelay delay animation }

        _ ->
            scroll


{-| Add a duration (in ms) to your scroll command.

    -- default: 1000
    ScrollTo.withDuration 5000

-}
withDuration : Float -> Status -> Status
withDuration duration scroll =
    case scroll of
        Waiting ({ animation } as data) ->
            Waiting <| 
                { data | animation = Animation.withDuration duration animation }

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
        Waiting ({ animation } as data) ->
            Waiting <| 
                { data | animation = Animation.withEasing easing animation }

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
toPosition : Position -> Status -> Cmd Msg
toPosition to scroll =
    let 
        tgt = {-for either whole window of scrollable elment-}
            case scroll of
                Waiting { target } ->
                    target
                Animating { target } ->
                    target
    in
        let 
            task = 
                case tgt of 
                    Window ->
                        Browser.Dom.getViewport
                    Element id ->
                        Browser.Dom.getViewportOf id
        in
            Task.attempt (GotViewport to) task


{-| Track scroll messages.

    type Msg
        = ScrollToMsg ScrollTo.Msg

-}
type Msg
    = GotViewport Position ( Result Browser.Dom.Error Browser.Dom.Viewport )
    | StartAnimation Position Browser.Dom.Viewport
    | Step Float
    | NoOp


{-| Handle updates from the scroll animation.

    ScrollTo.update scrollToMsg model.scrollToStatus

-}
update : Msg -> Status -> ( Status, Cmd Msg )
update msg scroll =
    case msg of
        GotViewport to result ->
            case result of
                Ok viewport ->
                    case scroll of
                        Waiting { animation } ->
                            ( scroll
                            , Animation.wait animation <| StartAnimation to viewport
                            )

                        Animating { animation } ->
                            ( scroll
                            , Animation.wait animation <| StartAnimation to viewport
                            )
                Err _ ->
                    ( scroll, Cmd.none)

        StartAnimation to { scene, viewport } ->
            case scroll of
                Waiting { animation, target } ->
                    ( Animating
                        { scene = { height = scene.height, width = scene.width }
                        , to = to
                        , from = { x = viewport.x, y = viewport.y }
                        , animation = animation
                        , elapsed = 0
                        , target = target
                        }
                    , Cmd.none
                    )

                Animating { animation, target } ->
                    ( Animating
                        { scene = { height = scene.height, width = scene.width }
                        , to = to
                        , from = { x = viewport.x, y = viewport.y }
                        , animation = animation
                        , elapsed = 0
                        , target = target
                        }
                    , Cmd.none
                    )

        Step delta ->
            case scroll of
                Animating ({ to, from, animation, elapsed, scene, target } as data) ->
                    let
                        time =
                            delta + elapsed

                        step =
                            Animation.step time animation
 
                        task = {-for either window, or scrollable element-}
                            case target of
                                Window ->
                                    Browser.Dom.setViewport
                                Element id ->
                                    Browser.Dom.setViewportOf id
                    in
                    if Animation.isFinished animation time then
                        ( Waiting { animation = animation, target = target }
                        , Task.attempt (\_ -> NoOp) <|
                            task to.x to.y
                        )

                    else
                        ( Animating { data | elapsed = time }
                        , Task.attempt (\_ -> NoOp) <|
                            task
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
