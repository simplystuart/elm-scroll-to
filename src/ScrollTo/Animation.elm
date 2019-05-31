module ScrollTo.Animation exposing
    ( Animation
    , init
    , isFinished
    , step
    , wait
    , withDelay
    , withDuration
    , withEasing
    )

import Process
import Task



-- ANIMATION


type Animation
    = Animation
        { delay : Float
        , duration : Float
        , easing : Float -> Float
        }


init : Animation
init =
    Animation
        { delay = 0
        , duration = 1000
        , easing = identity
        }



-- WITH DELAY


withDelay : Float -> Animation -> Animation
withDelay delay (Animation data) =
    Animation { data | delay = delay }



-- WITH DURATION


withDuration : Float -> Animation -> Animation
withDuration duration (Animation data) =
    Animation { data | duration = duration }



-- WITH EASING


withEasing : (Float -> Float) -> Animation -> Animation
withEasing easing (Animation data) =
    Animation { data | easing = easing }



-- IS FINISHED


isFinished : Animation -> Float -> Bool
isFinished (Animation { duration }) elapsed =
    elapsed >= duration



-- STEP


step : Float -> Animation -> Float
step elapsed (Animation { duration, easing }) =
    easing <| elapsed / duration



-- WAIT


wait : Animation -> msg -> Cmd msg
wait (Animation { delay }) msg =
    Task.perform (\_ -> msg) <|
        Process.sleep delay
