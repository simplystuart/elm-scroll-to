module DefaultAnimation exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events
import ScrollTo



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { scroll : ScrollTo.Status }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { scroll = ScrollTo.init }, Cmd.none )



-- UPDATE


type Msg
    = ClickScrollToTop
    | ScrollMsg ScrollTo.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickScrollToTop ->
            ( model
            , Cmd.map ScrollMsg <|
                ScrollTo.toPosition { x = 0, y = 0 } model.scroll
            )

        ScrollMsg scrollMsg ->
            Tuple.mapBoth (\status -> { model | scroll = status })
                (Cmd.map ScrollMsg)
                (ScrollTo.update scrollMsg model.scroll)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    ScrollTo.subscriptions ScrollMsg model.scroll



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "elm-scroll"
    , body =
        [ div [] (List.map colorDiv <| List.reverse <| List.range 0 100)
        , scrollToTopLink
        ]
    }



-- colorDiv


colorDiv : Int -> Html Msg
colorDiv i =
    let
        color =
            String.join "" [ "hsla(0,50%,", String.fromInt i, "%)" ]

        content =
            if i == 100 then
                [ text "Scroll down and then click \"Scroll To Top ↑\"" ]

            else
                []
    in
    div
        [ style "background-color" color
        , style "color" "#222"
        , style "font-family" "sans-serif"
        , style "text-align" "center"
        , style "width" "100%"
        , style "height" "40px"
        ]
        content



-- scrollToTopLink


scrollToTopLink : Html Msg
scrollToTopLink =
    div
        [ style "background-color" "#fff"
        , style "color" "#222"
        , style "line-height" "40px"
        , style "text-align" "center"
        , style "position" "fixed"
        , style "bottom" "0"
        , style "width" "100%"
        , style "height" "40px"
        ]
        [ span
            [ style "cursor" "pointer"
            , style "font-family" "sans-serif"
            , Events.onClick ClickScrollToTop
            ]
            [ text "Scroll To Top ↑" ]
        ]
