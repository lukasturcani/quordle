module Main exposing (main)

import Browser
import Element exposing (Element, el, layout, text)
import Html



-- MODEL


type alias Model =
    {}


init : flags -> ( Model, Cmd Msg )
init _ =
    ( {}, Cmd.none )



-- VIEW


view : Model -> Html.Html Msg
view model =
    layout
        []
        (text "this")



-- UPDATE


type Msg
    = Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions model =
    Sub.none



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
