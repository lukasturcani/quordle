module Main exposing (main)

import Browser
import Element exposing (Element, el, layout, text)
import Html



-- MODEL


type alias Model =
    { firstQuad : Quad
    , secondQuad : Quad
    , thirdQuad : Quad
    , fourthQuad : Quad
    , currentGuess : String
    , maxGuesses : Int
    , guesses : List String
    }


type alias Quad =
    { word : Word
    }


type alias Word =
    { firstLetter : Char
    , secondLetter : Char
    , thirdLetter : Char
    , fourthLetter : Char
    , fifthLetter : Char
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { firstQuad =
            { word =
                { firstLetter = 'a'
                , secondLetter = 'a'
                , thirdLetter = 'a'
                , fourthLetter = 'a'
                , fifthLetter = 'a'
                }
            }
      , secondQuad =
            { word =
                { firstLetter = 'b'
                , secondLetter = 'b'
                , thirdLetter = 'b'
                , fourthLetter = 'b'
                , fifthLetter = 'b'
                }
            }
      , thirdQuad =
            { word =
                { firstLetter = 'c'
                , secondLetter = 'c'
                , thirdLetter = 'c'
                , fourthLetter = 'c'
                , fifthLetter = 'c'
                }
            }
      , fourthQuad =
            { word =
                { firstLetter = 'd'
                , secondLetter = 'd'
                , thirdLetter = 'd'
                , fourthLetter = 'd'
                , fifthLetter = 'd'
                }
            }
      , guesses = []
      , currentGuess = ""
      , maxGuesses = 9
      }
    , Cmd.none
    )



-- VIEW


view : Model -> Html.Html Msg
view model =
    layout
        []
        (text "this")



-- UPDATE


type Msg
    = PressLetter Char
    | PressBackspace
    | PressEnter


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
