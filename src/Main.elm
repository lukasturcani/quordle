module Main exposing (main)

import Browser
import Element
import Element.Background as Background
import Element.Border as Border
import Html
import List
import String



-- MODEL


type alias Model =
    { firstQuad : Quad
    , secondQuad : Quad
    , thirdQuad : Quad
    , fourthQuad : Quad
    , currentGuess : String
    , maxGuesses : Int
    , guesses : List Word
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
      , guesses =
            [ { firstLetter = 't'
              , secondLetter = 'e'
              , thirdLetter = 'a'
              , fourthLetter = 'r'
              , fifthLetter = 's'
              }
            , { firstLetter = 'f'
              , secondLetter = 'e'
              , thirdLetter = 'a'
              , fourthLetter = 'r'
              , fifthLetter = 's'
              }
            ]
      , currentGuess = "abc"
      , maxGuesses = 9
      }
    , Cmd.none
    )



-- VIEW


view : Model -> Html.Html Msg
view model =
    Element.layout
        [ Background.color (Element.rgb 1 0 0)
        , Element.padding 7
        , Element.width Element.fill
        , Element.height Element.fill
        ]
        (Element.column
            [ Background.color (Element.rgb 1 1 0)
            , Element.spacing 7
            , Element.width Element.fill
            , Element.height Element.fill
            ]
            [ Element.row
                [ Background.color (Element.rgb 1 1 0)
                , Element.width Element.fill
                , Element.height Element.fill
                , Element.spacing 7
                ]
                [ viewQuad model.maxGuesses model.currentGuess model.guesses model.firstQuad
                , viewQuad model.maxGuesses model.currentGuess model.guesses model.secondQuad
                ]
            , Element.row
                [ Background.color (Element.rgb 1 1 0)
                , Element.width Element.fill
                , Element.height Element.fill
                , Element.spacing 7
                ]
                [ viewQuad model.maxGuesses model.currentGuess model.guesses model.thirdQuad
                , viewQuad model.maxGuesses model.currentGuess model.guesses model.fourthQuad
                ]
            ]
        )


viewQuad : Int -> String -> List Word -> Quad -> Element.Element msg
viewQuad totalRows currentGuess guesses quad =
    Element.column
        [ Background.color (Element.rgb 0 0 1)
        , Element.width Element.fill
        , Element.height Element.fill
        , Element.spacing 7
        ]
        (List.map viewWord guesses
            ++ [ viewCurrentGuess currentGuess ]
            ++ viewEmptyRows (totalRows - 1 - List.length guesses)
        )


viewEmptyRows : Int -> List (Element.Element msg)
viewEmptyRows numEmptyRows =
    case numEmptyRows of
        0 ->
            []

        _ ->
            Element.row
                wordStyle
                [ Element.el
                    letterStyle
                    Element.none
                , Element.el
                    letterStyle
                    Element.none
                , Element.el
                    letterStyle
                    Element.none
                , Element.el
                    letterStyle
                    Element.none
                , Element.el
                    letterStyle
                    Element.none
                ]
                :: viewEmptyRows (numEmptyRows - 1)


letterStyle =
    [ Background.color (Element.rgb 0 1 1)
    , Element.width Element.fill
    , Element.height Element.fill
    ]


wordStyle =
    [ Background.color (Element.rgb 1 0 1)
    , Element.width Element.fill
    , Element.height Element.fill
    , Element.spacing 7
    ]


viewWord : Word -> Element.Element msg
viewWord word =
    Element.row
        wordStyle
        [ Element.el
            letterStyle
            (word.firstLetter |> String.fromChar >> Element.text)
        , Element.el
            letterStyle
            (word.secondLetter |> String.fromChar >> Element.text)
        , Element.el
            letterStyle
            (word.thirdLetter |> String.fromChar >> Element.text)
        , Element.el
            letterStyle
            (word.fourthLetter |> String.fromChar >> Element.text)
        , Element.el
            letterStyle
            (word.fifthLetter |> String.fromChar >> Element.text)
        ]


viewCurrentGuess : String -> Element.Element msg
viewCurrentGuess currentGuess =
    let
        currentGuessLength =
            String.length currentGuess

        numEmptyLetters =
            if currentGuessLength < 5 then
                5 - currentGuessLength

            else
                0
    in
    Element.row
        wordStyle
        ((String.toList currentGuess |> List.map viewLetter)
            ++ viewEmptyLetters numEmptyLetters
        )


viewLetter : Char -> Element.Element msg
viewLetter char =
    Element.el
        letterStyle
        (char |> String.fromChar >> Element.text)


viewEmptyLetters : Int -> List (Element.Element msg)
viewEmptyLetters numEmptyLetters =
    case numEmptyLetters of
        0 ->
            []

        _ ->
            Element.el
                letterStyle
                Element.none
                :: viewEmptyLetters (numEmptyLetters - 1)



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
