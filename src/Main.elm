module Main exposing (main)

import Browser
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Html
import List
import String
import Set
import Json.Decode



-- MODEL


type alias Model =
    { firstQuad : Quad
    , secondQuad : Quad
    , thirdQuad : Quad
    , fourthQuad : Quad
    , currentGuess : String
    , maxGuesses : Int
    , guesses : List Word
    , validWords : Set.Set String
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

currentWordToWord : String -> Maybe Word
currentWordToWord currentWord =
    case String.toList currentWord of
        (firstLetter :: secondLetter :: thirdLetter :: fourthLetter :: fifthLetter :: _) ->
            Just
                { firstLetter = firstLetter
                , secondLetter = secondLetter
                , thirdLetter = thirdLetter
                , fourthLetter = fourthLetter
                , fifthLetter = fifthLetter
                }
        _ -> Nothing


type alias Flags =
    { answers : List String
    , allowed : List String
    }


init : Json.Decode.Value -> ( Model, Cmd Msg )
init flags =
    let
        decoder =
            Json.Decode.map2
                Flags
                (Json.Decode.field "answers" (Json.Decode.list Json.Decode.string))
                (Json.Decode.field "allowed" (Json.Decode.list Json.Decode.string))
        decodedFlags =
            case Json.Decode.decodeValue decoder flags of
                Ok decodedWords ->
                    decodedWords
                Err _ ->
                    {allowed = [], answers = []}

    in
    ( { firstQuad =
            { word =
                { firstLetter = 'A'
                , secondLetter = 'A'
                , thirdLetter = 'A'
                , fourthLetter = 'A'
                , fifthLetter = 'A'
                }
            }
      , secondQuad =
            { word =
                { firstLetter = 'B'
                , secondLetter = 'B'
                , thirdLetter = 'B'
                , fourthLetter = 'B'
                , fifthLetter = 'B'
                }
            }
      , thirdQuad =
            { word =
                { firstLetter = 'C'
                , secondLetter = 'C'
                , thirdLetter = 'C'
                , fourthLetter = 'C'
                , fifthLetter = 'C'
                }
            }
      , fourthQuad =
            { word =
                { firstLetter = 'D'
                , secondLetter = 'D'
                , thirdLetter = 'D'
                , fourthLetter = 'D'
                , fifthLetter = 'D'
                }
            }
      , guesses =
            [ { firstLetter = 'T'
              , secondLetter = 'E'
              , thirdLetter = 'A'
              , fourthLetter = 'R'
              , fifthLetter = 'S'
              }
            , { firstLetter = 'F'
              , secondLetter = 'E'
              , thirdLetter = 'A'
              , fourthLetter = 'R'
              , fifthLetter = 'S'
              }
            ]
      , currentGuess = "ABC"
      , maxGuesses = 9
      , validWords = Set.fromList decodedFlags.allowed
      }
    , Cmd.none
    )



type alias GuessResult =
    { green : Set.Set Int
    , yellow : Set.Set Int
    }

{--
checkGuess : String -> String -> GuessResult
checkGuess answer guess =
    let
        greenLetters = matchingLetters answer guess
        answerCounter = answer |> removeLetters greenLetters >> countLetters
        guessCounter = guess |> removeLetters greenLetters >> countLetters
    in
    { green = green
    , yellow = yellow
    }


matchingLetters : String -> String -> Set Int
matchingLetters first second =
--}


-- VIEW

backgroundColor = Element.rgb 0.12157 0.16078 0.2157
emptyRowColor = Element.rgb 0.066666 0.094117 0.15294
currentGuessLetterColor = Element.rgb 0.29411 0.3333333 0.38823
submittedGuessLetterColor = Element.rgb 0.2157 0.254901 0.3176
activeLetterColor = Element.rgb 0.13725 0.3137 0.38823
rounded = Border.rounded 5


view : Model -> Html.Html Msg
view model =
    Element.layout
        [ Background.color backgroundColor
        , Element.paddingXY 700 0
        , Element.width Element.fill
        , Element.height Element.fill
        ]
        (Element.column
            [ Background.color backgroundColor
            , Element.spacing 7
            , Element.width Element.fill
            , Element.height Element.fill
            ]
            [ Element.row
                [ Background.color backgroundColor
                , Element.width (Element.fillPortion 4)
                , Element.height (Element.fillPortion 4)
                , Element.spacing 7
                ]
                [ viewQuad model.maxGuesses model.currentGuess model.guesses model.firstQuad
                , viewQuad model.maxGuesses model.currentGuess model.guesses model.secondQuad
                ]
            , Element.row
                [ Background.color backgroundColor
                , Element.width (Element.fillPortion 4)
                , Element.height (Element.fillPortion 4)
                , Element.spacing 7
                ]
                [ viewQuad model.maxGuesses model.currentGuess model.guesses model.thirdQuad
                , viewQuad model.maxGuesses model.currentGuess model.guesses model.fourthQuad
                ]
            , Element.column
                [ Background.color backgroundColor
                , Element.width (Element.fillPortion 2)
                , Element.height (Element.fillPortion 2)
                , Element.spacing 7
                ]
                viewKeyboard
            ]
        )


viewQuad : Int -> String -> List Word -> Quad -> Element.Element msg
viewQuad totalRows currentGuess guesses quad =
    Element.column
        [ Background.color backgroundColor
        , Element.width Element.fill
        , Element.height Element.fill
        , Element.spacing 4
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
                [ Background.color emptyRowColor
                , Element.width Element.fill
                , Element.height Element.fill
                , rounded
                ]
                []
                :: viewEmptyRows (numEmptyRows - 1)


letterStyle =
    [ Background.color submittedGuessLetterColor
    , Element.width Element.fill
    , Element.height Element.fill
    , rounded
    ]



wordStyle =
    [ Background.color backgroundColor
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
        ((String.toList currentGuess |> List.map viewCurrentGuessLetter)
            ++ viewEmptyLetters numEmptyLetters
        )


viewLetter : Char -> Element.Element msg
viewLetter char =
    Element.el
        letterStyle
        (char |> String.fromChar >> Element.text)

viewCurrentGuessLetter : Char -> Element.Element msg
viewCurrentGuessLetter char =
    Element.el
        [ Background.color currentGuessLetterColor
        , Element.width Element.fill
        , Element.height Element.fill
        ]
        (char |> String.fromChar >> Element.text)


viewEmptyLetters : Int -> List (Element.Element msg)
viewEmptyLetters = viewEmptyLetters_ True

viewEmptyLetters_ : Bool -> Int -> List (Element.Element msg)
viewEmptyLetters_ first numEmptyLetters =
    if first && numEmptyLetters > 0 then
        Element.el
            [ Background.color activeLetterColor
            , Element.width Element.fill
            , Element.height Element.fill
            , rounded
            ]
            Element.none
        :: viewEmptyLetters_ False (numEmptyLetters - 1)
    else
        case numEmptyLetters of
            0 ->
                []

            _ ->
                Element.el
                    [ Background.color currentGuessLetterColor
                    , Element.width Element.fill
                    , Element.height Element.fill
                    , rounded
                    ]
                    Element.none
                    :: viewEmptyLetters_ False (numEmptyLetters - 1)


keyStyle =
    [ Background.color (Element.rgb 0 1 1)
    , Element.width Element.fill
    , Element.height Element.fill
    ]


viewKey : Char -> Element.Element Msg
viewKey char =
    Input.button
        keyStyle
        { onPress = Just (PressKey char)
        , label = char |> String.fromChar >> Element.text
        }


viewKeyboard : List (Element.Element Msg)
viewKeyboard =
    [ Element.row
        [ Element.height Element.fill
        , Element.width Element.fill
        , Element.spacing 7
        ]
        ("QWERTYUIOP" |> String.toList >> viewKeyboardRow)
    , Element.row
        [ Element.height Element.fill
        , Element.width Element.fill
        , Element.spacing 7
        ]
        ("ASDFGHJKL" |> String.toList >> viewKeyboardRow)
    , Element.row
        [ Element.height Element.fill
        , Element.width Element.fill
        , Element.spacing 7
        ]
        ("ZXCVBNM" |> String.toList >> viewKeyboardBottomRow)
    ]


viewKeyboardRow : List Char -> List (Element.Element Msg)
viewKeyboardRow keys =
    List.map viewKey keys


viewKeyboardBottomRow : List Char -> List (Element.Element Msg)
viewKeyboardBottomRow letters =
    Input.button
        keyStyle
        { onPress = Just PressBackspace
        , label = Element.text "BKSPC"
        }
        :: viewKeyboardRow letters
        ++ [ Input.button
                keyStyle
                { onPress = Just PressEnter
                , label = Element.text "ENTER"
                }
           ]



-- UPDATE


type Msg
    = PressKey Char
    | PressBackspace
    | PressEnter


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PressKey char ->
            if String.length model.currentGuess < 5 then
                ( { model
                    | currentGuess =
                        String.append model.currentGuess (String.fromChar char)
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        PressBackspace ->
            if String.length model.currentGuess == 0 then
                ( model, Cmd.none )

            else
                ( { model
                    | currentGuess = String.dropRight 1 model.currentGuess
                  }
                , Cmd.none
                )

        PressEnter ->
            if isValidGuess model.validWords model.currentGuess then
                (
                  commitGuess model
                , Cmd.none
                )
            else
                ( model, Cmd.none )


isValidGuess : Set.Set String -> String -> Bool
isValidGuess allowedWords guess =
    String.length guess >= 5 && Set.member guess allowedWords


commitGuess : Model -> Model
commitGuess model =
    case currentWordToWord model.currentGuess of
        Just guess ->
            { model
            | currentGuess =
                ""
            , guesses =
                model.guesses ++ [guess]
            , validWords =
                Set.remove model.currentGuess model.validWords
            }
        Nothing ->
            model



-- SUBSCRIPTIONS


subscriptions model =
    Sub.none



-- MAIN


main : Program Json.Decode.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
