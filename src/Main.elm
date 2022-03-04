module Main exposing (main)

import Browser
import Dict
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Json.Decode
import List
import Set
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

wordToLetters : Word -> List Letter
wordToLetters word =
    [ (0, word.firstLetter)
    , (1, word.secondLetter)
    , (2, word.thirdLetter)
    , (3, word.fourthLetter)
    , (4, word.firstLetter)
    ]


currentWordToWord : String -> Maybe Word
currentWordToWord currentWord =
    case String.toList currentWord of
        firstLetter :: secondLetter :: thirdLetter :: fourthLetter :: fifthLetter :: _ ->
            Just
                { firstLetter = firstLetter
                , secondLetter = secondLetter
                , thirdLetter = thirdLetter
                , fourthLetter = fourthLetter
                , fifthLetter = fifthLetter
                }

        _ ->
            Nothing


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
                    { allowed = [], answers = [] }
    in
    ( { firstQuad =
            { word =
                { firstLetter = 'T'
                , secondLetter = 'E'
                , thirdLetter = 'A'
                , fourthLetter = 'R'
                , fifthLetter = 'S'
                }
            }
      , secondQuad =
            { word =
                { firstLetter = 'D'
                , secondLetter = 'E'
                , thirdLetter = 'A'
                , fourthLetter = 'R'
                , fifthLetter = 'S'
                }
            }
      , thirdQuad =
            { word =
                { firstLetter = 'D'
                , secondLetter = 'I'
                , thirdLetter = 'N'
                , fourthLetter = 'E'
                , fifthLetter = 'R'
                }
            }
      , fourthQuad =
            { word =
                { firstLetter = 'S'
                , secondLetter = 'I'
                , thirdLetter = 'L'
                , fourthLetter = 'L'
                , fifthLetter = 'Y'
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

type alias Letter = (Int, Char)
type Thruple a b c= Thruple a b c
thrupleFirst (Thruple x _ _) = x
thrupleSecond (Thruple _ x _) = x
thrupleThird (Thruple _ _ x) = x

thrupleFromLetters : Letter -> Letter -> Thruple Int Char Char
thrupleFromLetters (index, x) (_, y) = Thruple index x y


b : (a -> b) -> (a -> c) -> (b -> c -> d) -> a -> d
b f g e x = e (f x) (g x)

bb : (a -> b) -> (c -> d -> a) -> c -> d -> b
bb f g x y = f (g x y)

c : (a -> b) -> (b -> c -> d) -> a -> c -> d
c f g x y = g (f x) y

flip : (a -> b -> c) -> b -> a -> c
flip f x y = f y x

dictUpdate : (Maybe v -> Maybe v) -> comparable -> Dict.Dict comparable v -> Dict.Dict comparable v
dictUpdate f k d = Dict.update k f d

checkGuess : List Letter -> List Letter -> GuessResult
checkGuess answer guess =
    let
        greenLetters =
            List.foldr
                (\x acc ->
                    if b thrupleSecond thrupleThird (==) x
                    then Set.insert (thrupleFirst x) acc
                    else acc
                )
                Set.empty
                (List.map2 thrupleFromLetters answer guess)

        answerCounter =
            answer
            |> List.filter
                (Tuple.first >> flip Set.member greenLetters >> not)
            |> List.foldr
                (c Tuple.second (dictUpdate incrementCount))
                Dict.empty
    in
    { green = greenLetters
    , yellow = getYellowLetters answerCounter guess
    }


incrementCount : Maybe Int -> Maybe Int
incrementCount count =
    case count of
        Nothing -> Just 1
        Just x -> Just (x + 1)


getWithDefault : v -> comparable -> Dict.Dict comparable v -> v
getWithDefault v k d =
    case Dict.get k d of
        Nothing ->
            v
        Just r ->
            r


getYellowLetters : Dict.Dict Char Int -> List Letter -> Set.Set Int
getYellowLetters answerCounter guess =
    (List.foldr
        (\(index, char) acc ->
            let
                letterCount = getWithDefault 0 char acc.answerCounter
            in
            if letterCount > 0
            then
                { yellowLetters =
                    Set.insert index acc.yellowLetters
                , answerCounter =
                    Dict.insert char (letterCount - 1) acc.answerCounter
                }
            else acc
        )
        { yellowLetters = Set.empty
        , answerCounter = answerCounter
        }
        guess
    ).yellowLetters

-- VIEW


currentGuessLetterColor =
    Element.rgb 0.29411 0.3333333 0.38823


fontColor =
    Element.rgb 1.0 1.0 1.0


rounded =
    Border.rounded 5


styleAttributes =
    { topLevelNode =
        [ Background.color (Element.rgb 0.12157 0.16078 0.2157)
        , Element.paddingXY 700 0
        ]
    , topLevelColumn =
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.spacing 7
        ]
    , quadRow =
        [ Element.width (Element.fillPortion 4)
        , Element.height (Element.fillPortion 4)
        , Element.spacing 7
        ]
    , keyboardRow =
        [ Element.width (Element.fillPortion 2)
        , Element.height (Element.fillPortion 2)
        , Element.spacing 7
        ]
    , quad =
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.spacing 4
        ]
    , letterRow =
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.spacing 7
        ]
    }


view : Model -> Html.Html Msg
view model =
    Element.layout
        styleAttributes.topLevelNode
        (Element.column
            styleAttributes.topLevelColumn
            [ viewQuadRow model model.firstQuad model.secondQuad
            , viewQuadRow model model.thirdQuad model.fourthQuad
            , viewKeyboard
            ]
        )


viewQuadRow : Model -> Quad -> Quad -> Element.Element msg
viewQuadRow model first second =
    Element.row
        styleAttributes.quadRow
        [ viewQuad model.maxGuesses model.currentGuess model.guesses first
        , viewQuad model.maxGuesses model.currentGuess model.guesses second
        ]


viewQuad : Int -> String -> List Word -> Quad -> Element.Element msg
viewQuad totalRows currentGuess guesses quad =
    Element.column
        styleAttributes.quad
        (List.concat
            [ List.map (viewWord (wordToLetters quad.word)) guesses
            , [ viewCurrentGuess currentGuess ]
            , viewEmptyRows (totalRows - 1 - List.length guesses)
            ]
        )


viewEmptyRows : Int -> List (Element.Element msg)
viewEmptyRows numEmptyRows =
    case numEmptyRows of
        0 ->
            []

        _ ->
            Element.row
                [ Background.color (Element.rgb 0.066666 0.094117 0.15294)
                , Element.width Element.fill
                , Element.height Element.fill
                , rounded
                ]
                []
                :: viewEmptyRows (numEmptyRows - 1)


viewWord : List Letter -> Word -> Element.Element msg
viewWord answer word =
    let
        guessResult = checkGuess answer (wordToLetters word)
    in
    Element.row
        styleAttributes.letterRow
        [ viewSubmittedLetter (letterColor guessResult 0) word.firstLetter
        , viewSubmittedLetter (letterColor guessResult 1) word.secondLetter
        , viewSubmittedLetter (letterColor guessResult 2) word.thirdLetter
        , viewSubmittedLetter (letterColor guessResult 3) word.fourthLetter
        , viewSubmittedLetter (letterColor guessResult 4) word.fifthLetter
        ]

letterColor : GuessResult -> Int -> Element.Color
letterColor guessResult index =
    case Set.member index guessResult.green of
        True ->
            Element.rgb 0.0 1.0 0.0
        False ->
            case Set.member index guessResult.yellow of
                True ->
                    Element.rgb 1.0 1.0 0.0
                False ->
                    Element.rgb 0.2157 0.254901 0.3176


viewSubmittedLetter : Element.Color -> Char -> Element.Element msg
viewSubmittedLetter color letter =
    Element.row
        [ Background.color color
        , Element.width Element.fill
        , Element.height Element.fill
        , rounded
        ]
        [ Element.el
            [ Font.color fontColor
            , Font.center
            , Element.centerX
            , Element.centerY
            , rounded
            ]
            (letter |> String.fromChar >> Element.text)
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
        styleAttributes.letterRow
        ((String.toList currentGuess |> List.map viewCurrentGuessLetter)
            ++ viewEmptyLetters numEmptyLetters
        )


viewCurrentGuessLetter : Char -> Element.Element msg
viewCurrentGuessLetter char =
    Element.row
        [ Background.color currentGuessLetterColor
        , Element.width Element.fill
        , Element.height Element.fill
        , rounded
        ]
        [ Element.el
            [ Font.color fontColor
            , Font.center
            , Element.centerX
            , Element.centerY
            ]
            (char |> String.fromChar >> Element.text)
        ]


viewEmptyLetters : Int -> List (Element.Element msg)
viewEmptyLetters =
    viewEmptyLetters_ True


viewEmptyLetters_ : Bool -> Int -> List (Element.Element msg)
viewEmptyLetters_ first numEmptyLetters =
    if first && numEmptyLetters > 0 then
        Element.el
            [ Background.color (Element.rgb 0.13725 0.3137 0.38823)
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
    [ Background.color (Element.rgb 0.4196 0.447 0.50196)
    , Element.width Element.fill
    , Element.height Element.fill
    , Font.center
    , Font.color fontColor
    , rounded
    ]


viewKey : Char -> Element.Element Msg
viewKey char =
    Input.button
        keyStyle
        { onPress = Just (PressKey char)
        , label = char |> String.fromChar >> Element.text
        }


viewKeyboard : Element.Element Msg
viewKeyboard =
    Element.column
        styleAttributes.keyboardRow
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
                ( commitGuess model
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
                    model.guesses ++ [ guess ]
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
