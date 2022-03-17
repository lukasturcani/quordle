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
    { answer : Word
    }


type alias Word =
    List Char


currentWordToWord : String -> Maybe Word
currentWordToWord currentWord =
    case String.toList currentWord of
        firstLetter :: secondLetter :: thirdLetter :: fourthLetter :: fifthLetter :: _ ->
            Just [ firstLetter, secondLetter, thirdLetter, fourthLetter, fifthLetter ]

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
    ( { firstQuad = { answer = String.toList "TEARS" }
      , secondQuad = { answer = String.toList "DEARS" }
      , thirdQuad = { answer = String.toList "DINER" }
      , fourthQuad = { answer = String.toList "SILLY" }
      , guesses =
            [ String.toList "TEARS"
            , String.toList "FEARS"
            ]
      , currentGuess = "ABC"
      , maxGuesses = 9
      , validWords =
            Set.union
                (Set.fromList decodedFlags.allowed)
                (Set.fromList decodedFlags.answers)
      }
    , Cmd.none
    )


type Thruple a b c
    = Thruple a b c


thrupleFirst (Thruple x _ _) =
    x


thrupleSecond (Thruple _ x _) =
    x


thrupleThird (Thruple _ _ x) =
    x


b : (a -> b) -> (a -> c) -> (b -> c -> d) -> a -> d
b f g e x =
    e (f x) (g x)


bb : (a -> b) -> (c -> d -> a) -> c -> d -> b
bb f g x y =
    f (g x y)


c : (a -> b) -> (b -> c -> d) -> a -> c -> d
c f g x y =
    g (f x) y


flip : (a -> b -> c) -> b -> a -> c
flip f x y =
    f y x


dictUpdate : (Maybe v -> Maybe v) -> comparable -> Dict.Dict comparable v -> Dict.Dict comparable v
dictUpdate f k d =
    Dict.update k f d


incrementCount : Maybe Int -> Maybe Int
incrementCount count =
    case count of
        Nothing ->
            Just 1

        Just x ->
            Just (x + 1)


getWithDefault : v -> comparable -> Dict.Dict comparable v -> v
getWithDefault v k d =
    case Dict.get k d of
        Nothing ->
            v

        Just r ->
            r


getYellowLetters : Dict.Dict Char Int -> Word -> Set.Set Int
getYellowLetters answerCounter guess =
    (List.foldl
        (\( index, char ) acc ->
            let
                letterCount =
                    getWithDefault 0 char acc.answerCounter
            in
            if letterCount > 0 then
                { yellowLetters =
                    Set.insert index acc.yellowLetters
                , answerCounter =
                    Dict.insert char (letterCount - 1) acc.answerCounter
                }

            else
                acc
        )
        { yellowLetters = Set.empty
        , answerCounter = answerCounter
        }
        (List.map2 Tuple.pair (List.range 0 (List.length guess)) guess)
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


type alias ModelStyle msg =
    { elementLayout : List (Element.Attribute msg)
    , quadRow : QuadRowStyle msg
    }

type alias QuadRowStyle msg =
    { elementRow : List (Element.Attribute msg)
    , quad : QuadStyle msg
    }

type alias QuadRowView =
    { first: QuadView
    , second: QuadView
    }

type alias QuadStyle msg =
    { elementColumn : List (Element.Attribute msg)
    , emptyRow : EmptyRowStyle msg
    }

type alias QuadView =
    { totalRows : Int
    , currentGuess : String
    , guesses : List Word
    , quad : Quad
    }

type alias EmptyRowStyle msg =
    { elementRow : List (Element.Attribute msg)
    }

modelStyle : ModelStyle msg
modelStyle =
    { elementLayout = []
    , quadRow =
        { elementRow = []
        , quad =
            { elementColumn = []
            , emptyRow =
                { elementRow = []
                }
            }
        }
    }


view : Model -> Html.Html Msg
view model =
    let
        quadRowView =
            { totalRows = model.maxGuesses
            , currentGuess = model.currentGuess
            , guesses = model.guesses
            }
    in
    Element.layout
        modelStyle.elementLayout
        Element.lazy viewMod
        (Element.column
            styleAttributes.topLevelColumn
            [ viewQuadRow
                modelStyle.quadRow
                { quadRowView | first = model.firstQuad }
            , viewQuadRow
                modelStyle.quadRow
                {first = model.thirdQuad, second = model.fourthQuad}
            , viewKeyboard
            ]
        )




viewQuadRow : QuadRowStyle msg -> QuadRowView -> Element.Element msg
viewQuadRow style {first, second} =
    Element.row
        style.elementRow
        [ viewQuad style.quad first
        , viewQuad style.quad second
        ]


type alias GuessMiss =
    List MissedWordLetter


type MissedWordLetter
    = MissedWordLetter MissedLetterColor Char


type MissedLetterColor
    = Green
    | Yellow
    | Normal


type GuessResult
    = GuessResultGuessMatch
    | GuessResultGuessMiss GuessMiss


type QuadResult
    = QuadResultMiss (List GuessMiss)
    | QuadResultMatch (List GuessMiss) Word


getColor : MissedLetterColor -> Element.Color
getColor color =
    case color of
        Green ->
            Element.rgb 0.0 1.0 0.0

        Yellow ->
            Element.rgb 0.8 0.8 0.2

        Normal ->
            Element.rgb 0.2157 0.254901 0.3176







viewQuad : QuadStyle msg -> QuadView -> Element.Element msg
viewQuad style {totalRows, currentGuess, guesses, quad} =
    let
        quadResult =
            let
                r = List.foldr
                    (\guess acc ->
                        case acc of
                            QuadResultMatch _ _ ->
                                acc

                            QuadResultMiss misses ->
                                case checkGuess quad.answer guess of
                                    GuessResultGuessMatch ->
                                        QuadResultMatch
                                            misses
                                            guess

                                    GuessResultGuessMiss miss ->
                                        QuadResultMiss (miss :: misses)
                    )
                    (QuadResultMiss [])
                    guesses
            in
            case r of
                QuadResultMatch misses guess ->
                    QuadResultMatch (List.reverse misses) guess
                QuadResultMiss misses ->
                    QuadResultMiss <| List.reverse misses


        quadMatched =
            case quadResult of
                QuadResultMatch _ _ ->
                    True

                QuadResultMiss _ ->
                    False

        quadActive =
            not quadMatched && List.length guesses < totalRows

        numEmptyRows =
            getNumEmptyRows totalRows quadResult

        currentGuessElement =
            if quadActive then
                [ viewCurrentGuess style.currentWord currentGuess ]

              else
                []
    in
    Element.column
        style.elementColumn
        (case quadResult of
            QuadResultMiss misses ->
                    (List.concat
                        [ List.map (viewMissedWord style.missedWord) misses
                        , currentGuessElement
                        , viewEmptyRow style.emptyRow |> List.repeat numEmptyRows
                        ]
                    )
            QuadResultMatch misses answer ->
                    (List.concat
                        [ List.map viewMissedWord misses
                        , [ viewAnswer style.answer answer ]
                        , currentGuessElement
                        , viewEmptyRow style.emptyRow |> List.repeat numEmptyRows
                        ]
                    )
        )


viewMissedWord : GuessMiss -> Element.Element msg
viewMissedWord word =
    Element.row
        styleAttributes.letterRow
        (List.map viewMissedWordLetter word)


viewMissedWordLetter : MissedWordLetter -> Element.Element msg
viewMissedWordLetter (MissedWordLetter color char) =
    Element.row
        [ Background.color (getColor color)
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
            (char |> String.fromChar >> Element.text)
        ]


viewAnswer : Word -> Element.Element msg
viewAnswer word =
    Element.row
        styleAttributes.letterRow
        (List.map viewAnswerLetter word)


viewAnswerLetter : Char -> Element.Element msg
viewAnswerLetter char =
    Element.row
        [ Background.color (Element.rgb 0.0 1.0 0.0)
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
            (char |> String.fromChar >> Element.text)
        ]


checkGuess : Word -> Word -> GuessResult
checkGuess answer guess =
    case answer == guess of
        True ->
            GuessResultGuessMatch

        False ->
            GuessResultGuessMiss (checkMiss answer guess)


checkMiss : Word -> Word -> GuessMiss
checkMiss answer guess =
    let
        greenLetters =
            let
                numLetters =
                    max (List.length answer) (List.length guess)
            in
            List.foldl
                (\x acc ->
                    if b thrupleSecond thrupleThird (==) x then
                        Set.insert (thrupleFirst x) acc

                    else
                        acc
                )
                Set.empty
                (List.map3 Thruple (List.range 0 numLetters) answer guess)

        answerCounter =
            List.map2 Tuple.pair (List.range 0 (List.length answer)) answer
                |> List.filter
                    (Tuple.first >> flip Set.member greenLetters >> not)
                |> List.foldl
                    (c Tuple.second (dictUpdate incrementCount))
                    Dict.empty
        yellowLetters = getYellowLetters answerCounter guess
    in
    List.map
        (\(index, letter) ->
            case Set.member index greenLetters of
                True ->
                    MissedWordLetter Green letter
                False ->
                    case Set.member index yellowLetters of
                        True ->
                            MissedWordLetter Yellow letter
                        False ->
                            MissedWordLetter Normal letter
        )
        (List.map2 Tuple.pair (List.range 0 (List.length guess)) guess)


getNumEmptyRows : Int -> QuadResult -> Int
getNumEmptyRows totalRows quadResult =
    case quadResult of
        QuadResultMiss misses ->
            totalRows - 1 - List.length misses

        QuadResultMatch misses _ ->
            totalRows - List.length misses


viewEmptyRow : EmptyRowStyle msg -> Element.Element msg
viewEmptyRow style =
    Element.row style.elementRow []


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


type alias CurrentGuessStyle msg =
    { elementRow : List (Element.Attribute msg)
    , currentLetter : CurrentGuessLetterStyle msg
    , activeEmptyLetter : ActiveEmptyLetterStyle msg
    , inactiveEmptyLetter : InactiveEmptyLetterStyle msg
    }

type alias CurrentGuessLetterStyle msg =
    { elementRow : List (Element.Attribute msg)
    , elementEl : List (Element.Attribute msg)
    }

viewCurrentGuess : CurrentGuessStyle msg -> String -> Element.Element msg
viewCurrentGuess style currentGuess =
    let
        currentGuessLength =
            String.length currentGuess

        numEmptyLetters =
            if currentGuessLength < 5 then
                5 - currentGuessLength

            else
                0

        guessLetters =
            List.map (viewCurrentGuessLetter style.currentLetter)
                <| String.toList currentGuess

        emptyLetters =
            if numEmptyLetters <= 0 then
                []
            else
                viewActiveEmptyLetter style.activeEmptyLetter
                :: List.repeat
                    (numEmptyLetters - 1)
                    (viewInactiveEmptyLetter style.inactiveEmptyLetter)

    in
    Element.row style.elementRow (guessLetters ++ emptyLetters)


viewCurrentGuessLetter : CurrentGuessLetterStyle msg -> Char -> Element.Element msg
viewCurrentGuessLetter style char =
    Element.row
        style.elementRow
        [ Element.el
            style.elementEl
            (Element.text <| String.fromChar char)
        ]


type alias InactiveEmptyLetterStyle msg =
    { elementEl : List (Element.Attribute msg)
    }


type alias ActiveEmptyLetterStyle msg =
    { elementEl : List (Element.Attribute msg)
    }

viewInactiveEmptyLetter : InactiveEmptyLetterStyle msg -> Element.Element msg
viewInactiveEmptyLetter style =
        Element.el style.elementEl Element.none

viewActiveEmptyLetter : InactiveEmptyLetterStyle msg -> Element.Element msg
viewActiveEmptyLetter style =
        Element.el style.elementEl Element.none


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
                    guess :: model.guesses
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
