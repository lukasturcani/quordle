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
      , validWords = Set.fromList decodedFlags.allowed
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
    (List.foldr
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


viewQuad : Int -> String -> List Word -> Quad -> Element.Element msg
viewQuad totalRows currentGuess guesses quad =
    let
        quadResult =
            List.foldl
                (\guess acc ->
                    case acc of
                        QuadResultMatch _ _ ->
                            acc

                        QuadResultMiss misses ->
                            case checkGuess quad.answer guess of
                                GuessResultGuessMatch ->
                                    QuadResultMatch misses guess

                                GuessResultGuessMiss miss ->
                                    QuadResultMiss (miss :: misses)
                )
                (QuadResultMiss [])
                guesses

        quadMatched =
            case quadResult of
                QuadResultMatch _ _ ->
                    True

                QuadResultMiss _ ->
                    False

        quadActive =
            not quadMatched && List.length guesses < totalRows
    in
    Element.column
        styleAttributes.quad
        (List.concat
            [ viewQuadResult quadResult
            , if quadActive then
                [ viewCurrentGuess currentGuess ]

              else
                []
            , viewEmptyRows (getNumEmptyRows totalRows quadResult)
            ]
        )


viewQuadResult : QuadResult -> List (Element.Element msg)
viewQuadResult quadResult =
    case quadResult of
        QuadResultMiss misses ->
            List.map viewMissedWord misses

        QuadResultMatch misses answer ->
            List.map viewMissedWord misses ++ [ viewAnswer answer ]


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
            List.foldr
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
                |> List.foldr
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
