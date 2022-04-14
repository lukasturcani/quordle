module Main exposing (main)

import Animator
import Browser
import Browser.Events
import Debug
import Dict
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes
import Json.Decode
import List
import Set
import String
import Time



-- MODEL


type alias Model =
    { firstQuadAnswer : Word
    , secondQuadAnswer : Word
    , thirdQuadAnswer : Word
    , fourthQuadAnswer : Word
    , currentGuess : Animator.Timeline String
    , maxGuesses : Int
    , guesses : List Word
    , validWords : Set.Set String
    , hoverButton : Animator.Timeline (Maybe String)
    , windowWidth : Int
    }


animator : Animator.Animator Model
animator =
    Animator.animator
        |> Animator.watching
            .hoverButton
            (\newHoverButton model ->
                { model | hoverButton = newHoverButton }
            )
        |> Animator.watching
            .currentGuess
            (\newCurrentGuess model ->
                { model | currentGuess = newCurrentGuess }
            )


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
    , windowWidth : Int
    }


init : Json.Decode.Value -> ( Model, Cmd Msg )
init flags =
    let
        decoder =
            Json.Decode.map3
                Flags
                (Json.Decode.field
                    "answers"
                    (Json.Decode.list Json.Decode.string)
                )
                (Json.Decode.field
                    "allowed"
                    (Json.Decode.list Json.Decode.string)
                )
                (Json.Decode.field
                    "windowWidth"
                    Json.Decode.int
                )

        decodedFlags =
            case Json.Decode.decodeValue decoder flags of
                Ok decodedWords ->
                    decodedWords

                Err _ ->
                    { allowed = [], answers = [], windowWidth = 10 }
    in
    ( { firstQuadAnswer = String.toList "TEARS"
      , secondQuadAnswer = String.toList "DEARS"
      , thirdQuadAnswer = String.toList "DINER"
      , fourthQuadAnswer = String.toList "SILLY"
      , guesses =
            [ String.toList "TEARS"
            , String.toList "FEARS"
            ]
      , currentGuess = Animator.init "ABC"
      , maxGuesses = 9
      , validWords =
            Set.union
                (Set.fromList decodedFlags.allowed)
                (Set.fromList decodedFlags.answers)
      , hoverButton = Animator.init Nothing
      , windowWidth = decodedFlags.windowWidth
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


flip : (a -> b -> c) -> b -> a -> c
flip f x y =
    f y x


dictUpdate :
    (Maybe v -> Maybe v)
    -> comparable
    -> Dict.Dict comparable v
    -> Dict.Dict comparable v
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
                    Dict.insert
                        char
                        (letterCount - 1)
                        acc.answerCounter
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


rounded =
    Border.rounded 5


styleAttributes =
    { keyboardRow =
        [ Element.width (Element.fillPortion 2)
        , Element.height (Element.fillPortion 2)
        , Element.spacing 7
        ]
    , letterRow =
        []
    }


type alias ModelStyle msg =
    { elementLayout : List (Element.Attribute msg)
    , elementColumn : List (Element.Attribute msg)
    , elementRow : List (Element.Attribute msg)
    , quad : QuadStyle msg
    }


type alias QuadStyle msg =
    { elementColumn : List (Element.Attribute msg)
    , emptyRow : EmptyRowStyle msg
    , currentWord : CurrentGuessStyle msg
    , missedWord : MissedWordStyle msg
    , answer : AnswerStyle msg
    }


type alias EmptyRowStyle msg =
    { elementRow : List (Element.Attribute msg)
    }


modelStyle : ModelStyle msg
modelStyle =
    { elementLayout =
        [ Background.color (Element.rgb 0.12157 0.16078 0.2157)
        ]
    , elementColumn =
        [ Background.color (Element.rgb 0.12157 0.16078 0.2157)
        , Element.centerX
        , Element.width <| Element.maximum 600 Element.fill
        , Element.height Element.fill
        , Element.spacing 7
        ]
    , elementRow =
        [ Element.width (Element.fillPortion 4)
        , Element.height (Element.fillPortion 4)
        , Element.spacing 7
        ]
    , quad =
        { elementColumn =
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.spacing 4
            ]
        , emptyRow =
            { elementRow =
                [ Background.color
                    (Element.rgb 0.066666 0.094117 0.15294)
                , Element.width Element.fill
                , Element.height Element.fill
                , Element.spacing 4
                , rounded
                ]
            }
        , currentWord =
            { elementRow =
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.spacing 4
                ]
            , currentLetter =
                { elementRow =
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , rounded
                    , Background.color currentGuessLetterColor
                    ]
                , elementEl =
                    [ Element.centerX
                    , Element.centerY
                    , rounded
                    ]
                }
            , activeEmptyLetter =
                { elementEl =
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , Element.spacing 4
                    , rounded
                    , Background.color
                        (Element.rgb 0.13725 0.3137 0.38823)
                    ]
                }
            , inactiveEmptyLetter =
                { elementEl =
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , Element.spacing 4
                    , rounded
                    , Background.color currentGuessLetterColor
                    ]
                }
            }
        , answer =
            { elementRow =
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.spacing 4
                ]
            , answerLetter =
                { elementRow =
                    [ Background.color (Element.rgb 0.0 0.8 0.53333)
                    , Element.width Element.fill
                    , Element.height Element.fill
                    , rounded
                    ]
                , elementEl =
                    [ Font.color (Element.rgb 0.0 0.0 0.0)
                    , Element.centerX
                    , Element.centerY
                    , rounded
                    ]
                }
            }
        , missedWord =
            { elementRow =
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.spacing 4
                ]
            , missedWordLetter =
                { elementRow =
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , rounded
                    ]
                , elementEl =
                    [ Element.centerX
                    , Element.centerY
                    , rounded
                    ]
                }
            }
        }
    }


view : Model -> Html.Html Msg
view model =
    let
        firstQuadResult =
            getQuadResult model.firstQuadAnswer model.guesses

        secondQuadResult =
            getQuadResult model.secondQuadAnswer model.guesses

        thirdQuadResult =
            getQuadResult model.thirdQuadAnswer model.guesses

        fourthQuadResult =
            getQuadResult model.fourthQuadAnswer model.guesses

        summary =
            { first =
                getQuadSummary firstQuadResult
            , second =
                getQuadSummary secondQuadResult
            , third =
                getQuadSummary thirdQuadResult
            , fourth =
                getQuadSummary fourthQuadResult
            }
    in
    Element.layout
        modelStyle.elementLayout
        (Element.column
            modelStyle.elementColumn
            [ Element.row
                modelStyle.elementRow
                [ viewQuad
                    modelStyle.quad
                    model.validWords
                    model.firstQuadAnswer
                    model.maxGuesses
                    model.guesses
                    model.currentGuess

                -- If viewQuad is lazy, the fact that model.guesses and
                -- model.currentGuess are passed it means that it won't
                -- behave lazily because the values passed to the
                -- function will change. This can be fixed by keeping
                -- track of if the quad has finished already.
                , viewQuad
                    modelStyle.quad
                    model.validWords
                    model.secondQuadAnswer
                    model.maxGuesses
                    model.guesses
                    model.currentGuess
                ]
            , Element.row
                modelStyle.elementRow
                [ viewQuad
                    modelStyle.quad
                    model.validWords
                    model.thirdQuadAnswer
                    model.maxGuesses
                    model.guesses
                    model.currentGuess
                , viewQuad
                    modelStyle.quad
                    model.validWords
                    model.fourthQuadAnswer
                    model.maxGuesses
                    model.guesses
                    model.currentGuess
                ]
            , viewKeyboard summary model.hoverButton
            ]
        )


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


getQuadSummaryFromMiss : GuessMiss -> QuadSummary
getQuadSummaryFromMiss miss =
    List.foldr
        (\guess acc ->
            case guess of
                MissedWordLetter Green char ->
                    { acc
                        | greenLetters = Set.insert char acc.greenLetters
                    }
                MissedWordLetter Yellow char ->
                    { acc
                        | yellowLetters = Set.insert char acc.yellowLetters
                    }
                _ ->
                    acc
        )
        { greenLetters = Set.empty
        , yellowLetters = Set.empty
        }
        miss


getQuadSummary : QuadResult -> QuadSummary
getQuadSummary quadResult =
    let
        guesses =
            case quadResult of
                QuadResultMiss guesses_ ->
                    guesses_

                QuadResultMatch guesses_ _ ->
                    guesses_
    in
    List.foldr
        (\guess acc ->
            let
                summary = getQuadSummaryFromMiss guess
            in
                { acc
                    | greenLetters =
                        Set.union acc.greenLetters summary.greenLetters

                    , yellowLetters =
                        Set.union
                            acc.yellowLetters
                            summary.yellowLetters

                }
        )
        { greenLetters = Set.empty
        , yellowLetters = Set.empty
        }
        guesses


getBackgroundColor : MissedLetterColor -> Element.Color
getBackgroundColor color =
    case color of
        Green ->
            Element.rgb 0.0 0.8 0.53333

        Yellow ->
            Element.rgb 1.0 0.8 0.0

        Normal ->
            Element.rgb 0.2157 0.254901 0.3176


getFontColor : MissedLetterColor -> Element.Color
getFontColor color =
    case color of
        Green ->
            Element.rgb 0.0 0.0 0.0

        Yellow ->
            Element.rgb 0.0 0.0 0.0

        Normal ->
            Element.rgb 1.0 1.0 1.0


getQuadResult : Word -> List Word -> QuadResult
getQuadResult quadAnswer guesses =
    let
        r =
            List.foldr
                (\guess acc ->
                    case acc of
                        QuadResultMatch _ _ ->
                            acc

                        QuadResultMiss misses ->
                            case checkGuess quadAnswer guess of
                                GuessResultGuessMatch ->
                                    QuadResultMatch
                                        misses
                                        guess

                                GuessResultGuessMiss miss ->
                                    QuadResultMiss
                                        (miss :: misses)
                )
                (QuadResultMiss [])
                guesses
    in
    case r of
        QuadResultMatch misses guess ->
            QuadResultMatch (List.reverse misses) guess

        QuadResultMiss misses ->
            QuadResultMiss <| List.reverse misses


viewQuad :
    QuadStyle msg
    -> Set.Set String
    -> Word
    -> Int
    -> List Word
    -> Animator.Timeline String
    -> Element.Element msg
viewQuad style validWords quadAnswer totalRows guesses currentGuess =
    let
        quadResult =
            let
                r =
                    List.foldr
                        (\guess acc ->
                            case acc of
                                QuadResultMatch _ _ ->
                                    acc

                                QuadResultMiss misses ->
                                    case checkGuess quadAnswer guess of
                                        GuessResultGuessMatch ->
                                            QuadResultMatch
                                                misses
                                                guess

                                        GuessResultGuessMiss miss ->
                                            QuadResultMiss
                                                (miss :: misses)
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
                [ viewCurrentGuess
                    style.currentWord
                    validWords
                    currentGuess
                ]

            else
                []
    in
    Element.column
        style.elementColumn
        (case quadResult of
            QuadResultMiss misses ->
                List.concat
                    [ List.map (viewMissedWord style.missedWord) misses
                    , currentGuessElement
                    , viewEmptyRow style.emptyRow
                        |> List.repeat numEmptyRows
                    ]

            QuadResultMatch misses answer ->
                List.concat
                    [ List.map (viewMissedWord style.missedWord) misses
                    , [ viewAnswer style.answer answer ]
                    , currentGuessElement
                    , viewEmptyRow style.emptyRow
                        |> List.repeat (numEmptyRows - 1)
                    ]
        )


viewMissedWord :
    MissedWordStyle msg
    -> GuessMiss
    -> Element.Element msg
viewMissedWord style word =
    Element.row
        style.elementRow
        (List.map (viewMissedWordLetter style.missedWordLetter) word)


type alias MissedWordStyle msg =
    { elementRow : List (Element.Attribute msg)
    , missedWordLetter : MissedWordLetterStyle msg
    }


type alias MissedWordLetterStyle msg =
    { elementRow : List (Element.Attribute msg)
    , elementEl : List (Element.Attribute msg)
    }


viewMissedWordLetter :
    MissedWordLetterStyle msg
    -> MissedWordLetter
    -> Element.Element msg
viewMissedWordLetter style (MissedWordLetter color char) =
    Element.row
        (Background.color (getBackgroundColor color)
            :: Font.color (getFontColor color)
            :: style.elementRow
        )
        [ Element.el
            style.elementEl
            (char |> String.fromChar |> Element.text)
        ]


viewAnswer : AnswerStyle msg -> Word -> Element.Element msg
viewAnswer style word =
    Element.row
        style.elementRow
        (List.map (viewAnswerLetter style.answerLetter) word)


type alias AnswerStyle msg =
    { elementRow : List (Element.Attribute msg)
    , answerLetter : AnswerLetterStyle msg
    }


type alias AnswerLetterStyle msg =
    { elementRow : List (Element.Attribute msg)
    , elementEl : List (Element.Attribute msg)
    }


viewAnswerLetter : AnswerLetterStyle msg -> Char -> Element.Element msg
viewAnswerLetter style char =
    Element.row
        style.elementRow
        [ Element.el
            style.elementEl
            (char |> String.fromChar >> Element.text)
        ]


checkGuess : Word -> Word -> GuessResult
checkGuess answer guess =
    if answer == guess then
        GuessResultGuessMatch

    else
        GuessResultGuessMiss (checkMiss answer guess)


checkMiss : Word -> Word -> GuessMiss
checkMiss answer guess =
    let
        numLetters =
            max (List.length answer) (List.length guess)

        indices =
            List.range 0 numLetters

        greenLetters =
            List.foldl
                (\x acc ->
                    let
                        letterIndex =
                            thrupleFirst x

                        answerLetter =
                            thrupleSecond x

                        guessLetter =
                            thrupleThird x
                    in
                    if answerLetter == guessLetter then
                        Set.insert letterIndex acc

                    else
                        acc
                )
                Set.empty
                (List.map3 Thruple indices answer guess)

        answerCounter =
            List.map2 Tuple.pair indices answer
                |> List.filter
                    (Tuple.first
                        >> flip Set.member greenLetters
                        >> not
                    )
                |> List.foldl
                    (Tuple.second >> dictUpdate incrementCount)
                    Dict.empty

        yellowLetters =
            getYellowLetters answerCounter guess
    in
    List.map
        (\( index, letter ) ->
            if Set.member index greenLetters then
                MissedWordLetter Green letter

            else if Set.member index yellowLetters then
                MissedWordLetter Yellow letter

            else
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


viewCurrentGuess :
    CurrentGuessStyle msg
    -> Set.Set String
    -> Animator.Timeline String
    -> Element.Element msg
viewCurrentGuess style validWords currentGuess =
    let
        currentGuessLength =
            String.length currentGuessValue

        numEmptyLetters =
            if currentGuessLength < 5 then
                5 - currentGuessLength

            else
                0

        ( initialLetters_, lastLetter_ ) =
            let
                lastIndex =
                    currentGuessLength - 1

                indexedLetters =
                    List.map2
                        Tuple.pair
                        (List.range 0 lastIndex)
                        (String.toList currentGuessValue)

                ( indexedInitialLetters, indexedLastLetter ) =
                    List.partition
                        (\( index, _ ) -> index < lastIndex)
                        indexedLetters
            in
            ( List.map Tuple.second indexedInitialLetters
            , List.map Tuple.second indexedLastLetter
            )

        initialLetters =
            List.map
                (viewCurrentGuessLetter style.currentLetter)
                initialLetters_

        lastLetter =
            let
                newCurrentLetterStyle =
                    { elementEl = style.currentLetter.elementEl
                    , elementRow =
                        Element.htmlAttribute
                            (Html.Attributes.style "z-index" "11")
                            :: Element.scale
                                (Animator.move
                                    currentGuess
                                    (\_ ->
                                        Animator.once
                                            Animator.slowly
                                            (Animator.zigzag 1 1.3)
                                    )
                                )
                            :: style.currentLetter.elementRow
                    }
            in
            List.map
                (viewCurrentGuessLetter newCurrentLetterStyle)
                lastLetter_

        emptyLetters =
            if numEmptyLetters <= 0 then
                []

            else
                viewActiveEmptyLetter style.activeEmptyLetter
                    :: List.repeat
                        (numEmptyLetters - 1)
                        (viewInactiveEmptyLetter
                            style.inactiveEmptyLetter
                        )

        currentGuessValue =
            Animator.current currentGuess

        fontColor =
            let
                incomplete =
                    String.length currentGuessValue < 5
            in
            if incomplete || Set.member currentGuessValue validWords then
                Font.color (Element.rgb 1.0 1.0 1.0)

            else
                Font.color (Element.rgb 1.0 0.114 0.282)
    in
    Element.row
        (fontColor :: style.elementRow)
        (List.concat
            [ initialLetters
            , lastLetter
            , emptyLetters
            ]
        )


viewCurrentGuessLetter :
    CurrentGuessLetterStyle msg
    -> Char
    -> Element.Element msg
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


viewInactiveEmptyLetter :
    InactiveEmptyLetterStyle msg
    -> Element.Element msg
viewInactiveEmptyLetter style =
    Element.el style.elementEl Element.none


viewActiveEmptyLetter :
    InactiveEmptyLetterStyle msg
    -> Element.Element msg
viewActiveEmptyLetter style =
    Element.el style.elementEl Element.none


keyStyle =
    [ Background.color (Element.rgb 0.4196 0.447 0.50196)
    , Element.width Element.fill
    , Element.height Element.fill
    , Font.center
    , Font.color (Element.rgb 1.0 1.0 1.0)
    , rounded
    , Events.onMouseLeave UnhoverButton
    , Element.pointer
    ]


isHoverKey : Maybe String -> String -> Bool
isHoverKey true test =
    case true of
        Just hoverKey_ ->
            hoverKey_ == test

        _ ->
            False


type alias QuadSummary =
    { greenLetters : Set.Set Char
    , yellowLetters : Set.Set Char
    }


type alias KeyboardSummary =
    { first : QuadSummary
    , second : QuadSummary
    , third : QuadSummary
    , fourth : QuadSummary
    }


getQuadColor : QuadSummary -> Char -> MissedLetterColor
getQuadColor summary char =
    if Set.member char summary.greenLetters then
        Green

    else if Set.member char summary.yellowLetters then
        Yellow

    else
        Normal


viewKey :
    KeyboardSummary
    -> Animator.Timeline (Maybe String)
    -> Char
    -> Element.Element Msg
viewKey summary hoverKey char =
    button summary hoverKey char



{--
    let
        keyStyle_ =
            Events.onMouseEnter
                (HoverButton <| String.fromChar char)
                :: keyStyle
    in
    Input.button
        (Element.moveUp
            (Animator.linear
                hoverKey
                (\state ->
                    if isHoverKey state (String.fromChar char) then
                        Animator.at 4

                    else
                        Animator.at 0
                )
            )
            :: keyStyle_
        )
        { onPress = Just (PressKey char)
        , label = char |> String.fromChar >> Element.text
        }
--}


button :
    KeyboardSummary
    -> Animator.Timeline (Maybe String)
    -> Char
    -> Element.Element Msg
button summary hoverKey char =
    let
        keyStyle_ =
            Events.onMouseEnter
                (HoverButton <| String.fromChar char)
                :: keyStyle
    in
    Element.row
        (Element.moveUp
            (Animator.linear
                hoverKey
                (\state ->
                    if isHoverKey state (String.fromChar char) then
                        Animator.at 4

                    else
                        Animator.at 0
                )
            )
            :: keyStyle_
        )
        [ Element.el
            [ buttonBackground summary char |> Element.behindContent
            , Element.width Element.fill
            , Element.height Element.fill
            ]
            (char |> String.fromChar |> Element.text)
        ]

buttonBackground : KeyboardSummary -> Char -> Element.Element Msg
buttonBackground summary char =
    let
        firstQuadColor =
            char
                |> getQuadColor summary.first
                |> getBackgroundColor
                |> Background.color

        secondQuadColor =
            char
                |> getQuadColor summary.second
                |> getBackgroundColor
                |> Background.color

        thirdQuadColor =
            char
                |> getQuadColor summary.third
                |> getBackgroundColor
                |> Background.color

        fourthQuadColor =
            char
                |> getQuadColor summary.fourth
                |> getBackgroundColor
                |> Background.color

        elementStyle =
            [ Element.width Element.fill
            , Element.height Element.fill
            ]

        rowStyle =
            [ Element.width Element.fill
            , Element.height Element.fill
            ]

        firstElementStyle =
            firstQuadColor :: elementStyle

        secondElementStyle =
            secondQuadColor :: elementStyle

        thirdElementStyle =
            thirdQuadColor :: elementStyle

        fourthElementStyle =
            fourthQuadColor :: elementStyle
    in
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        [ Element.row
            rowStyle
            [ Element.el firstElementStyle Element.none
            , Element.el secondElementStyle Element.none
            ]
        , Element.row
            rowStyle
            [ Element.el thirdElementStyle Element.none
            , Element.el fourthElementStyle Element.none
            ]
        ]


viewKeyboard :
    KeyboardSummary
    -> Animator.Timeline (Maybe String)
    -> Element.Element Msg
viewKeyboard summary hoverKey =
    Element.column
        styleAttributes.keyboardRow
        [ Element.row
            [ Element.height Element.fill
            , Element.width Element.fill
            , Element.spacing 7
            ]
            ("QWERTYUIOP"
                |> String.toList
                |> viewKeyboardRow summary hoverKey
            )
        , Element.row
            [ Element.height Element.fill
            , Element.width Element.fill
            , Element.spacing 7
            ]
            ("ASDFGHJKL"
                |> String.toList
                |> viewKeyboardRow summary hoverKey
            )
        , Element.row
            [ Element.height Element.fill
            , Element.width Element.fill
            , Element.spacing 7
            ]
            ("ZXCVBNM"
                |> String.toList
                |> viewKeyboardBottomRow summary hoverKey
            )
        ]


viewKeyboardRow :
    KeyboardSummary
    -> Animator.Timeline (Maybe String)
    -> List Char
    -> List (Element.Element Msg)
viewKeyboardRow summary hoverKey keys =
    List.map
        (viewKey summary hoverKey)
        keys


viewKeyboardBottomRow :
    KeyboardSummary
    -> Animator.Timeline (Maybe String)
    -> List Char
    -> List (Element.Element Msg)
viewKeyboardBottomRow summary hoverKey letters =
    let
        backspaceKeyStyle =
            Events.onMouseEnter (HoverButton "BKSPC")
                :: Element.moveUp
                    (Animator.linear
                        hoverKey
                        (\state ->
                            if isHoverKey state "BKSPC" then
                                Animator.at 4

                            else
                                Animator.at 0
                        )
                    )
                :: keyStyle

        enterKeyStyle =
            Events.onMouseEnter (HoverButton "ENTER")
                :: Element.moveUp
                    (Animator.linear
                        hoverKey
                        (\state ->
                            if isHoverKey state "ENTER" then
                                Animator.at 4

                            else
                                Animator.at 0
                        )
                    )
                :: keyStyle
    in
    Input.button
        backspaceKeyStyle
        { onPress = Just PressBackspace
        , label = Element.text "BKSPC"
        }
        :: viewKeyboardRow summary hoverKey letters
        ++ [ Input.button
                enterKeyStyle
                { onPress = Just PressEnter
                , label = Element.text "ENTER"
                }
           ]



-- UPDATE


type Msg
    = PressKey Char
    | PressBackspace
    | PressEnter
    | HoverButton String
    | UnhoverButton
    | RuntimeTriggeredAnimationStep Time.Posix
    | WindowResize Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PressKey char ->
            if String.length (Animator.current model.currentGuess) < 5 then
                ( { model
                    | currentGuess =
                        Animator.go
                            Animator.immediately
                            (String.append
                                (Animator.current model.currentGuess)
                                (String.fromChar char)
                            )
                            model.currentGuess
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        PressBackspace ->
            if String.length (Animator.current model.currentGuess) == 0 then
                ( model, Cmd.none )

            else
                ( { model
                    | currentGuess =
                        Animator.go
                            Animator.immediately
                            (String.dropRight
                                1
                                (Animator.current model.currentGuess)
                            )
                            model.currentGuess
                  }
                , Cmd.none
                )

        PressEnter ->
            if isValidGuess model.validWords (Animator.current model.currentGuess) then
                ( commitGuess model
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        HoverButton id ->
            ( { model
                | hoverButton =
                    Animator.go
                        Animator.quickly
                        (Just id)
                        model.hoverButton
              }
            , Cmd.none
            )

        UnhoverButton ->
            ( { model
                | hoverButton =
                    Animator.go
                        Animator.quickly
                        Nothing
                        model.hoverButton
              }
            , Cmd.none
            )

        RuntimeTriggeredAnimationStep newTime ->
            ( Animator.update newTime animator model
            , Cmd.none
            )

        WindowResize width height ->
            ( { model | windowWidth = width }
            , Cmd.none
            )


isValidGuess : Set.Set String -> String -> Bool
isValidGuess allowedWords guess =
    String.length guess >= 5 && Set.member guess allowedWords


commitGuess : Model -> Model
commitGuess model =
    case currentWordToWord (Animator.current model.currentGuess) of
        Just guess ->
            { model
                | currentGuess =
                    Animator.go
                        Animator.immediately
                        ""
                        model.currentGuess
                , guesses =
                    guess :: model.guesses
                , validWords =
                    Set.remove
                        (Animator.current model.currentGuess)
                        model.validWords
            }

        Nothing ->
            model



-- SUBSCRIPTIONS


subscriptions model =
    Sub.batch
        [ Animator.toSubscription
            RuntimeTriggeredAnimationStep
            model
            animator
        , Browser.Events.onResize WindowResize
        ]



-- MAIN


main : Program Json.Decode.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
