module Main exposing (Model, Msg(..), init, main, update, view)

import Array exposing (Array)
import Browser
import Browser.Events
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, stopPropagationOn)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Set exposing (Set)
import Task exposing (attempt)



-- MAIN


main : Program Encode.Value Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyDown keyDecoder



-- MODEL


alphabet : String
alphabet =
    "abcdefghijklmnopqrstuvwxyz"


numAttempts : number
numAttempts =
    6


type LetterColor
    = Normal
    | Green
    | Yellow
    | Gray


type GameState
    = OnGoing
    | Lost
    | Won


type ModalState
    = Open
    | Closed


type alias Word =
    { l0 : Char
    , l1 : Char
    , l2 : Char
    , l3 : Char
    , l4 : Char
    }


type alias CurrentAttempt =
    { l0 : Maybe Char
    , l1 : Maybe Char
    , l2 : Maybe Char
    , l3 : Maybe Char
    , l4 : Maybe Char
    }


type alias Model =
    { wordList : Set String
    , answer : String
    , currentAttempt : Maybe String
    , previousAttempts : List Word
    , endGameModalState : ModalState
    }


initWordSet : Encode.Value -> Set String
initWordSet flags =
    case Decode.decodeValue flagsDecoder flags of
        Ok answers ->
            Set.fromList (Array.toList answers)

        Err _ ->
            Set.empty


initAnswer : Encode.Value -> String
initAnswer flags =
    case Decode.decodeValue flagsDecoder flags of
        Ok answers ->
            case Array.get 0 answers of
                Just answer ->
                    answer

                Nothing ->
                    "error"

        Err _ ->
            "error"


initPreviousAttempts : List Word
initPreviousAttempts =
    [ Word 'o' 'l' 'i' 'v' 'e'
    , Word 'e' 'e' 'r' 'i' 'e'
    , Word 'r' 'i' 'd' 'g' 'e'
    , Word 'g' 'i' 'r' 't' 'h'
    ]


init : Encode.Value -> ( Model, Cmd Msg )
init flags =
    ( Model
        (initWordSet flags)
        (initAnswer flags)
        (Just "")
        initPreviousAttempts
        (determineEndGameModalState (determineGameState initPreviousAttempts (initAnswer flags)))
    , Cmd.none
    )



-- DECODERS


flagsDecoder : Decoder (Array String)
flagsDecoder =
    Decode.field "wordList" answersDecoder


answersDecoder : Decoder (Array String)
answersDecoder =
    Decode.array Decode.string



-- UPDATE


type Msg
    = LetterInput Char
    | DeleteInput
    | EnterInput
    | HideEndGameModal
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LetterInput letter ->
            case addLetterToCurrentAttempt letter model of
                Err _ ->
                    ( model, Cmd.none )

                Ok currentAttempt ->
                    ( { model | currentAttempt = Just currentAttempt }, Cmd.none )

        DeleteInput ->
            case deleteLetterFromCurrentAttempt model of
                Err _ ->
                    ( model, Cmd.none )

                Ok currentAttempt ->
                    ( { model | currentAttempt = Just currentAttempt }, Cmd.none )

        EnterInput ->
            case addNewAttempt model of
                Err _ ->
                    ( model, Cmd.none )

                Ok previousAttempts ->
                    ( { model
                        | previousAttempts = previousAttempts
                        , currentAttempt = resetCurrentAttempt previousAttempts
                        , endGameModalState = determineEndGameModalState (determineGameState previousAttempts model.answer)
                      }
                    , Cmd.none
                    )

        HideEndGameModal ->
            ( { model | endGameModalState = Closed }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- WORD HELPERS


stringToWord : String -> Result String Word
stringToWord answer =
    case String.toList answer of
        [ l0, l1, l2, l3, l4 ] ->
            Ok (Word l0 l1 l2 l3 l4)

        _ ->
            Err "length != 5"


wordToString : Word -> String
wordToString word =
    String.fromList [ word.l0, word.l1, word.l2, word.l3, word.l4 ]


wordLength : Int
wordLength =
    5



-- ACTIONS


addLetterToCurrentAttempt : Char -> Model -> Result String String
addLetterToCurrentAttempt letter model =
    case determineGameState model.previousAttempts model.answer of
        Lost ->
            Err "Game already lost"

        Won ->
            Err "Game already won"

        OnGoing ->
            case model.currentAttempt of
                Nothing ->
                    Err "Out of Attempts"

                Just attempt ->
                    if String.length attempt >= wordLength then
                        Err "Out of spaces"

                    else if not (String.contains (String.fromChar letter) alphabet) then
                        Err "Not in the alphabet"

                    else
                        Ok (attempt ++ String.fromChar letter)


deleteLetterFromCurrentAttempt : Model -> Result String String
deleteLetterFromCurrentAttempt model =
    case determineGameState model.previousAttempts model.answer of
        Lost ->
            Err "Game already lost"

        Won ->
            Err "Game already won"

        OnGoing ->
            case model.currentAttempt of
                Nothing ->
                    Err "Out of Attempts"

                Just attempt ->
                    Ok (String.dropRight 1 attempt)


addNewAttempt : Model -> Result String (List Word)
addNewAttempt model =
    case determineGameState model.previousAttempts model.answer of
        Lost ->
            Err "Game already lost"

        Won ->
            Err "Game already won"

        OnGoing ->
            case model.currentAttempt of
                Nothing ->
                    Err "Out of Attempts"

                Just attempt ->
                    case stringToWord attempt of
                        Err error ->
                            Err error

                        Ok a ->
                            if not (Set.member model.answer model.wordList) then
                                Err "Isn't in the word list"

                            else
                                Ok (List.append model.previousAttempts [ a ])


resetCurrentAttempt : List Word -> Maybe String
resetCurrentAttempt previousAttempts =
    if List.length previousAttempts >= numAttempts then
        Nothing

    else
        Just ""



-- GAME STATE


doesPreviousAttemptHasTheAnswer : List Word -> String -> Bool
doesPreviousAttemptHasTheAnswer previousAttempts answer =
    previousAttempts |> List.map wordToString |> List.member answer


determineGameState : List Word -> String -> GameState
determineGameState previousAttempts answer =
    if doesPreviousAttemptHasTheAnswer previousAttempts answer then
        Won

    else if List.length previousAttempts == numAttempts then
        Lost

    else
        OnGoing


determineEndGameModalState : GameState -> ModalState
determineEndGameModalState gameState =
    case gameState of
        Won ->
            Open

        Lost ->
            Open

        OnGoing ->
            Closed



-- COLORS


decideLetterColor : Int -> Char -> String -> ( Char, LetterColor )
decideLetterColor index letter answer =
    let
        letterStr =
            letter
                |> String.fromChar
                |> String.toLower

        letterColor =
            if String.slice index (index + 1) answer == letterStr then
                Green

            else if String.contains letterStr answer then
                Yellow

            else
                Gray
    in
    ( letter, letterColor )


decideAttemptColors : String -> Word -> List ( Char, LetterColor )
decideAttemptColors answer previousAttempt =
    previousAttempt
        |> wordToString
        |> String.toList
        |> List.indexedMap (\index -> \letter -> decideLetterColor index letter answer)


letterColorToOrder : LetterColor -> Int
letterColorToOrder letterColor =
    case letterColor of
        Green ->
            4

        Yellow ->
            3

        Gray ->
            2

        Normal ->
            1


usedLetters : List Word -> String -> Dict Char LetterColor
usedLetters previousAttempts answer =
    previousAttempts
        |> List.map (decideAttemptColors answer)
        |> List.concat
        |> List.sortBy (\tuple -> tuple |> Tuple.second |> letterColorToOrder)
        |> Dict.fromList



-- INPUT


type Key
    = Character Char
    | Control String


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map (\str -> str |> toKey |> keyToMessage) (Decode.field "key" Decode.string)


keyToMessage : Key -> Msg
keyToMessage key =
    case key of
        Character char ->
            LetterInput char

        Control str ->
            case str of
                "Backspace" ->
                    DeleteInput

                "Enter" ->
                    EnterInput

                _ ->
                    NoOp


toKey : String -> Key
toKey string =
    case String.uncons string of
        Just ( char, "" ) ->
            Character char

        _ ->
            Control string



-- VIEW


view : Model -> Html Msg
view model =
    div [ id "body" ]
        [ div [ id "content" ]
            [ header [ id "header" ]
                [ viewHelpButton
                , viewTitle
                , viewSettingsButton
                ]
            , div [ id "board" ]
                (List.concat
                    [ viewPreviousAttempts model.previousAttempts model.answer
                    , viewCurrentAttempt model.currentAttempt
                    , viewFutureAttempts (List.length model.previousAttempts)
                    ]
                )
            , div [ id "keyboard" ]
                (List.concat
                    [ viewLetterButtons (usedLetters model.previousAttempts model.answer)
                    , List.singleton viewDeleteButton
                    , List.singleton viewEnterButton
                    ]
                )
            ]
        , viewEndGameModal model.endGameModalState
        ]


viewHelpButton : Html msg
viewHelpButton =
    button [ class "material-icons", class "icon-button" ] [ text "help_outline" ]


viewSettingsButton : Html msg
viewSettingsButton =
    button [ class "material-icons", class "icon-button" ] [ text "settings" ]


viewTitle : Html Msg
viewTitle =
    div [ id "title" ] [ text "Wordle" ]


letterColorToColorString : LetterColor -> String
letterColorToColorString letterColor =
    case letterColor of
        Gray ->
            "gray"

        Normal ->
            "white"

        Green ->
            "green"

        Yellow ->
            "yellow"


viewPreviousAttempt : Word -> String -> List (Html msg)
viewPreviousAttempt attempt answer =
    let
        letterColor =
            \index -> \letter -> letterColorToColorString (Tuple.second (decideLetterColor index letter answer))

        letterText =
            \letter -> text (String.fromChar letter)
    in
    attempt
        |> wordToString
        |> String.toList
        |> List.indexedMap (\index -> \letter -> div [ class "letter-box", class (letterColor index letter) ] [ letterText letter ])


viewPreviousAttempts : List Word -> String -> List (Html msg)
viewPreviousAttempts attempts answer =
    attempts
        |> List.map (\attempt -> viewPreviousAttempt attempt answer)
        |> List.map (div [ class "attempt" ])


viewFutureAttempt : Html msg
viewFutureAttempt =
    div [ class "attempt" ] (List.repeat wordLength (div [ class "letter-box" ] [ text "" ]))


viewFutureAttempts : Int -> List (Html msg)
viewFutureAttempts numPreviousAttempts =
    List.repeat (numAttempts - numPreviousAttempts - 1) viewFutureAttempt


viewCurrentAttempt : Maybe String -> List (Html msg)
viewCurrentAttempt currentAttempt =
    case currentAttempt of
        Nothing ->
            []

        Just attempt ->
            let
                letterDivs =
                    attempt
                        |> String.padRight wordLength ' '
                        |> String.toList
                        |> List.map String.fromChar
                        |> List.map (\char -> div [ class "letter-box" ] [ text char ])
            in
            [ div [ class "attempt" ] letterDivs ]


viewLetterButton : Char -> LetterColor -> Html Msg
viewLetterButton letter letterColor =
    button
        [ onClick (LetterInput letter)
        , class "letter-button"
        , class "button"
        , class (String.fromChar letter)
        , class (letterColorToColorString letterColor)
        ]
        [ text (String.fromChar letter) ]


determineLetterButtonColor : Dict Char LetterColor -> Char -> LetterColor
determineLetterButtonColor letterColors char =
    case Dict.get char letterColors of
        Nothing ->
            Normal

        Just color ->
            color


viewLetterButtons : Dict Char LetterColor -> List (Html Msg)
viewLetterButtons letterColors =
    alphabet
        |> String.toLower
        |> String.toList
        |> List.map (\char -> viewLetterButton char (determineLetterButtonColor letterColors char))


viewDeleteButton : Html Msg
viewDeleteButton =
    button [ class "material-icons-outlined", class "special-button", class "button", class "delete", onClick DeleteInput ] [ text "backspace" ]


viewEnterButton : Html Msg
viewEnterButton =
    button [ class "special-button", class "button", class "enter", onClick EnterInput ] [ text "enter" ]


viewEndGameModal : ModalState -> Html Msg
viewEndGameModal modalState =
    case modalState of
        Open ->
            div
                [ onClick HideEndGameModal
                , class "modal-background"
                ]
                [ div
                    [ stopPropagationOn "click" (Decode.succeed ( NoOp, True ))
                    , class "modal"
                    ]
                    [ text "statistics"
                    , viewStat "played" "3"
                    , viewStat "win %" "67"
                    , viewStat "current streak" "1"
                    , viewStat "max streak" "1"
                    , text "guess distribution"
                    , viewTimer
                    , viewShareButton
                    ]
                ]

        Closed ->
            div [] []


viewStat : String -> String -> Html msg
viewStat title stat =
    div [ class "stat" ] [ text stat, text title ]


viewTimer : Html Msg
viewTimer =
    div [] [ text "next wordle", text "08:09:10" ]


viewShareButton : Html Msg
viewShareButton =
    div [] [ text "share" ]
