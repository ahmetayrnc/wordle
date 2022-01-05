module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, stopPropagationOn)
import Json.Decode as Decode
import Set
import Task exposing (attempt)



-- MAIN


main : Program () Model Msg
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


alphabet =
    "abcdefghijklmnopqrstuvwxyz"


wordLength : number
wordLength =
    5


numAttempts : number
numAttempts =
    6


word : String
word =
    "tiger"


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


type alias PreviousAttempt =
    { letters : List Char
    }


type alias Model =
    { currentAttempt : Maybe String
    , previousAttempts : List PreviousAttempt
    , endGameModalState : ModalState
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (Just "") ([ "olive", "eerie", "ridge", "girth", "tiger" ] |> List.map String.toList |> List.map PreviousAttempt) Open
    , Cmd.none
    )



-- UPDATE


type Msg
    = LetterInput Char
    | DeleteInput
    | EnterInput
    | ShowEndGameModal
    | HideEndGameModal
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LetterInput letter ->
            case addLetterToCurrentAttempt letter model.currentAttempt of
                Err _ ->
                    ( model, Cmd.none )

                Ok currentAttempt ->
                    ( { model | currentAttempt = Just currentAttempt }, Cmd.none )

        DeleteInput ->
            case deleteLetterFromCurrentAttempt model.currentAttempt of
                Err _ ->
                    ( model, Cmd.none )

                Ok currentAttempt ->
                    ( { model | currentAttempt = Just currentAttempt }, Cmd.none )

        EnterInput ->
            case addNewAttempt model.previousAttempts model.currentAttempt of
                Err _ ->
                    ( model, Cmd.none )

                Ok previousAttempts ->
                    ( { model
                        | previousAttempts = previousAttempts
                        , currentAttempt = resetCurrentAttempt previousAttempts
                      }
                    , Cmd.none
                    )

        ShowEndGameModal ->
            ( { model | endGameModalState = Open }, Cmd.none )

        HideEndGameModal ->
            ( { model | endGameModalState = Closed }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


doesPreviousAttemptHasTheWord : List PreviousAttempt -> Bool
doesPreviousAttemptHasTheWord previousAttempts =
    previousAttempts |> List.map .letters |> List.map String.fromList |> List.member word


determineGameState : List PreviousAttempt -> GameState
determineGameState previousAttempts =
    if List.length previousAttempts <= numAttempts then
        if doesPreviousAttemptHasTheWord previousAttempts then
            Won

        else
            Lost

    else
        OnGoing



-- ACTIONS


addLetterToCurrentAttempt : Char -> Maybe String -> Result String String
addLetterToCurrentAttempt letter currentAttempt =
    case currentAttempt of
        Nothing ->
            Err "Out of Attempts"

        Just attempt ->
            if String.length attempt >= wordLength then
                Err "Out of spaces"

            else if not (String.contains (String.fromChar letter) alphabet) then
                Err "Not in the alphabet"

            else
                Ok (attempt ++ String.fromChar letter)


deleteLetterFromCurrentAttempt : Maybe String -> Result String String
deleteLetterFromCurrentAttempt currentAttempt =
    case currentAttempt of
        Nothing ->
            Err "Out of Attempts"

        Just attempt ->
            Ok (String.dropRight 1 attempt)


addNewAttempt : List PreviousAttempt -> Maybe String -> Result String (List PreviousAttempt)
addNewAttempt previousAttempts currentAttempt =
    case currentAttempt of
        Nothing ->
            Err "Out of Attempts"

        Just attempt ->
            let
                previousAttempt =
                    attempt
                        |> String.toList
                        |> PreviousAttempt
            in
            if String.length attempt > wordLength then
                Err "Current attempt somehow too long "

            else if String.length attempt < wordLength then
                Err "Current attempt too short"

            else
                Ok (List.append previousAttempts [ previousAttempt ])


resetCurrentAttempt : List PreviousAttempt -> Maybe String
resetCurrentAttempt previousAttempts =
    if List.length previousAttempts >= numAttempts then
        Nothing

    else
        Just ""



-- COLORS


decideLetterColor : Int -> Char -> ( Char, LetterColor )
decideLetterColor index letter =
    let
        letterStr =
            letter
                |> String.fromChar
                |> String.toLower

        letterColor =
            if String.slice index (index + 1) word == letterStr then
                Green

            else if String.contains letterStr word then
                Yellow

            else
                Gray
    in
    ( letter, letterColor )


decideAttemptColors : PreviousAttempt -> List ( Char, LetterColor )
decideAttemptColors previousAttempt =
    previousAttempt.letters
        |> List.indexedMap decideLetterColor


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


usedLetters : List PreviousAttempt -> Dict Char LetterColor
usedLetters previousAttempts =
    previousAttempts
        |> List.map decideAttemptColors
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
                    [ viewPreviousAttempts model.previousAttempts
                    , viewCurrentAttempt model.currentAttempt
                    , viewFutureAttempts (List.length model.previousAttempts)
                    ]
                )
            , div [ id "keyboard" ]
                (List.concat
                    [ viewLetterButtons (usedLetters model.previousAttempts)
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


viewPreviousAttempt : PreviousAttempt -> List (Html msg)
viewPreviousAttempt attempt =
    let
        letterColor =
            \index -> \letter -> letterColorToColorString (Tuple.second (decideLetterColor index letter))

        letterText =
            \letter -> text (String.fromChar letter)
    in
    attempt.letters
        |> List.indexedMap (\index -> \letter -> div [ class "letter-box", class (letterColor index letter) ] [ letterText letter ])


viewPreviousAttempts : List PreviousAttempt -> List (Html msg)
viewPreviousAttempts attempts =
    attempts
        |> List.map viewPreviousAttempt
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
