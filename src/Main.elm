module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
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
    Sub.none



-- MODEL


wordLength : number
wordLength =
    5


numAttempts : number
numAttempts =
    6


word : String
word =
    "siege"


type LetterColor
    = Normal
    | Green
    | Yellow
    | Gray


type alias ColoredLetter =
    { letter : Char
    , color : LetterColor
    }


type alias Letter =
    { letter : Char
    }


type alias PreviousAttempt =
    { letters : List Letter
    }


type alias Model =
    { currentAttempt : Maybe String
    , previousAttempts : List PreviousAttempt
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (Just "") ([ "plane", "eerie" ] |> List.map String.toList |> List.map (List.map Letter) |> List.map PreviousAttempt)
    , Cmd.none
    )



-- UPDATE


type Msg
    = LetterButtonPressed String
    | DeleteButtonPressed
    | EnterButtonPressed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LetterButtonPressed letter ->
            if validateLetterButtonPress model.currentAttempt then
                ( { model | currentAttempt = addLetterToCurrentAttempt letter model.currentAttempt }, Cmd.none )

            else
                ( model, Cmd.none )

        DeleteButtonPressed ->
            if validateDeleteButtonPress model then
                ( { model | currentAttempt = deleteLetterFromCurrentAttempt model.currentAttempt }, Cmd.none )

            else
                ( model, Cmd.none )

        EnterButtonPressed ->
            if validateAttempt model.currentAttempt then
                ( { model
                    | previousAttempts = addNewAttempt model.previousAttempts model.currentAttempt
                    , currentAttempt = resetCurrentAttempt model.previousAttempts
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )



-- VALIDATION


validateDeleteButtonPress : Model -> Bool
validateDeleteButtonPress model =
    model.currentAttempt /= Nothing


validateLetterButtonPress : Maybe String -> Bool
validateLetterButtonPress currentAttempt =
    case currentAttempt of
        Nothing ->
            False

        Just attempt ->
            if String.length attempt >= wordLength then
                False

            else
                True


validateAttempt : Maybe String -> Bool
validateAttempt currentAttempt =
    case currentAttempt of
        Nothing ->
            False

        Just attempt ->
            if String.length attempt > wordLength then
                False

            else if String.length attempt < wordLength then
                False

            else
                True



-- ACTIONS


addLetterToCurrentAttempt : String -> Maybe String -> Maybe String
addLetterToCurrentAttempt letter currentAttempt =
    currentAttempt
        |> Maybe.map (\str -> str ++ letter)


deleteLetterFromCurrentAttempt : Maybe String -> Maybe String
deleteLetterFromCurrentAttempt currentAttempt =
    currentAttempt
        |> Maybe.map (String.dropRight 1)


addNewAttempt : List PreviousAttempt -> Maybe String -> List PreviousAttempt
addNewAttempt previousAttempts currentAttempt =
    case currentAttempt of
        Nothing ->
            previousAttempts

        Just attempt ->
            let
                previousAttempt =
                    attempt
                        |> String.toList
                        |> List.map (\letter -> Letter letter)
                        |> PreviousAttempt
            in
            List.append previousAttempts [ previousAttempt ]


resetCurrentAttempt : List PreviousAttempt -> Maybe String
resetCurrentAttempt previousAttempts =
    if List.length previousAttempts >= numAttempts - 1 then
        Nothing

    else
        Just ""



-- COLORS


decideLetterColor : Int -> Letter -> ColoredLetter
decideLetterColor index letter =
    let
        letterStr =
            letter
                |> .letter
                |> String.fromChar
                |> String.toLower
    in
    if String.slice index (index + 1) word == letterStr then
        ColoredLetter letter.letter Green

    else if String.contains letterStr word then
        ColoredLetter letter.letter Yellow

    else
        ColoredLetter letter.letter Gray



-- decideAttemptColors : PreviousAttempt -> List ColoredLetter
-- decideAttemptColors previousAttempt =
--     previousAttempt.letters
--         |> List.indexedMap decideLetterColor
-- usedLetters : List PreviousAttempt -> Dict Letter LetterColor
-- usedLetters previousAttempts =
--     previousAttempts
--         |> List.map decideAttemptColors
--         |> List.concat
-- |> List.foldl
-- |> List.concat
-- |> List.map .letter
-- |> Set.fromList
--
-- VIEW


view : Model -> Html Msg
view model =
    div [ id "body" ]
        [ header [ id "header" ]
            [ viewHelpButton
            , div [ id "title" ] [ text "Wordle" ]
            , viewSettingsButton
            ]
        , div [ id "board" ]
            (List.concat
                [ viewPreviousAttempts model.previousAttempts
                , List.singleton (viewCurrentAttempt model.currentAttempt)
                , viewFutureAttempts (List.length model.previousAttempts)
                ]
            )
        , div [ id "keyboard" ]
            (List.concat
                [ viewLetterButtons
                , List.singleton viewDeleteButton
                , List.singleton viewEnterButton
                ]
            )
        ]


viewHelpButton : Html msg
viewHelpButton =
    button [ class "material-icons", class "icon-button" ] [ text "help_outline" ]


viewSettingsButton : Html msg
viewSettingsButton =
    button [ class "material-icons", class "icon-button" ] [ text "settings" ]


viewLetterColor : LetterColor -> String
viewLetterColor letterColor =
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
            \index -> \letter -> viewLetterColor (decideLetterColor index letter).color

        letterText =
            \letter -> text (String.fromChar letter.letter)
    in
    attempt.letters
        |> List.indexedMap (\index -> \letter -> div [ class "letter-box", class (letterColor index letter) ] [ letterText letter ])


viewPreviousAttempts : List PreviousAttempt -> List (Html msg)
viewPreviousAttempts attempts =
    attempts
        |> List.map viewPreviousAttempt
        |> List.map (div [ class "attempt" ])



-- |> List.
-- |> div [ class "previous-attempts" ]


viewFutureAttempt : Html msg
viewFutureAttempt =
    div [ class "attempt" ] (List.repeat wordLength (div [ class "letter-box" ] [ text "" ]))


viewFutureAttempts : Int -> List (Html msg)
viewFutureAttempts numPreviousAttempts =
    List.repeat (numAttempts - numPreviousAttempts - 1) viewFutureAttempt



-- |> div [ id "future-attempts" ]


viewCurrentAttempt : Maybe String -> Html msg
viewCurrentAttempt currentAttempt =
    case currentAttempt of
        Nothing ->
            div [] []

        Just attempt ->
            let
                letterDivs =
                    attempt
                        |> String.padRight wordLength ' '
                        |> String.toList
                        |> List.map String.fromChar
                        |> List.map (\char -> div [ class "letter-box" ] [ text char ])
            in
            div [ class "attempt" ] letterDivs


viewLetterButton : String -> Html Msg
viewLetterButton letter =
    button [ onClick (LetterButtonPressed letter), class "letter-button", class "button", class letter ] [ text letter ]


viewLetterButtons : List (Html Msg)
viewLetterButtons =
    let
        letters =
            "abcdefghijklmnopqrstuvwxyz"
    in
    letters
        |> String.toLower
        |> String.toList
        |> List.map String.fromChar
        |> List.map viewLetterButton


viewDeleteButton : Html Msg
viewDeleteButton =
    button [ class "material-icons-outlined", class "special-button", class "button", class "delete", onClick DeleteButtonPressed ] [ text "backspace" ]


viewEnterButton : Html Msg
viewEnterButton =
    button [ class "special-button", class "button", class "enter", onClick EnterButtonPressed ] [ text "enter" ]
