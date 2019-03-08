module TaskDemo exposing (Model, Msg(..), init, main, resultToMsg, taskCanFail, taskWillNotFail, update, view)

import Browser exposing (Document)
import Html exposing (Html, button, div, p, text)
import Html.Events exposing (onClick)
import Http exposing (Request)
import Task exposing (Task)
import Time


type alias Model =
    String


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( "", Cmd.none )


type Msg
    = Perform
    | PerformSuccess Time.Posix
    | Attempt
    | AttemptSuccess String
    | AttemptFailure String


taskWillNotFail : Task Never Time.Posix
taskWillNotFail =
    Time.now


taskCanFail : Task Http.Error String
taskCanFail =
    Http.toTask <| Http.getString "https://api.github.com/search/repositories?q=elm"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        Perform ->
            ( "Getting the current time", Task.perform PerformSuccess taskWillNotFail )

        PerformSuccess time ->
            ( "Perform: " ++ Debug.toString time, Cmd.none )

        Attempt ->
            ( "Retrieving Elm Github repos", Task.attempt resultToMsg taskCanFail )

        AttemptSuccess results ->
            ( "Attempt: " ++ results, Cmd.none )

        AttemptFailure message ->
            ( "Error: " ++ message, Cmd.none )


resultToMsg : Result e a -> Msg
resultToMsg result =
    result
        |> Result.map Debug.toString
        |> Result.map AttemptSuccess
        |> Result.withDefault (AttemptFailure "Could not retrieve results")


view : Model -> Document Msg
view model =
    div
        []
        [ button [ onClick Perform ] [ text "This won't fail" ]
        , button [ onClick Attempt ] [ text "This might fail" ]
        , p [] [ text model ]
        ]
