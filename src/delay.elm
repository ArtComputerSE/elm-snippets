module Main exposing (main)

import Browser exposing (Document)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Process
import Task
import Time


type alias Model =
    { message : String }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { message = "nothing yet" }, Cmd.none )


main =
    Browser.document
        { view = view
        , init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }


view : Model -> Document Msg
view model =
    div [] [ button [ onClick Login ] [ text "click me!" ], text model.message ]


type Msg
    = Login
    | ShowDashboard Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Login ->
            ( { model | message = "temporary message" }
            , Task.perform ShowDashboard (Process.sleep 2000 |> Task.andThen (\_ -> Time.now))
            )

        ShowDashboard _ ->
            ( { model | message = "" }
            , Cmd.none
            )
