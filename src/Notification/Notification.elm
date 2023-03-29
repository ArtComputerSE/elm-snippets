module Notification.Notification exposing (Model, init, main)

import Browser
import Html exposing (div, input, label, p, text)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Encode as Encode
import Notification.Port exposing (requestPermission, sendNotification)


type alias Model =
    { currentPermission : String
    , body : String
    }


type Msg
    = RequestPermission
    | SendNotification
    | MessageInput String


init : String -> ( Model, Cmd Msg )
init currentPermission =
    ( { currentPermission = currentPermission, body = "No message" }, Cmd.none )


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RequestPermission ->
            ( model, requestPermission () )

        SendNotification ->
            ( model
            , sendNotification
                (Encode.object
                    [ ( "title", Encode.string "Title text" )
                    , ( "body", Encode.string model.body )
                    ]
                )
            )

        MessageInput string ->
            ( { model | body = string }, Cmd.none )


view : Model -> Html.Html Msg
view model =
    div []
        [ p [] [ text <| "Current permission: " ++ model.currentPermission ]
        , p [] [ input [ type_ "button", onClick RequestPermission, value "Request permission" ] [] ]
        , p [] [ label [] [ text "body: ", input [ type_ "text", onInput MessageInput, value model.body ] [] ] ]
        , p [] [ input [ type_ "button", onClick SendNotification, value "Send" ] [] ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
