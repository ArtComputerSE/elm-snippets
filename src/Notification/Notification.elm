module Notification.Notification exposing (Model, init, main)

import Browser
import Html exposing (a, div, input, label, p, span, text)
import Html.Attributes exposing (href, size, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Encode as Encode
import Notification.Port exposing (notificationClicked, notificationError, permissionChanged, requestPermission, sendNotification)


type alias Model =
    { currentPermission : String
    , title : String
    , body : String
    , icon : String
    , eventText : String
    }


type Msg
    = RequestPermission
    | PermissionChanged String
    | NotificationClicked String
    | NotificationError String
    | SendNotification
    | ChangeTitle String
    | ChangeBody String
    | ChangeIcon String


init : String -> ( Model, Cmd Msg )
init currentPermission =
    ( { currentPermission = currentPermission
      , title = "Title text"
      , body = "No message"
      , icon = "https://artcomputer.se/apple-touch-icon.png"
      , eventText = ""
      }
    , Cmd.none
    )


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

        PermissionChanged string ->
            ( { model | currentPermission = string, eventText = "Permission changed to " ++ string }, Cmd.none )

        NotificationClicked body ->
            ( { model | eventText = "Notification clicked: '" ++ body ++ "'." }, Cmd.none )

        NotificationError body ->
            ( { model | eventText = "Notification error: '" ++ body ++ "'. Check console." }, Cmd.none )

        SendNotification ->
            ( model
            , sendNotification
                (Encode.object
                    [ ( "title", Encode.string model.title )
                    , ( "body", Encode.string model.body )
                    , ( "icon", Encode.string model.icon )
                    ]
                )
            )

        ChangeTitle string ->
            ( { model | title = string }, Cmd.none )

        ChangeBody string ->
            ( { model | body = string }, Cmd.none )

        ChangeIcon string ->
            ( { model | icon = string }, Cmd.none )


view : Model -> Html.Html Msg
view model =
    div []
        [ viewCurrentPermission model
        , p [] [ text "Permission should be requested only in response to a user gesture." ]
        , p [] [ input [ type_ "button", onClick RequestPermission, value "Request permission" ] [] ]
        , div [ style "border-radius" "5px", style "border-style" "solid", style "border-color" "lightgreen", style "padding" "2px", style "width" "fit-content" ]
            [ p [] [ text "Send a notification" ]
            , p [] [ label [] [ text "title: ", input [ type_ "text", onInput ChangeTitle, value model.title ] [] ] ]
            , p [] [ label [] [ text "body: ", input [ type_ "text", onInput ChangeBody, value model.body ] [] ] ]
            , p [] [ label [] [ text "icon: ", input [ type_ "text", onInput ChangeIcon, value model.icon, size 45 ] [] ] ]
            , div [ style "display" "flex", style "justify-content" "right" ]
                [ input [ type_ "button", onClick SendNotification, value "Send" ] [] ]
            ]
        , p [] [ text model.eventText ]
        , div [ style "display" "flex", style "justify-content" "right" ]
            [ a [ href "https://github.com/ArtComputerSE/elm-snippets/tree/master/src/Notification" ]
                [ text "Source code at GitHub" ]
            ]
        ]


viewCurrentPermission : Model -> Html.Html Msg
viewCurrentPermission model =
    div [ style "border" "solid", style "width" "fit-content", style "padding" "2px" ]
        [ text <| "Current permission: "
        , case model.currentPermission of
            "granted" ->
                span [ style "background-color" "lightgreen" ] [ text "granted" ]

            "denied" ->
                span [ style "background-color" "pink" ] [ text "denied" ]

            "default" ->
                span [ style "background-color" "yellow" ] [ text "default" ]

            _ ->
                span [] [ text <| "unknown: " ++ model.currentPermission ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ permissionChanged PermissionChanged
        , notificationClicked NotificationClicked
        , notificationError NotificationError
        ]
