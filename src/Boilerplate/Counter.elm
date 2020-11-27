module Counter exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


main =
    Browser.sandbox
        { init = 0
        , update = update
        , view = view
        }


type alias Model =
    Int


type Msg
    = Increment
    | Decrement


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (String.fromInt model) ]
        , button [ onClick Increment ] [ text "+" ]
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1
