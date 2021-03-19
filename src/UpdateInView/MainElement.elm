module UpdateInView.MainElement exposing (Model, Msg(..), init, initialModel, main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Time


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = Tick Time.Posix
    | ModelChange Model


type alias Model =
    { tickTock : String
    , counter : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    { tickTock = "Hello World"
    , counter = 42
    }



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( { model
                | tickTock =
                    if model.tickTock == "Tick" then
                        "Tock"

                    else
                        "Tick"
              }
            , Cmd.none
            )

        ModelChange newModel ->
            ( newModel, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    div []
        [ text model.tickTock
        , div []
            [ button [ onClick (ModelChange { model | counter = model.counter - 1 }) ] [ text "-" ]
            , div [] [ text (String.fromInt model.counter) ]
            , button [ onClick (ModelChange { model | counter = model.counter + 1 }) ] [ text "+" ]
            ]
        ]



-- Subscription


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick


second =
    1000
