module Main exposing (Model, Msg(..), init, initialModel, main)

import Browser
import Html exposing (Html, div, text)
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


type alias Model =
    { hello : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    { hello = "Hello World" }



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( { model | hello = "Hello" }, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    div []
        [ text model.hello ]



-- Subscription


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick


second =
    1000
