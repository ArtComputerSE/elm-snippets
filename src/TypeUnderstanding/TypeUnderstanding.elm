module TypeUnderstanding.TypeUnderstanding exposing (main)

import Browser
import Html exposing (Html, div, text)
import Time
import TypeUnderstanding.SomeType exposing (SomeType(..))


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Msg
    = Tick Time.Posix


type alias Model =
    { hello : SomeType
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    { hello = SomeType "Hello World" }



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( { model | hello = SomeType "Hello" }, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    let
        (SomeType hello) =
            model.hello
    in
    div []
        [ text hello, viewSomeType model.hello ]


viewSomeType : SomeType -> Html Msg
viewSomeType (SomeType str) =
    text str



-- Subscription


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every second Tick


second =
    1000
