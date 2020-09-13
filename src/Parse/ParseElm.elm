module Parse.ParseElm exposing (main, parse, src)

import Browser
import Elm.Parser
import Html exposing (Html, div, text)


type alias Model =
    String


type Msg
    = NoOp


src =
    """module Foo exposing(foo)

foo = 1
"""


parse : String -> String
parse input =
    case Elm.Parser.parse input of
        Err e ->
            "Failed: " ++ Debug.toString e

        Ok v ->
            "Success: " ++ Debug.toString v


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model


view : Model -> Html Msg
view model =
    div []
        [ text model ]


main =
    Browser.sandbox
        { init = parse src
        , update = update
        , view = view
        }
