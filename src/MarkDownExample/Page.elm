module MarkDownExample.Page exposing (Model, init, main)

import Browser
import Element exposing (Element, column, el, fill, height, layout, row, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import MarkDownExample.Markdowner as Mardowner


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { content : String
    }


type Msg
    = ContentChanged String


init : () -> ( Model, Cmd Msg )
init _ =
    ( { content = "" }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ContentChanged newContent ->
            ( { model | content = newContent }, Cmd.none )


view : Model -> Html.Html Msg
view model =
    layout [ height fill, Font.size 24 ] <| viewContent model


viewContent : Model -> Element Msg
viewContent model =
    row [ width fill ]
        [ viewInput model
        , viewRendered model
        ]


viewInput : Model -> Element Msg
viewInput model =
    column [ width fill, Border.width 1 ]
        [ Input.multiline []
            { onChange = ContentChanged
            , text = model.content
            , placeholder = Nothing
            , label = Input.labelAbove [] <| el [] <| text "Enter here"
            , spellcheck = False
            }
        ]


viewRendered : Model -> Element Msg
viewRendered model =
    case Mardowner.render model.content of
        Ok elements ->
            column [ width fill ] elements

        Err error ->
            text error
