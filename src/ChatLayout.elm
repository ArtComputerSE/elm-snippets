module ChatLayout exposing (main)

import Browser
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)


view : Model -> Html msg
view model =
    layout [ height fill ] <|
        row [ width <| minimum 600 fill, height fill, Font.size 16 ]
            [ column [ height fill, width <| fillPortion 3, Border.width 1 ]
                [ column [ height <| minimum 0 fill, padding 10, spacingXY 0 20, scrollbarY, Border.width 1, Border.color (rgb255 255 0 0) ] <|
                    List.map mentry <|
                        List.range 1 100
                , el [ alignBottom, padding 20, width fill ] <| text "FOOT"
                ]
            ]


mentry n =
    paragraph [] [ text "sdfn jkhsdf jhadsf kjhads fklsfnbdfmn,asdbfmas fdn fgdlhj f  fghsd fgsjhfdg  fshfg  sfg asjfj hsd jhsf gfbasdfdmn,fbdsa,mnfbjh dksfjh adsjkf ajkshjkashjkas fkjasdhf jkasdf jkadshjk adhf afh" ]


type alias Model =
    String


type Msg
    = NoOp


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( "", Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )
