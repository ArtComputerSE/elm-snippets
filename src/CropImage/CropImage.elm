port module CropImage.CropImage exposing (main)

import Browser
import Croppie
import Croppie.BindOptions exposing (..)
import Croppie.Options exposing (..)
import Html
import Html.Attributes exposing (id)


port croppie : Croppie.Data -> Cmd msg


type Msg
    = Bind
    | Result


type alias Model =
    {}


init : () -> ( Model, Cmd Msg )
init _ =
    ( {}, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Croppie.croppie
        [ viewport
            { width = 150
            , height = 200
            , type_ = Square
            }
        ]
        [ id "demo-basic" ]


update msg model =
    case msg of
        Bind ->
            ( model
            , croppie <|
                Croppie.bind "demo-basic"
                    [ url "images/cat.jpg"
                    , points [ 77, 469, 280, 739 ]
                    ]
            )

        Result ->
            ( model
            , croppie <|
                Croppie.result "demo-basic" []
            )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
