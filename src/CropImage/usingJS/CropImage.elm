port module CropImage.CropImage exposing (main)

import Browser
import Croppie
import Croppie.BindOptions exposing (..)
import Croppie.Options exposing (..)
import Html exposing (div)
import Html.Attributes exposing (height, id, style)


port croppie : Croppie.Data -> Cmd msg


type Msg
    = Bind
    | Result


type alias Model =
    {}


init : () -> ( Model, Cmd Msg )
init _ =
    ( {}
    , croppie <|
        Croppie.bind "demo-basic"
            [ url "images/cat.jpg"
            , points [ 77, 469, 280, 739 ]
            ]
    )


view : Model -> Html.Html Msg
view model =
    div [ style "height" "200px" ]
        [ Croppie.croppie
            [ viewport
                { width = 150
                , height = 200
                , type_ = Square
                }
            ]
            [ id "demo-basic" ]
        ]


update : Msg -> Model -> ( Model, Cmd msg )
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
