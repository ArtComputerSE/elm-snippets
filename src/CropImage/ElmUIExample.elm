module CropImage.ElmUIExample exposing (Model, Msg(..), main, update, view)

import Browser
import Element exposing (behindContent, clip, column, height, image, moveDown, moveRight, px, width)
import Element.Border as Border
import Html
import Html.Events.Extra.Mouse


type alias Model =
    { dragStart : ( Float, Float )
    , offset : ( Float, Float )
    }


type Msg
    = MouseDown
    | DownMsg ( Float, Float )
    | UpMsg ( Float, Float )


init : () -> ( Model, Cmd Msg )
init _ =
    ( { dragStart = ( 0, 0 ), offset = ( 0, 0 ) }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        MouseDown ->
            let
                _ =
                    Debug.log "mouse down"
            in
            ( model, Cmd.none )

        DownMsg ( x, y ) ->
            let
                _ =
                    Debug.log "mouse down" ( x, y )
            in
            ( { model | dragStart = ( x, y ) }, Cmd.none )

        UpMsg ( x, y ) ->
            let
                _ =
                    Debug.log "mouse up" ( x, y )

                _ =
                    Debug.log "moved" ( x - dx, y - dy )

                ( sx, sy ) =
                    model.dragStart

                ( dx, dy ) =
                    ( x - sx, y - sy )

                ( ox, oy ) =
                    model.offset
            in
            ( { model | offset = ( ox + dx, oy + dy ) }, Cmd.none )


photoUrl =
    "src/CropImage/images/kittens-1280x711.jpg"


view : Model -> Html.Html Msg
view model =
    let
        ( ox, oy ) =
            model.offset

        theImage =
            image [ moveDown oy, moveRight ox ] { src = photoUrl, description = "cute" }
    in
    Element.layout [] <|
        column
            [ Border.width 1
            , width (px 300)
            , height (px 300)
            , behindContent theImage
            , Element.htmlAttribute <| Html.Events.Extra.Mouse.onDown (\event -> DownMsg event.clientPos)
            , Element.htmlAttribute <| Html.Events.Extra.Mouse.onUp (\event -> UpMsg event.clientPos)
            , clip
            ]
            []


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
