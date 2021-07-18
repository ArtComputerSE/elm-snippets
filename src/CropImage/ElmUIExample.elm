module CropImage.ElmUIExample exposing (Model, Msg(..), main, update, view)

import Browser
import Element exposing (behindContent, centerX, clip, column, fill, height, image, moveDown, moveRight, padding, px, width)
import Element.Border as Border
import Html
import Html.Events.Extra.Mouse


type DragState
    = Initial
    | Dragging ( Float, Float )


type alias Model =
    { dragState : DragState
    , offset : ( Float, Float )
    }


type Msg
    = DownMsg ( Float, Float )
    | MoveMsg ( Float, Float )
    | UpMsg ( Float, Float )


init : () -> ( Model, Cmd Msg )
init _ =
    ( { dragState = Initial, offset = ( 0, 0 ) }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        DownMsg ( x, y ) ->
            let
                _ =
                    Debug.log "mouse down" ( x, y )
            in
            ( { model | dragState = Dragging ( x, y ) }, Cmd.none )

        MoveMsg ( x, y ) ->
            case model.dragState of
                Initial ->
                    ( model, Cmd.none )

                Dragging ( sx, sy ) ->
                    let
                        _ =
                            Debug.log "mouse move" ( dx, dy )

                        ( dx, dy ) =
                            ( x - sx, y - sy )

                        ( ox, oy ) =
                            model.offset
                    in
                    ( { model | dragState = Dragging ( x, y ), offset = ( ox + dx, oy + dy ) }, Cmd.none )

        UpMsg ( x, y ) ->
            case model.dragState of
                Initial ->
                    ( model, Cmd.none )

                Dragging ( sx, sy ) ->
                    let
                        _ =
                            Debug.log "mouse up" ( x, y )

                        ( dx, dy ) =
                            ( x - sx, y - sy )

                        ( ox, oy ) =
                            model.offset
                    in
                    ( { model
                        | dragState = Initial
                        , offset = ( Basics.min 0.0 (ox + dx), Basics.min 0.0 (oy + dy) )
                      }
                    , Cmd.none
                    )


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
        column [ width fill, padding 10 ]
            [ column
                [ Border.width 1
                , width (px 300)
                , height (px 300)
                , behindContent theImage
                , Element.htmlAttribute <| Html.Events.Extra.Mouse.onDown (\event -> DownMsg event.clientPos)
                , Element.htmlAttribute <| Html.Events.Extra.Mouse.onMove (\event -> MoveMsg event.clientPos)
                , Element.htmlAttribute <| Html.Events.Extra.Mouse.onUp (\event -> UpMsg event.clientPos)
                , Element.htmlAttribute <| Html.Events.Extra.Mouse.onLeave (\event -> UpMsg event.clientPos)
                , clip
                , centerX
                ]
                []
            ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
