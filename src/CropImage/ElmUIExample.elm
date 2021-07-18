module CropImage.ElmUIExample exposing (Model, Msg(..), main, update, view)

import Browser
import Element as Color exposing (Element, behindContent, centerX, centerY, clip, column, el, fill, height, html, htmlAttribute, layout, moveDown, moveRight, none, padding, px, row, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Html exposing (img)
import Html.Attributes exposing (src, style)
import Html.Events
import Html.Events.Extra.Mouse
import Json.Decode as Decode


type alias ImageDimensions =
    { width : Int
    , height : Int
    }


type DragState
    = Initial
    | Dragging ( Float, Float )


type alias Model =
    { dragState : DragState
    , offset : ( Float, Float )
    , previewImageDimensions : Maybe ImageDimensions
    , zoom : Float
    }


type Msg
    = MouseDown ( Float, Float )
    | MouseMove ( Float, Float )
    | MouseUpOrLeave ( Float, Float )
    | PreviewImageLoaded ImageDimensions
    | SetZoom Float


init : () -> ( Model, Cmd Msg )
init _ =
    ( { dragState = Initial
      , offset = ( 0, 0 )
      , previewImageDimensions = Nothing
      , zoom = 100
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        MouseDown ( x, y ) ->
            let
                _ =
                    Debug.log "mouse down" ( x, y )
            in
            ( { model | dragState = Dragging ( x, y ) }, Cmd.none )

        MouseMove ( x, y ) ->
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

        MouseUpOrLeave ( x, y ) ->
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

        PreviewImageLoaded imageDimensions ->
            ( { model | previewImageDimensions = Just imageDimensions }, Cmd.none )

        SetZoom float ->
            ( { model | zoom = float }, Cmd.none )


photoUrl : String
photoUrl =
    "src/CropImage/images/kittens-1280x711.jpg"


decodeImageLoad : (ImageDimensions -> value) -> Decode.Decoder value
decodeImageLoad msg =
    Decode.map msg <|
        Decode.field "target" <|
            Decode.map2 ImageDimensions
                (Decode.field "width" Decode.int)
                (Decode.field "height" Decode.int)


view : Model -> Html.Html Msg
view model =
    let
        ( ox, oy ) =
            model.offset

        zoomFactor : Int
        zoomFactor =
            Basics.round model.zoom

        imageWidth : Int -> String
        imageWidth width =
            String.fromInt ((zoomFactor * width) // 100) ++ "px"

        theImage : Element Msg
        theImage =
            column [ moveDown oy, moveRight ox ]
                (case model.previewImageDimensions of
                    Nothing ->
                        [ html <| img [ Html.Events.on "load" (decodeImageLoad PreviewImageLoaded), src photoUrl ] [] ]

                    Just dimensions ->
                        [ html <| img [ style "max-width" (imageWidth dimensions.width), src photoUrl ] [] ]
                )
    in
    layout [] <|
        column [ width fill, padding 10 ]
            [ column
                [ Border.width 1
                , width (px 300)
                , height (px 300)
                , behindContent theImage
                , htmlAttribute <| Html.Events.Extra.Mouse.onDown (\event -> MouseDown event.clientPos)
                , htmlAttribute <| Html.Events.Extra.Mouse.onMove (\event -> MouseMove event.clientPos)
                , htmlAttribute <| Html.Events.Extra.Mouse.onUp (\event -> MouseUpOrLeave event.clientPos)
                , htmlAttribute <| Html.Events.Extra.Mouse.onLeave (\event -> MouseUpOrLeave event.clientPos)
                , clip
                , centerX
                ]
                []
            , row
                [ width fill
                , Border.width 1
                , width (px 300)
                , centerX
                ]
                [ Input.slider
                    [ height (px 30)
                    , behindContent
                        (el
                            [ width fill
                            , height (px 2)
                            , centerY
                            , Background.color <| Color.rgb255 128 128 128
                            , Border.rounded 2
                            ]
                            none
                        )
                    ]
                    { onChange = SetZoom
                    , label = Input.labelAbove [] (text "Zoom")
                    , min = 0
                    , max = 100
                    , step = Nothing
                    , value = model.zoom
                    , thumb = Input.defaultThumb
                    }
                ]
            ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
