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
    , originalDimensions : Maybe ImageDimensions
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
      , originalDimensions = Nothing
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
                        ( dx, dy ) =
                            ( x - sx, y - sy )

                        ( ox, oy ) =
                            model.offset
                    in
                    ( { model | dragState = Dragging ( x, y ), offset = ( ox + dx, oy + dy ) }, Cmd.none )

        MouseUpOrLeave moveEnd ->
            case model.dragState of
                Initial ->
                    ( model, Cmd.none )

                Dragging moveStart ->
                    ( { model
                        | dragState = Initial
                        , offset = clampOffset moveStart moveEnd model.offset
                      }
                    , Cmd.none
                    )

        PreviewImageLoaded imageDimensions ->
            ( { model | originalDimensions = Just imageDimensions }, Cmd.none )

        SetZoom float ->
            ( { model | zoom = float }, Cmd.none )


photoUrl =
    "src/CropImage/images/kittens-1280x711.jpg"


sideLength =
    300


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

        imageWidthStr : Int -> Float -> String
        imageWidthStr originalWidth zoom =
            String.fromInt (imageWidth originalWidth zoom) ++ "px"

        theImage : Element Msg
        theImage =
            column [ moveDown oy, moveRight ox ]
                (case model.originalDimensions of
                    Nothing ->
                        [ html <| img [ Html.Events.on "load" (decodeImageLoad PreviewImageLoaded), src photoUrl ] [] ]

                    Just dimensions ->
                        [ html <| img [ style "max-width" (imageWidthStr dimensions.width model.zoom), src photoUrl ] [] ]
                )
    in
    layout [] <|
        column [ width fill, padding 10 ]
            [ column
                [ Border.width 1
                , width (px sideLength)
                , height (px sideLength)
                , behindContent theImage
                , htmlAttribute <| Html.Events.Extra.Mouse.onDown (\event -> MouseDown event.clientPos)
                , htmlAttribute <| Html.Events.Extra.Mouse.onMove (\event -> MouseMove event.clientPos)
                , htmlAttribute <| Html.Events.Extra.Mouse.onUp (\event -> MouseUpOrLeave event.clientPos)
                , htmlAttribute <| Html.Events.Extra.Mouse.onLeave (\event -> MouseUpOrLeave event.clientPos)
                , clip
                , centerX
                ]
                []
            , zoomSlider model.zoom
            ]


zoomSlider : Float -> Element Msg
zoomSlider zoomValue =
    row
        [ width fill
        , Border.width 1
        , width (px sideLength)
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
            , value = zoomValue
            , thumb = Input.defaultThumb
            }
        ]


imageWidth : Int -> Float -> Int
imageWidth originalWidth zoom =
    Basics.max sideLength ((Basics.round zoom * originalWidth) // 100)


clampOffset : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
clampOffset moveStart moveEnd currentOffset =
    let
        ( ox, oy ) =
            currentOffset

        ( sx, sy ) =
            moveStart

        ( ex, ey ) =
            moveEnd

        ( dx, dy ) =
            ( ex - sx, ey - sy )

        ( newX, newY ) =
            ( ox + dx, oy + dy )
    in
    ( Basics.min 0.0 newX, Basics.min 0.0 newY )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
