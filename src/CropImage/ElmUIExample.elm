module CropImage.ElmUIExample exposing (Model, Msg(..), clampOffset, main, update, view)

import Browser
import Element as Color exposing (Element, behindContent, centerX, centerY, clip, column, el, fill, height, html, htmlAttribute, layout, moveDown, moveRight, none, padding, px, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Html exposing (img)
import Html.Attributes exposing (src, style)
import Html.Events
import Html.Events.Extra.Mouse
import Json.Decode as Decode



{-
   Display a random image of size 2000 x 3000 in a 300 x 300 window.
   Let the user pan the image by dragging and zooming with the slider.

   Do not use any ports.
   Use elm-ui for display and elm-pointer-events for mouse and touch events.
   Clamp the offset so that the image is within limits.

   Solution: put the image in a behindContent of a column with fixed width and height.
   The column catches mouse and touch events and offset is calculated from that.

-}


type alias ImageDimensions =
    { width : Int
    , height : Int
    }


type DragState
    = Initial
    | Dragging ( Float, Float )


type alias Model =
    { dragState : DragState
    , originalDimensions : Maybe ImageDimensions
    , offset : ( Float, Float )
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
      , zoom = 0
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
                    ( case model.originalDimensions of
                        Nothing ->
                            model

                        Just dimensions ->
                            { model
                                | dragState = Initial
                                , offset = clampOffset (imageWidth dimensions.width model.zoom |> Basics.toFloat) moveStart moveEnd model.offset
                            }
                    , Cmd.none
                    )

        PreviewImageLoaded imageDimensions ->
            ( { model | originalDimensions = Just imageDimensions, zoom = clampZoom imageDimensions.width 0 }, Cmd.none )

        SetZoom zoomValue ->
            case model.originalDimensions of
                Nothing ->
                    ( model, Cmd.none )

                Just dimensions ->
                    ( { model
                        | zoom = clampZoom dimensions.width zoomValue
                        , offset = clampOffset (imageWidth dimensions.width model.zoom |> Basics.toFloat) ( 0, 0 ) ( 0, 0 ) model.offset
                      }
                    , Cmd.none
                    )


photoUrl =
    "https://picsum.photos/2000/4096"


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
        column [ width fill, padding 10, spacing 5 ]
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
            , label = Input.labelAbove [ padding 5 ] (text "Zoom")
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


clampZoom : Int -> Float -> Float
clampZoom originalWidth setZoom =
    if ((Basics.round setZoom * originalWidth) // 100) < sideLength then
        sideLength / Basics.toFloat originalWidth * 100

    else
        setZoom


clampOffset : Float -> ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
clampOffset zoomedWidth moveStart moveEnd currentOffset =
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

        zeroOrBelow value =
            Basics.min 0.0 value

        minimumOffsetX =
            sideLength - zoomedWidth
    in
    ( Basics.clamp minimumOffsetX 0.0 newX, zeroOrBelow newY )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
