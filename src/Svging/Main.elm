module Svging.Main exposing (Model, Msg(..), init, initialModel, main)

import Browser
import Browser.Dom
import Browser.Events
import Dict exposing (Dict)
import Element exposing (Element, column, rgb, text)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Json.Decode as Decode
import Svg exposing (circle, path, svg)
import Svg.Attributes exposing (cx, cy, d, fill, fillOpacity, height, id, r, startOffset, stroke, strokeOpacity, strokeWidth, style, transform, viewBox, width, xlinkHref)
import Svg.Events exposing (onClick, onMouseDown)
import Task


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = ClickedCircle
    | DragStart NodeId
    | DragMove NodeId Bool Position
    | DragStop NodeId Position
    | SetZoom Float
    | GotDomElement (Result Browser.Dom.Error Browser.Dom.Element)


type alias Model =
    { scale : Float
    , graphElementPosition : Position
    , clicked : Bool
    , dragState : DragState
    , nodes : Dict NodeId Node
    , edges : List Edge
    }


type alias Node =
    { position : Position
    }


type alias NodeId =
    Int


type alias Position =
    { x : Float
    , y : Float
    }


type alias Edge =
    { fromNode : NodeId
    , toNode : NodeId
    }


type DragState
    = Static
    | Moving NodeId


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Browser.Dom.getElement graphId |> Task.attempt GotDomElement )


initialModel : Model
initialModel =
    { scale = 1.0
    , graphElementPosition = Position 0 0
    , clicked = False
    , dragState = Static
    , nodes = Dict.fromList [ ( 1, node1 ), ( 2, node2 ) ]
    , edges = [ Edge 1 2 ]
    }


node1 : Node
node1 =
    { position = Position 50 50
    }


node2 : Node
node2 =
    { position = Position 75 75
    }



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedCircle ->
            ( { model | clicked = not model.clicked }, Cmd.none )

        DragStart nodeId ->
            ( { model | dragState = Moving nodeId }
            , Cmd.none
            )

        DragMove nodeId isDown pos ->
            let
                newNode =
                    Node (fromScreen pos model.scale model.graphElementPosition)
            in
            ( { model
                | nodes = Dict.insert nodeId newNode model.nodes
                , dragState =
                    if isDown then
                        Moving nodeId

                    else
                        Static
              }
            , Cmd.none
            )

        DragStop nodeId pos ->
            let
                newNode =
                    Node (fromScreen pos model.scale model.graphElementPosition)
            in
            ( { model | dragState = Static, nodes = Dict.insert nodeId newNode model.nodes }
            , Cmd.none
            )

        SetZoom float ->
            ( { model | scale = float }, Cmd.none )

        GotDomElement result ->
            case result of
                Err _ ->
                    ( model, Cmd.none )

                Ok domElement ->
                    ( { model | graphElementPosition = Position domElement.element.x domElement.element.y }, Cmd.none )


getNode nodes nodeId =
    Dict.get nodeId nodes |> Maybe.withDefault (Node (Position 0 0))


graphPixelWidth =
    500


graphPixelHeight =
    500


fromScreen : Position -> Float -> Position -> Position
fromScreen position zoom graphElementPosition =
    let
        offsetX =
            if graphElementPosition.x == 0 then
                0

            else
                graphPixelWidth / graphElementPosition.x

        offsetY =
            if graphElementPosition.y == 0 then
                0

            else
                graphPixelHeight / graphElementPosition.y
    in
    Position ((position.x * 100 - offsetX) / zoom)
        ((position.y * 100 - offsetY) / zoom)



-- View


view : Model -> Html Msg
view model =
    Element.layoutWith { options = [] }
        []
        (column []
            [ viewZoomControl model
            , viewGraph model
            ]
        )


viewZoomControl : Model -> Element Msg
viewZoomControl model =
    Input.slider
        [ Element.height (Element.px 30)
        , Element.width (Element.px 100)
        , Element.behindContent
            (Element.el
                [ Element.width Element.fill
                , Element.height (Element.px 5)
                , Element.centerY
                , Background.color (rgb 0 0 0)
                , Border.rounded 2
                ]
                Element.none
            )
        ]
        { onChange = SetZoom
        , label =
            Input.labelAbove []
                (text <| "Zoom " ++ String.fromFloat model.scale)
        , min = 0.1
        , max = 5
        , step = Nothing
        , value = model.scale
        , thumb =
            Input.defaultThumb
        }


graphId =
    "graph"


viewGraph : Model -> Element Msg
viewGraph model =
    Element.html <|
        Html.div [ Html.Attributes.id graphId ]
            [ svg
                [ width <| String.fromInt graphPixelWidth
                , height <| String.fromInt graphPixelHeight
                , viewBox "0 0 100 100"
                ]
                [ Svg.g [ transform (scale model.scale) ]
                    (drawEdges model.edges model.nodes
                        ++ drawNodes model.nodes
                    )
                ]
            ]


scale : Float -> String
scale zoom =
    "scale(" ++ String.fromFloat zoom ++ ", " ++ String.fromFloat zoom ++ ")"


drawEdges : List Edge -> Dict NodeId Node -> List (Svg.Svg Msg)
drawEdges edges dict =
    List.map (\e -> drawEdge e dict) edges


drawEdge : Edge -> Dict NodeId Node -> Svg.Svg Msg
drawEdge edge nodes =
    let
        fromNode =
            getNode nodes edge.fromNode

        toNode =
            getNode nodes edge.toNode

        idString =
            "edge" ++ String.fromInt edge.fromNode ++ "-" ++ String.fromInt edge.toNode
    in
    Svg.g []
        [ path
            [ id idString
            , d <|
                "M"
                    ++ String.fromFloat fromNode.position.x
                    ++ ","
                    ++ String.fromFloat fromNode.position.y
                    ++ " L"
                    ++ String.fromFloat toNode.position.x
                    ++ ","
                    ++ String.fromFloat toNode.position.y
            , stroke "black"
            , strokeWidth "0.1"
            ]
            []
        , Svg.text_ []
            [ Svg.textPath
                [ xlinkHref ("#" ++ idString)
                , startOffset "20%"
                , style "font-size:10"
                ]
                [ Svg.text "hej" ]
            ]
        ]


drawNodes : Dict NodeId Node -> List (Svg.Svg Msg)
drawNodes dict =
    Dict.toList dict |> List.map (\( id, node ) -> drawNode id node)


drawNode : NodeId -> Node -> Svg.Svg Msg
drawNode nodeId node =
    circle
        [ cx <| String.fromFloat node.position.x
        , cy <| String.fromFloat node.position.y
        , r "5"
        , stroke "black"
        , strokeWidth "0.4"
        , strokeOpacity "0.5"
        , fill "rgb(216,196,30)"
        , fillOpacity "1"
        , onClick ClickedCircle
        , onMouseDown (DragStart nodeId)
        ]
        []



-- Subscription
{- We listen for the "mousemove" and "mouseup" events for the whole window.
   This way we catch all events, even if they are not on our drag zone.

   Listening for mouse moves is costly though, so we only listen if there is an
   ongoing drag.
-}


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.dragState of
        Static ->
            Sub.none

        Moving id ->
            Sub.batch
                [ Browser.Events.onMouseMove (Decode.map2 (DragMove id) decodeButtons decodePosition)
                , Browser.Events.onMouseUp (Decode.map (DragStop id) decodePosition)
                ]


decodePosition : Decode.Decoder Position
decodePosition =
    Decode.map2 Position decodeFractionX decodeFractionY


decodeFractionX : Decode.Decoder Float
decodeFractionX =
    Decode.map2 (/)
        (Decode.field "pageX" Decode.float)
        (Decode.succeed graphPixelWidth)


decodeFractionY : Decode.Decoder Float
decodeFractionY =
    Decode.map2 (/)
        (Decode.field "pageY" Decode.float)
        (Decode.succeed graphPixelHeight)


decodeButtons : Decode.Decoder Bool
decodeButtons =
    Decode.field "buttons" (Decode.map (\buttons -> buttons == 1) Decode.int)
