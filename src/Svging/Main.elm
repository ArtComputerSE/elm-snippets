module Svging.Main exposing (Model, Msg(..), init, initialModel, main)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html, div)
import Json.Decode as Decode
import Svg exposing (circle, line, svg)
import Svg.Attributes exposing (cx, cy, fill, fillOpacity, height, r, stroke, strokeOpacity, transform, viewBox, width, x1, x2, y1, y2)
import Svg.Events exposing (onClick, onMouseDown)


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
    | DragMove NodeId Bool Float Float
    | DragStop NodeId Float Float


type alias Model =
    { clicked : Bool
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
    | Moving NodeId Float Float


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    { clicked = False
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
            let
                node =
                    getNode model.nodes nodeId
            in
            ( { model | dragState = Moving nodeId (node.position.x / 100) (node.position.y / 100) }
            , Cmd.none
            )

        DragMove nodeId isDown fractionX fractionY ->
            ( { model
                | dragState =
                    if isDown then
                        Moving nodeId fractionX fractionY

                    else
                        Static
              }
            , Cmd.none
            )

        DragStop nodeId fractionX fractionY ->
            let
                newNode =
                    Node (Position (fractionX * 100) (fractionY * 100))
            in
            ( { model | dragState = Static, nodes = Dict.insert nodeId newNode model.nodes }
            , Cmd.none
            )


getNode nodes nodeId =
    Dict.get nodeId nodes |> Maybe.withDefault (Node (Position 0 0))



-- View


view : Model -> Html Msg
view model =
    div []
        [ svg
            [ width "500"
            , height "500"
            , viewBox "0 0 100 100"
            ]
            [ Svg.g [ transform "scale(1, 1)" ]
                (drawEdges model.dragState model.edges model.nodes
                    ++ drawNodes model.dragState model.nodes
                )
            ]
        ]


drawEdges : DragState -> List Edge -> Dict NodeId Node -> List (Svg.Svg Msg)
drawEdges dragState edges dict =
    List.map (\e -> drawEdge dragState e dict) edges


drawEdge : DragState -> Edge -> Dict NodeId Node -> Svg.Svg Msg
drawEdge dragState edge nodes =
    let
        fromPos =
            nodePostion edge.fromNode fromNode dragState

        fromNode =
            getNode nodes edge.fromNode

        toPos =
            nodePostion edge.toNode toNode dragState

        toNode =
            getNode nodes edge.toNode
    in
    line
        [ x1 <| String.fromFloat fromPos.x
        , y1 <| String.fromFloat fromPos.y
        , x2 <| String.fromFloat toPos.x
        , y2 <| String.fromFloat toPos.y
        , stroke "black"
        ]
        []


drawNodes : DragState -> Dict NodeId Node -> List (Svg.Svg Msg)
drawNodes dragState dict =
    Dict.toList dict |> List.map (\( id, node ) -> drawNode dragState id node)


drawNode : DragState -> NodeId -> Node -> Svg.Svg Msg
drawNode dragState nodeId node =
    circle
        [ cx <| String.fromFloat (nodePostion nodeId node dragState).x
        , cy <| String.fromFloat (nodePostion nodeId node dragState).y
        , r "5"
        , stroke "black"
        , strokeOpacity "0.5"
        , fill "rgb(216,196,30)"
        , fillOpacity "1"
        , onClick ClickedCircle
        , onMouseDown (DragStart nodeId)
        ]
        []


nodePostion : NodeId -> Node -> DragState -> Position
nodePostion nodeId node dragState =
    case dragState of
        Static ->
            Position node.position.x node.position.y

        Moving id x y ->
            if id == nodeId then
                Position (x * 100) (y * 100)

            else
                Position node.position.x node.position.y



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

        Moving id _ _ ->
            Sub.batch
                [ Browser.Events.onMouseMove (Decode.map3 (DragMove id) decodeButtons decodeFractionX decodeFractionY)
                , Browser.Events.onMouseUp (Decode.map2 (DragStop id) decodeFractionX decodeFractionY)
                ]



{- The goal here is to get (mouse x / window width) on each mouse event. So if
   the mouse is at 500px and the screen is 1000px wide, we should get 0.5 from this.

   Getting the mouse x is not too hard, but getting window width is a bit tricky.
   We want the window.innerWidth value, which happens to be available at:

       event.currentTarget.defaultView.innerWidth

   The value at event.currentTarget is the document in these cases, but this will
   not work if you have a <section> or a <div> with a normal elm/html event handler.
   So if currentTarget is NOT the document, you should instead get the value at:

       event.currentTarget.ownerDocument.defaultView.innerWidth
                           ^^^^^^^^^^^^^
-}


decodeFractionX : Decode.Decoder Float
decodeFractionX =
    Decode.map2 (/)
        (Decode.field "pageX" Decode.float)
        (Decode.succeed 500)


decodeFractionY : Decode.Decoder Float
decodeFractionY =
    Decode.map2 (/)
        (Decode.field "pageY" Decode.float)
        (Decode.succeed 500)



{- What happens when the user is dragging, but the "mouse up" occurs outside
   the browser window? We need to stop listening for mouse movement and end the
   drag. We use MouseEvent.buttons to detect this:

       https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/buttons

   The "buttons" value is 1 when "left-click" is pressed, so we use that to
   detect zombie drags.
-}


decodeButtons : Decode.Decoder Bool
decodeButtons =
    Decode.field "buttons" (Decode.map (\buttons -> buttons == 1) Decode.int)
