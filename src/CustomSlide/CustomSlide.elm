module CustomSlide.CustomSlide exposing (Model, Msg(..), init, main)

import Browser
import Element
import Element.Input
import Html
import Svg exposing (circle, defs, g, linearGradient, path, stop, svg)
import Svg.Attributes exposing (cx, cy, d, fill, fillOpacity, height, id, offset, r, stroke, strokeOpacity, style, transform, viewBox, width, x1, x2, y1, y2)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { value : Float
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( { value = 0.5
      }
    , Cmd.none
    )


type Msg
    = ChangedValue Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedValue newValue ->
            ( { model | value = newValue }, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Element.layoutWith
        { options =
            [ Element.focusStyle
                { borderColor = Nothing
                , backgroundColor = Nothing
                , shadow = Nothing
                }
            ]
        }
        []
        (Element.column []
            [ Element.Input.slider
                [ Element.height (Element.px 120)
                , Element.width (Element.px 200)
                , Element.behindContent (viewSvg model.value)
                ]
                { onChange = ChangedValue
                , label = Element.Input.labelBelow [] (Element.text <| "value: " ++ String.fromFloat model.value)
                , min = 0.0
                , max = 1.0
                , value = model.value
                , thumb = Element.Input.thumb []
                , step = Just 0.01
                }
            ]
        )


viewSvg : Float -> Element.Element msg
viewSvg value =
    let
        rotateStr =
            "rotate(" ++ rotationValue ++ ", 50,50)"

        rotationValue =
            value * 180 - 90 |> String.fromFloat
    in
    Element.html <|
        svg
            [ width "200"
            , height "120"
            , viewBox "0 0 100 60"
            ]
            [ defs []
                [ linearGradient [ id "grad1", x1 "0%", y1 "0%", x2 "100%", y2 "0%" ]
                    [ stop [ offset "0%", style "stop-color:rgb(5,204,251);stop-opacity:1" ] []
                    , stop [ offset "50%", style "stop-color:rgb(216,196,30);stop-opacity:1" ] []
                    , stop [ offset "100%", style "stop-color:rgb(220,67,14);stop-opacity:1" ] []
                    ]
                ]
            , path
                [ fill "url('#grad1')"
                , stroke "black"
                , strokeOpacity "0.5"
                , d """
                      M 0,51
                      L 100,51
                      A 25,25 0,0,0 0,51
                      """
                ]
                []
            , circle [ cx "50", cy "50", r "5", stroke "black", strokeOpacity "0.5", fill "rgb(216,196,30)", fillOpacity "1" ] []
            , g [ transform rotateStr ]
                [ path
                    [ fill "rgb(216,196,30)"
                    , stroke "black"
                    , strokeOpacity "0.5"
                    , d """
                  M 47.5,60
                  L 50,0
                  L 52.5,60
                  L 47.5,60
                  """
                    ]
                    []
                ]
            , circle [ cx "50", cy "50", r "1", fill "black", fillOpacity "0.5" ] []
            ]
