module AnimatedButton.SimpleAnimatedButton exposing (Model, main)

import Browser
import Element exposing (Element, fill, height, image, width)
import Element.Input as Input
import Html
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated
import Simple.Animation.Property as Property


type alias Model =
    { pulse : Bool
    }


type Msg
    = Click


init : () -> ( Model, Cmd Msg )
init _ =
    ( { pulse = False }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click ->
            ( { model | pulse = not model.pulse }, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Element.layout [ width fill, height fill ] <| viewContent model


viewContent : Model -> Element Msg
viewContent model =
    Input.button []
        { onPress = Just Click
        , label =
            if model.pulse then
                animatedImage pulseAnimation
                    []
                    { src = "src/AnimatedButton/logo.png"
                    , description = "logo"
                    }

            else
                image []
                    { src = "src/AnimatedButton/logo.png"
                    , description = "logo"
                    }
        }


pulseAnimation =
    Animation.steps
        { startAt = [ Property.scale 1 ]
        , options = [ Animation.loop, Animation.easeInOutQuad ]
        }
        [ Animation.step 500 [ Property.scale 2 ]
        , Animation.step 650 [ Property.scale 1 ]
        ]


animatedUi =
    Simple.Animation.Animated.ui
        { behindContent = Element.behindContent
        , htmlAttribute = Element.htmlAttribute
        , html = Element.html
        }


animatedImage : Animation -> List (Element.Attribute msg) -> { src : String, description : String } -> Element msg
animatedImage =
    animatedUi Element.image


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
