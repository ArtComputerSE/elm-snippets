module CropImage.Simple exposing (Model, Msg(..), cropDataDump, init, main, subscriptions, update, view)

import Browser
import Cropper
import Element exposing (Element, column, el, html, image, px, row, spacing, text, width)
import Element.Input as Input
import Html
import Html.Attributes
import Html.Events


type Msg
    = ToCropper Cropper.Msg
    | Zoom Float
    | ZoomHtml String


type alias Model =
    { cropper : Cropper.Model
    }


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


photoUrl =
    "src/CropImage/images/kittens-1280x711.jpg"


init : () -> ( Model, Cmd Msg )
init _ =
    ( { cropper =
            Cropper.init
                { url = photoUrl
                , crop = { width = 720, height = 480 }
                }
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ToCropper (Cropper.subscriptions model.cropper)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToCropper subMsg ->
            let
                ( updatedSubModel, subCmd ) =
                    Cropper.update subMsg model.cropper
            in
            ( { model | cropper = updatedSubModel }, Cmd.map ToCropper subCmd )

        Zoom zoom ->
            ( { model | cropper = Cropper.zoom model.cropper zoom }, Cmd.none )

        ZoomHtml zoom ->
            ( { model | cropper = Cropper.zoom model.cropper (Maybe.withDefault 0 (String.toFloat zoom)) }, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Element.layout [] <|
        column []
            [ el [] <| text "Elm Cropper Simple Example"
            , html (Html.div [] [ Html.text "ZZZZ", Cropper.view model.cropper |> Html.map ToCropper ])
            , el [] <|
                image [ width (px 300) ]
                    { description = "cute!"
                    , src = photoUrl
                    }
            , Input.slider []
                { onChange = Zoom
                , label = Input.labelAbove [] <| el [] <| text "Zoom"
                , min = 0
                , max = 1
                , value = model.cropper.zoom
                , thumb = Input.defaultThumb
                , step = Just 0.0001
                }
            , cropDataDump <| Cropper.cropData model.cropper
            ]


viewHtml : Model -> Html.Html Msg
viewHtml model =
    Html.div []
        [ Html.p [ Html.Attributes.class "controls__row" ]
            [ Html.label [] [ Html.text "Z" ]
            , Cropper.view model.cropper |> Html.map ToCropper
            , Html.input
                [ Html.Events.onInput ZoomHtml
                , Html.Attributes.type_ "range"
                , Html.Attributes.min "0"
                , Html.Attributes.max "1"
                , Html.Attributes.step "0.0001"
                , Html.Attributes.value (String.fromFloat model.cropper.zoom)
                ]
                []
            ]
        ]


cropDataDump : Cropper.CropData -> Element Msg
cropDataDump data =
    let
        rectDebug rect =
            String.fromInt rect.width ++ "x" ++ String.fromInt rect.height

        pointDebug point =
            String.fromInt point.x ++ "|" ++ String.fromInt point.y
    in
    column []
        [ row [ spacing 5 ] [ el [] <| text "url", el [] <| text data.url ]
        , row [ spacing 5 ] [ el [] <| text "size", el [] <| text <| rectDebug data.size ]
        , row [ spacing 5 ] [ el [] <| text "crop", el [] <| text <| rectDebug data.crop ]
        , row [ spacing 5 ] [ el [] <| text "resized", el [] <| text <| rectDebug data.resized ]
        , row [ spacing 5 ] [ el [] <| text "origin", el [] <| text <| pointDebug data.origin ]
        ]
