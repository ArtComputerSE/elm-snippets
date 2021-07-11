port module CropImage.Demo exposing (main)

import Browser
import Croppie
import Croppie.BindOptions exposing (orientation, points, url, zoom)
import Croppie.Events exposing (onResult, onUpdate)
import Croppie.Options exposing (..)
import Croppie.ResultOptions exposing (Size(..), size)
import File exposing (File)
import File.Select as Select
import Html exposing (Html, text)
import Html.Attributes exposing (attribute, class, href, id, name, placeholder, src, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Encode as Encode
import Svg
import Svg.Attributes as SvgAttrs exposing (d, fill, height, viewBox, width)
import Task


port croppie : Croppie.Data -> Cmd msg


port sweetAlert : String -> Cmd msg


version =
    "1.0.3"


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { hidden : Bool
    , size : Size
    , url : Maybe String
    }


type alias Size =
    { width : Maybe Int
    , height : Maybe Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        bind_ params =
            Croppie.bind params.id params.options
    in
    ( { hidden = False
      , size = Size Nothing Nothing
      , url = Nothing
      }
    , Cmd.batch <|
        List.map (croppie << bind_)
            [ { id = "cropper-1"
              , options =
                    [ url "images/demo-1.jpg" ]
              }
            , { id = "demo-basic"
              , options =
                    [ url "images/cat.jpg"
                    , points [ 77, 469, 280, 739 ]
                    ]
              }
            , { id = "vanilla-demo"
              , options =
                    [ url "images/demo-2.jpg"
                    , orientation 4
                    ]
              }
            , { id = "resizer-demo"
              , options = [ url "images/demo-2.jpg" ]
              }
            , { id = "hidden-demo"
              , options = [ url "images/demo-3.jpg" ]
              }
            ]
    )


type Msg
    = FileLoaded File
    | GotUrl String
    | ResultButtonClicked String
    | RotateButtonClicked Int
    | ShowCroppedImage (Croppie.Result Msg)
    | ToggleButtonClicked
    | UpdatedSize Size
    | UploadButtonClicked


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FileLoaded file ->
            ( model
            , File.toUrl file
                |> Task.perform GotUrl
            )

        GotUrl url_ ->
            ( { model | url = Just url_ }
            , croppie <|
                Croppie.bind "upload-demo" [ url url_ ]
            )

        ResultButtonClicked id_ ->
            ( model
            , croppie <|
                Croppie.result id_ <|
                    case id_ of
                        "demo-basic" ->
                            [ size (Custom model.size) ]

                        _ ->
                            []
            )

        RotateButtonClicked degrees ->
            ( model
            , croppie <|
                Croppie.rotate "vanilla-demo" degrees
            )

        ShowCroppedImage res ->
            ( model
            , case res of
                Croppie.Canvas url ->
                    sweetAlert url

                _ ->
                    Cmd.none
            )

        ToggleButtonClicked ->
            ( { model | hidden = not model.hidden }
            , Cmd.none
            )

        UpdatedSize size ->
            ( { model | size = size }
            , Cmd.none
            )

        UploadButtonClicked ->
            ( model
            , Select.file [ "image/jpeg", "image/png" ] FileLoaded
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = "Croppie For Elm - The wrapper library of Croppie for Elm"
    , body = body model
    }


body : Model -> List (Html Msg)
body model =
    [ hero
    , simple
    , installation
    , usage
    , documentation
    , demos model
    , importantNotes
    , browsers
    , who
    , footer
    , githubCorner
    ]


hero : Html Msg
hero =
    Html.section [ class "hero" ]
        [ Html.a
            [ id "home"
            , name "home"
            ]
            []
        , Html.div [ class "container" ]
            [ Html.nav []
                [ Html.a [ href "#options" ] [ text "Usage" ]
                , Html.a [ href "#documentation" ] [ text "Documentation" ]
                , Html.a [ href "#demos" ] [ text "Demos" ]
                , Html.a [ href "#browsers" ] [ text "About" ]
                ]
            , Html.div [ class "grid" ]
                [ Html.div [ class "col-1-2" ]
                    [ Html.h1 [] [ text "Croppie for Elm" ]
                    , Html.h2 []
                        [ text "This is a wrapper library of "
                        , Html.a [ href "http://github.com/foliotek/croppie" ] [ text "Croppie" ]
                        ]
                    , Html.h2 []
                        [ Html.i []
                            [ text "Croppie is a fast, easy to use image cropping plugin with tons of configuration options!" ]
                        ]
                    , Html.div [ class "buttons" ]
                        [ Html.a
                            [ class "btn"
                            , href "http://github.com/hallelujahdrive/elm-croppie"
                            ]
                            [ text "View on GitHub" ]
                        , text " "
                        , Html.button
                            [ class "js-main-image"
                            , onClick (ResultButtonClicked "cropper-1")
                            ]
                            [ text "Get Result" ]
                        ]
                    ]
                , Html.div [ class "col-1-2" ]
                    [ Croppie.croppie
                        [ boundary { width = 300, height = 300 }
                        , viewport { width = 150, height = 150, type_ = Circle }
                        ]
                        [ id "cropper-1"
                        , onResult ShowCroppedImage
                        ]
                    ]
                ]
            ]
        ]


simple : Html Msg
simple =
    Html.section [ class "simple" ]
        [ Html.div [ class "section-header" ] [ Html.h2 [] [ text "The Basics" ] ]
        , Html.div [ class "container" ]
            [ Html.pre [ class "language-elm" ]
                [ Html.code [ class "language-elm" ] [ text simpleCode ] ]
            ]
        ]


installation : Html Msg
installation =
    Html.section [ class "installation" ]
        [ Html.a
            [ id "install"
            , name "install"
            ]
            []
        , Html.div [ class "section-header" ] [ Html.h2 [] [ text "Installation" ] ]
        , Html.div [ class "container" ]
            [ Html.p [] [ Html.strong [] [ text "Elm:" ] ]
            , Html.pre
                [ class "language-elm"
                , style "width" "390px"
                ]
                [ Html.code [ class "language-elm" ] [ text <| "elm install hallelujahdrive/elm-croppie@" ++ version ]
                ]
            , Html.hr [] []
            , Html.p [] [ Html.strong [] [ text "Then add the following elements to your page:" ] ]
            , Html.pre [ class "language-html" ]
                [ Html.code [ class "language-html" ]
                    [ text <| """<link rel="stylesheet" href ="https://unpkg.com/elm-croppie@""" ++ version ++ """/dist/elm-croppie.css" />
<script src="https://unpkg.com/elm-croppie@""" ++ version ++ """/dist/elm-croppie.js"></script>"""
                    ]
                ]
            , Html.p [] [ Html.strong [] [ text "If you use bundler please install the Javascript and CSS assets via npm:" ] ]
            , Html.pre
                [ class "language-js"
                , style "width" "390px"
                ]
                [ Html.code [ class "language-elm" ] [ text <| "npm install elm-croppie@" ++ version ]
                ]
            , Html.p [] [ Html.strong [] [ text "Then in your Javascript add following import:" ] ]
            , Html.pre [ class "language-js" ]
                [ Html.code [ class "language-js" ] [ text "require(\"elm-croppie\");" ]
                ]
            ]
        ]


usage : Html Msg
usage =
    Html.section []
        [ Html.a [ name "options" ]
            [ Html.div [ class "section-header" ] [ Html.h2 [] [ text "Usage" ] ]
            , Html.div [ class "container" ]
                [ Html.p [] [ text "You can initialize ElmCroppie with the following elm code:" ]
                , Html.pre [ class "language-elm" ]
                    [ Html.code [ class "language-elm" ] [ text usageCodeElm ] ]
                , Html.p [] [ text "You also need the following javascript code to communicate on the port." ]
                , Html.pre [ class "language-js" ]
                    [ Html.code [ class "language-js" ] [ text usageCodeJs ] ]
                ]
            ]
        ]


documentation : Html Msg
documentation =
    Html.section [ class "documentation" ]
        [ Html.a
            [ id "documentation"
            , name "documentation"
            ]
            []
        , Html.div [ class "section-header" ] [ Html.h2 [] [ text "Documentation" ] ]
        , Html.div [ class "container" ]
            [ croppieDoc
            , optionsDoc
            , bindOptionsDoc
            , resultOptionsDoc
            , eventsDoc
            ]
        ]


croppieDoc : Html Msg
croppieDoc =
    Html.section []
        [ Html.h3 [] [ text "Croppie" ]
        , Html.ul []
            [ Html.li [ id "get" ]
                [ Html.strong [ class "focus" ] [ text "get : String -> Data" ]
                , Html.p []
                    [ text "Get crop points, and the zoom of the image. The return value can be received with "
                    , Html.code [ class "language-elm" ] [ text "Croppie.Events.onGet" ]
                    , text "."
                    ]
                , Html.ul [ class "parameter-list" ]
                    [ Html.li []
                        [ Html.code [ class "language-elm" ] [ text "id" ]
                        , text " Id of the target Croppie element"
                        ]
                    ]
                ]
            , Html.li [ id "bind" ]
                [ Html.strong [ class "focus" ] [ text "bind : String -> List BindOption -> Data" ]
                , Html.p [] [ text "Bind an image to the croppie. Returns a promise to be resolved when the image has been loaded and the croppie has been initialized." ]
                , Html.ul [ class "parameter-list" ]
                    [ Html.li []
                        [ Html.code [ class "language-elm" ] [ text "id" ]
                        , text " id of the target Croppie element"
                        ]
                    , Html.li []
                        [ Html.code [ class "language-elm" ] [ text "bindOptions" ]
                        , text " List of bind options (see 'Croppie.BindOptions')."
                        ]
                    ]
                ]
            , Html.li [ id "result" ]
                [ Html.strong [ class "focus" ]
                    [ text "result : String -> List ResultOptions -> Data" ]
                , Html.p []
                    [ text "Get the resulting crop of the image. The return value can be received with "
                    , Html.code [ class "language-elm" ] [ text "Croppie.Events.onResult" ]
                    , text "."
                    ]
                , Html.ul [ class "parameter-list" ]
                    [ Html.li []
                        [ Html.code [ class "language-elm" ] [ text "id" ]
                        , text " id of the target Croppie element"
                        ]
                    , Html.li []
                        [ Html.code [ class "language-elm" ] [ text "resultOptions" ]
                        , text " List of result options (see 'Croppie.ResultOptions')."
                        ]
                    ]
                ]
            , Html.li [ id "rotate" ]
                [ Html.strong [ class "focus" ]
                    [ text "rotate : String -> Int -> Data" ]
                , Html.p []
                    [ text "Rotate the image by a specified degree amount.  Only works with "
                    , Html.code [ class "language-elm" ] [ text "enableOrientation" ]
                    , text " opton enabled (see 'Croppie.Options')."
                    ]
                , Html.ul [ class "parameter-list" ]
                    [ Html.li []
                        [ Html.code [ class "language-elm" ] [ text "id" ]
                        , text " id of the target Croppie element"
                        ]
                    , Html.li []
                        [ Html.code [ class "language-elm" ] [ text "degrees" ]
                        , text " rotation angle"
                        , Html.br [] []
                        , Html.span [ class " default" ] [ text " Valid Values:" ]
                        , Html.code [] [ text "90, 180, 270, -90, -180, 270" ]
                        ]
                    ]
                ]
            , Html.li [ id "setZoom" ]
                [ Html.strong [ class "focus" ]
                    [ text "setZoom : String -> Float -> Data" ]
                , Html.p [] [ text "Set the zoom of a Croppie instance.  The value passed in is still restricted to the min/max set by Croppie." ]
                , Html.ul [ class "parameter-list" ]
                    [ Html.li []
                        [ Html.code [ class "language-elm" ] [ text "id" ]
                        , text " id of the target Croppie element"
                        ]
                    , Html.li []
                        [ Html.code [ class "language-elm" ] [ text "value" ]
                        , text " a floating point to scale the image within the croppie.  Must be between a min and max value set by croppie."
                        ]
                    ]
                ]
            ]
        ]


optionsDoc : Html Msg
optionsDoc =
    Html.section []
        [ Html.h3 [] [ text "Croppie.Options" ]
        , Html.ul []
            [ Html.li [ id "boundary" ]
                [ Html.pre []
                    [ Html.strong [ class "focus" ]
                        [ text """boundary :
   { width : Int
   , height : Int
   }
   -> Option"""
                        ]
                    ]
                , Html.p [] [ text "The outer container of the cropper" ]
                , Html.span [ class "default" ] [ text "Default" ]
                , Html.code [ class "language-html" ] [ text "will default to the size of the container" ]
                ]
            , Html.li [ id "customClass" ]
                [ Html.strong [ class "focus" ] [ text "customClass : String -> Option" ]
                , Html.p [] [ text "A class of your choosing to add to the container to add custom styles to your croppie" ]
                , Html.span [ class "default" ] [ text "Default" ]
                , Html.code [ class "language-elm" ] [ text "\"\"" ]
                ]
            , Html.li [ id "enableExif" ]
                [ Html.strong [ class "focus" ] [ text "enableExif : Bool -> Option" ]
                , Html.p [] [ text "Enable exif orientation reading.  Tells Croppie to read exif orientation from the image data and orient the image correctly before rendering to the page." ]
                , Html.p []
                    [ text "Requires "
                    , Html.a [ href "https://github.com/exif-js/exif-js" ] [ text "exif.js" ]
                    ]
                , Html.span [ class "default" ] [ text "Deafult" ]
                , Html.code [ class "language-elm" ] [ text "False" ]
                ]
            , Html.li [ id "enableOrientation" ]
                [ Html.strong [ class "focus" ] [ text "enableOrientation : Bool -> Option" ]
                , Html.p [] [ text "Enable or disable support for specifying a custom orientation when binding images (See bind function)" ]
                , Html.span [ class "default" ] [ text "Default" ]
                , Html.code [ class "language-elm" ] [ text "False" ]
                ]
            , Html.li [ id "enableResize" ]
                [ Html.strong [ class "focus" ] [ text "enableResize : Bool -> Option" ]
                , Html.p [] [ text "Enable or disable support for resizing the viewport area." ]
                , Html.span [ class "default" ] [ text "Default" ]
                , Html.code [ class "language-elm" ] [ text "False" ]
                ]
            , Html.li [ id "enableZoom" ]
                [ Html.strong [ class "focus" ] [ text "enableZoom : Bool -> Option" ]
                , Html.p [] [ text "Enable zooming functionality. If set to false - scrolling and pinching would not zoom." ]
                , Html.span [ class "default" ] [ text "Default" ]
                , Html.code [ class "language-elm" ] [ text "True" ]
                ]
            , Html.li [ id "enforceBoundary" ]
                [ Html.strong [ class "focus" ] [ text "enforceBoundary : Bool -> Option" ]
                , Html.p [] [ text "Restricts zoom so image cannot be smaller than viewport" ]
                , Html.span [ class "default" ] [ text "Default" ]
                , Html.code [ class "language-elm" ] [ text "True" ]
                ]
            , Html.li [ id "mouseWheelZoom" ]
                [ Html.strong [ class "focus" ] [ text "mouseWheelZoom : MouseWheelZoom -> Option" ]
                , Html.p []
                    [ text "Enable or disable the ability to use the mouse wheel to zoom in and out on a croppie instance. If "
                    , Html.code [ class "language-elm" ] [ text "Ctrl" ]
                    , text "is passed mouse wheel will only work while control keyboard is pressed"
                    ]
                , Html.span [ class "default" ] [ text "Default" ]
                , Html.code [ class "language-elm" ] [ text "Enabled" ]
                , Html.br [] []
                , Html.br [] []
                , Html.span [ class "default" ] [ text "Valid values" ]
                , Html.code [ class "language-elm" ]
                    [ text "type MouseWheelZoom = Enabled | Disabled | Ctrl" ]
                ]
            , Html.li [ id "showZoomer" ]
                [ Html.strong [ class "focus" ] [ text "showZoomer : Bool -> Option" ]
                , Html.p [] [ text "Hide or Show the zoom slider" ]
                , Html.span [ class "default" ] [ text "Default" ]
                , Html.code [ class "language-elm" ] [ text "True" ]
                ]
            , Html.li [ id "viewport" ]
                [ Html.pre []
                    [ Html.strong [ class "focus" ]
                        [ text """viewport :
   { width : Int
   , height : Int
   , type_ : CropType
   }
   -> Option"""
                        ]
                    ]
                , Html.p [] [ text "The inner container of the coppie. The visible part of the image" ]
                , Html.span [ class "default" ] [ text "Default" ]
                , Html.code [ class "language-elm" ] [ text "{ width = 100, height = 100, type_ = Circle }" ]
                , Html.br [] []
                , Html.br [] []
                , Html.span [ class "default" ] [ text "Valid type_ values " ]
                , Html.code [ class "language-elm" ]
                    [ text "type CropType = Square | Circle" ]
                ]
            ]
        ]


bindOptionsDoc : Html Msg
bindOptionsDoc =
    let
        rotateValid_ index label =
            Html.li []
                [ Html.code [ class "language-elm" ] [ text <| String.fromInt <| index + 1 ]
                , text <| " " ++ label
                ]
    in
    Html.section []
        [ Html.h3 [] [ text "Cropie.BindOptions" ]
        , Html.ul []
            [ Html.li [ id "url" ]
                [ Html.strong [ class "focus" ] [ text "url : String -> BindOption" ]
                , Html.p [] [ text "Url to image" ]
                ]
            , Html.li [ id "points" ]
                [ Html.strong [ class "focus" ] [ text "points : List Int -> BindOption" ]
                , Html.p []
                    [ text "Array of points that translate into "
                    , Html.code [ class "language-elm" ] [ text "[topLeftX, topLeftY, bottomRightX, bottomRightY]" ]
                    ]
                ]
            , Html.li [ id "zoom" ]
                [ Html.strong [ class "focus" ] [ text "zoom : Float -> BindOption" ]
                , Html.p [] [ text " Apply zoom after image has been bound" ]
                ]
            , Html.li [ id "orientatin" ]
                [ Html.strong [ class "focus" ] [ text "orientation : Int -> BindOption" ]
                , Html.p []
                    [ text "Custom orientation, applied after exif orientation (if enabled). Only works with "
                    , Html.code [ class "language-elm" ] [ text "enableOrientation" ]
                    , text " option enabled (see 'Croppie.Option')."
                    ]
                , Html.span [ class "default" ] [ text "Valid options are:" ]
                , Html.ul [ class "parameter-list" ] <|
                    List.indexedMap rotateValid_
                        [ " unchanged"
                        , " flipped horizontally"
                        , " rotated 180 degrees"
                        , " flipped vertically"
                        , " flipped horizontally, then rotated left by 90 degrees"
                        , " rotated clockwise by 90 degrees"
                        , " flipped horizontally, then rotaetd right by 90 degrees"
                        , " rotated counter-clockwise by 90 degrees"
                        ]
                ]
            ]
        ]


resultOptionsDoc : Html Msg
resultOptionsDoc =
    Html.section []
        [ Html.h3 [] [ text "Croppie.ResultOptions" ]
        , Html.ul []
            [ Html.li [ id "type" ]
                [ Html.strong [ class "focus" ] [ text "type_ : Type -> ResultOption" ]
                , Html.p []
                    [ text "The type of result to return defaults to "
                    , Html.code [ class "language-elm" ] [ text "Canvas" ]
                    ]
                , Html.span [ class "default" ] [ text "Valid values" ]
                , Html.code [ class "language-elm" ] [ text "type Type = Base64 | Canvas | Html" ]
                ]
            , Html.li [ id "size" ]
                [ Html.strong [ class "focus" ] [ text "size : Size -> ResultOption" ]
                , Html.p []
                    [ text "The size of the cropped image defaults to "
                    , Html.code [ class "language-elm" ] [ text "Viewport" ]
                    ]
                , Html.span [ class "default" ] [ text "Valid values" ]
                , Html.code [ class "language-elm" ] [ text "type Size = Viewport | Original | Custom { width : Maybe Int, height : Maybe Int }" ]
                ]
            , Html.li [ id "format" ]
                [ Html.strong [ class "focus" ] [ text "format : Format -> ResultOption" ]
                , Html.p []
                    [ text "Indicating the image format. If you do not specify a format, the default is "
                    , Html.code [ class "language-elm" ] [ text "Png" ]
                    , text "."
                    ]
                , Html.span [ class "default" ] [ text "Valid values" ]
                , Html.code [ class "language-elm" ] [ text "type Format = Jpeg | Png | Webp" ]
                ]
            , Html.li [ id "quarity" ]
                [ Html.strong [ class "focus" ] [ text "quarity : Int -> ResultOption" ]
                , Html.p []
                    [ text "Number between "
                    , Html.code [ class "language-elm" ] [ text "0" ]
                    , text " and "
                    , Html.code [ class "language-elm" ] [ text "1" ]
                    , text " indicating image quarity."
                    ]
                , Html.span [ class "default" ] [ text "Default" ]
                , Html.code [ class "language-elm" ] [ text "1" ]
                ]
            , Html.li [ id "circle" ]
                [ Html.strong [ class "focus" ] [ text "circle : Bool -> ResultOption" ]
                , Html.p [] [ text "Force the result to be cropped into a circle." ]
                , Html.span [ class "default" ] [ text "Valid Values" ]
                , Html.code [ class "language-elm" ] [ text "type Bool = True | False" ]
                ]
            ]
        ]


eventsDoc : Html Msg
eventsDoc =
    Html.section []
        [ Html.h3 [] [ text "Croppie.Events" ]
        , Html.ul []
            [ Html.li [ id "onUpdate" ]
                [ Html.strong [ class "focus" ] [ text "onUpdate : (CropData -> msg) -> Html.Attribute msg" ]
                , Html.p [] [ text "Detect update evnents. Update events trigged when a drag or zoom occurs" ]
                ]
            , Html.li [ id "onGet" ]
                [ Html.strong [ class "focus" ] [ text "onGet : (CropData -> msg) -> Html.Attribute msg" ]
                , Html.p [] [ text "Receive get callbacks" ]
                ]
            , Html.li [ id "onResult" ]
                [ Html.strong [ class "focus" ] [ text "onResult : (String -> msg) -> Html.Attribute msg" ]
                , Html.p [] [ text "Receive result callbacks" ]
                ]
            ]
        , Html.pre [ class "language-elm" ]
            [ Html.code [ class "language-elm" ] [ text eventsExample ]
            ]
        ]


demos : Model -> Html Msg
demos model =
    Html.section [] <|
        [ Html.a
            [ id "demos"
            , name "demos"
            ]
            []
        , Html.div [ class "section-header" ] [ Html.h2 [] [ text "Demos" ] ]
        , demoBasic model
        , demoVanilla
        , demoResizer
        , demoUpload model.url
        , demoHidden model.hidden
        ]


viewDemoR : String -> String -> List (Html Msg) -> Html Msg -> Html Msg
viewDemoR code title actions croppieElt =
    Html.div [ class "demo-wrap" ]
        [ Html.div [ class "container" ]
            [ Html.div [ class "grid" ]
                [ Html.div [ class "col-1-2" ]
                    [ Html.strong [] [ text title ]
                    , Html.pre [ class "language-elm" ]
                        [ Html.code [ class "language-elm" ] [ text code ] ]
                    , Html.div [ class "actions" ] actions
                    ]
                , Html.div [ class "col-1-2" ] [ croppieElt ]
                ]
            ]
        ]


viewDemoL : String -> String -> List (Html Msg) -> Html Msg -> Html Msg
viewDemoL code title actions croppieElt =
    Html.div [ class "demo-wrap" ]
        [ Html.div [ class "container" ]
            [ Html.div [ class "grid" ]
                [ Html.div [ class "col-1-2" ] [ croppieElt ]
                , Html.div [ class "col-1-2" ]
                    [ Html.strong [] [ text title ]
                    , Html.pre [ class "language-elm" ]
                        [ Html.code [ class "language-elm" ] [ text code ] ]
                    , Html.div [ class "actions" ] actions
                    ]
                ]
            ]
        ]


demoBasic : Model -> Html Msg
demoBasic { size } =
    let
        updatedWidth width_ =
            UpdatedSize { size | width = String.toInt width_ }

        updatedHeight height_ =
            UpdatedSize { size | height = String.toInt height_ }
    in
    viewDemoR
        basicCode
        "Basic Example"
        [ Html.button
            [ class "basic-result"
            , onClick (ResultButtonClicked "demo-basic")
            ]
            [ text "Result" ]
        , Html.input
            [ type_ "number"
            , class "basic-width"
            , placeholder "width"
            , onInput updatedWidth
            , case size.width of
                Just w ->
                    value (String.fromInt w)

                _ ->
                    value ""
            ]
            []
        , text "x"
        , Html.input
            [ type_ "number"
            , class "basic-height"
            , placeholder "height"
            , onInput updatedHeight
            , case size.height of
                Just h ->
                    value (String.fromInt h)

                _ ->
                    value ""
            ]
            []
        ]
        (Croppie.croppie
            [ boundary { width = 300, height = 300 }
            , viewport { width = 150, height = 200, type_ = Square }
            ]
            [ id "demo-basic"
            , onResult ShowCroppedImage
            ]
        )


demoVanilla : Html Msg
demoVanilla =
    viewDemoL
        vanillaCode
        "Vanilla Example"
        [ Html.button
            [ class "vanilla-result"
            , onClick (ResultButtonClicked "vanilla-demo")
            ]
            [ text "Result" ]
        , Html.button
            [ class "vanilla-rotate"
            , onClick (RotateButtonClicked -90)
            ]
            [ text "Rotate Left" ]
        , Html.button
            [ class "vanilla-rotate"
            , onClick (RotateButtonClicked 90)
            ]
            [ text "Rotate Right" ]
        ]
        (Croppie.croppie
            [ boundary { width = 300, height = 300 }
            , viewport { width = 200, height = 100, type_ = Square }
            , showZoomer False
            , enableOrientation True
            ]
            [ id "vanilla-demo"
            , onResult ShowCroppedImage
            ]
        )


demoResizer : Html Msg
demoResizer =
    viewDemoR
        resizerCode
        "Resizer Example"
        [ Html.button
            [ class "resizer-result"
            , onClick (ResultButtonClicked "resizer-demo")
            ]
            [ text "Result" ]
        ]
        (Croppie.croppie
            [ boundary { width = 300, height = 300 }
            , viewport { width = 100, height = 100, type_ = Square }
            , showZoomer False
            , enableResize True
            , enableOrientation True
            , mouseWheelZoom Ctrl
            ]
            [ id "resizer-demo"
            , onResult ShowCroppedImage
            ]
        )


demoUpload : Maybe String -> Html Msg
demoUpload original =
    Html.div [ class "demo-wrap upload-demo" ]
        [ Html.div [ class "container" ]
            [ Html.div [ class "grid" ]
                [ Html.div [ class "col-1-2" ]
                    [ Html.strong [] [ text "Upload Example (with exif orientation compatability)" ]
                    , Html.pre [ class "language-elm" ]
                        [ Html.code [ class "language-elm" ] [ text uploadCode ] ]
                    , Html.div [ class "actions" ]
                        [ Html.button
                            [ class "file-btn"
                            , onClick UploadButtonClicked
                            ]
                            [ text "Upload" ]
                        , if original == Nothing then
                            text ""

                          else
                            Html.button
                                [ class "upload-result"
                                , onClick (ResultButtonClicked "upload-demo")
                                ]
                                [ text "Result" ]
                        ]
                    ]
                , Html.div [ class "col-1-2" ]
                    [ case original of
                        Just url_ ->
                            Html.div [ class "upload-demo-wrap" ]
                                [ Croppie.croppie
                                    [ viewport { width = 100, height = 100, type_ = Circle }
                                    , enableExif True
                                    ]
                                    [ id "upload-demo"
                                    , onResult ShowCroppedImage
                                    ]
                                ]

                        _ ->
                            Html.div [ class "upload-msg" ] [ text "Upload to a file to start cropping" ]
                    ]
                ]
            ]
        ]


demoHidden : Bool -> Html Msg
demoHidden hidden =
    Html.div [ class "demo-wrap hidden-demo" ]
        [ Html.div [ class "container" ]
            [ Html.div [ class "grid" ]
                [ Html.div [ class "col-1-2" ]
                    [ Html.strong [] [ text "Hidden Example" ]
                    , Html.p [] [ text "When binding a croppie element that isn't visible, i.e., in a modal - you'll need to call bind again on your croppie element, to indicate to croppie that the position has changed and it needs to recalculate its points." ]
                    , Html.pre [ class "language-elm" ]
                        [ Html.code [ class "language-elm" ] [ text hiddenCode ] ]
                    , Html.div [ class "actions" ]
                        [ Html.button
                            [ class "show-hidden"
                            , onClick ToggleButtonClicked
                            ]
                            [ text "Toggle Croppie" ]
                        ]
                    ]
                , Html.div [ class "col-1-2" ]
                    [ Croppie.croppie
                        [ viewport { width = 175, height = 175, type_ = Circle }
                        , boundary { width = 200, height = 200 }
                        ]
                        (id "hidden-demo"
                            :: (if hidden then
                                    [ style "display" "none" ]

                                else
                                    []
                               )
                        )
                    ]
                ]
            ]
        ]


importantNotes : Html Msg
importantNotes =
    Html.section [ class "important-notes" ]
        [ Html.a
            [ id "important-notes"
            , name "important-notes"
            ]
            []
        , Html.div [ class "section-header" ]
            [ Html.h2 [] [ text "Important Notes" ] ]
        , Html.div [ class "container" ]
            [ Html.article []
                [ Html.a
                    [ id "cors"
                    , name "cors"
                    ]
                    []
                , Html.h3 [] [ text "Image Hosting & Cross Origin Errors" ]
                , Html.p []
                    [ text "Croppie uses "
                    , Html.code [ class "language-javascript" ]
                        [ text "canvas.drawImage(...)" ]
                    , text " to manipulate images.  Thus, images must obey the CORS policy.  More info can be found "
                    , Html.a [ href "https://developer.mozilla.org/en-US/docs/Web/HTML/CORS_enabled_image" ] [ text "here" ]
                    , text "."
                    ]
                ]
            ]
        ]


browsers : Html Msg
browsers =
    Html.section [ class "browsers" ]
        [ Html.a
            [ id "browsers"
            , name "browsers"
            ]
            []
        , Html.div [ class "section-header" ] [ Html.h2 [] [ text "Browser Support" ] ]
        , Html.div [ class "container" ]
            [ Html.p [] [ text "Croppie for Elm is supported in the following browsers:" ]
            , Html.ul []
                [ Html.li [] [ text "Firefox 63+" ]
                , Html.li [] [ text "Chrome 54+" ]
                , Html.li [] [ text "Edge 79+" ]
                , Html.li [] [ text "Safari 10.1+" ]
                , Html.li [] [ text "Opera 41+" ]
                , Html.li [] [ text "iOS Safari 10.3+" ]
                , Html.li [] [ text "Chrome for Android" ]
                , Html.li [] [ text "Firefox for Android" ]
                ]
            ]
        ]


who : Html Msg
who =
    Html.section [ class "who" ]
        [ Html.a
            [ id "who"
            , name "who"
            ]
            []
        , Html.div [ class "section-header" ]
            [ Html.h2 [] [ text "Who" ] ]
        , Html.div [ class "container" ]
            [ Html.p []
                [ Html.i []
                    [ text "Croppie was built by "
                    , Html.a [ href "http://www.foliotek.com" ] [ text "Foliotek" ]
                    , text " for use in FolioTek Presentation."
                    ]
                ]
            , Html.p [] [ text "And, elm-croppie was built by hallelujahdrive." ]
            , Html.p []
                [ text "Please submit any issue or questions on "
                , Html.a [ href "https://github.com/hallelujahdrive/elm-croppie/issues" ] [ text "Github" ]
                , text "."
                ]
            ]
        ]


footer : Html Msg
footer =
    Html.footer []
        [ text "Copyright ©"
        , Html.span [] [ text "2020" ]
        , text " | hallelujahdrive"
        ]


githubCorner : Html Msg
githubCorner =
    Html.a
        [ href "https://github.com/hallelujahdrive/elm-croppie"
        , class "github-corner"
        , attribute "aria-label" "View source on GitHub"
        ]
        [ Svg.svg
            [ width "80"
            , height "80"
            , viewBox "0 0 250 250"
            , SvgAttrs.style "fill:#151513; color:#fff; position: absolute; top: 0; border: 0; left: 0; transform: scale(-1, 1);"
            ]
            [ Svg.path [ d "M0,0 L115,115 L130,115 L142,142 L250,250 L250,0 Z" ] []
            , Svg.path
                [ d "M128.3,109.0 C113.8,99.7 119.0,89.6 119.0,89.6 C122.0,82.7 120.5,78.6 120.5,78.6 C119.2,72.0 123.4,76.3 123.4,76.3 C127.3,80.9 125.5,87.3 125.5,87.3 C122.9,97.6 130.6,101.9 134.4,103.2"
                , fill "currentColor"
                , SvgAttrs.style "transform-origin: 130px 106px;"
                , SvgAttrs.class "octo-arm"
                ]
                []
            , Svg.path
                [ d "M115.0,115.0 C114.9,115.1 118.7,116.5 119.8,115.4 L133.7,101.6 C136.9,99.2 139.9,98.4 142.2,98.6 C133.8,88.0 127.5,74.4 143.8,58.0 C148.5,53.4 154.0,51.2 159.7,51.0 C160.3,49.4 163.2,43.6 171.4,40.1 C171.4,40.1 176.1,42.5 178.8,56.2 C183.1,58.6 187.2,61.8 190.9,65.4 C194.5,69.0 197.7,73.2 200.1,77.6 C213.8,80.2 216.3,84.9 216.3,84.9 C212.7,93.1 206.9,96.0 205.4,96.6 C205.1,102.4 203.0,107.8 198.3,112.5 C181.9,128.9 168.3,122.5 157.7,114.1 C157.9,116.9 156.7,120.9 152.7,124.9 L141.0,136.5 C139.8,137.7 141.6,141.9 141.8,141.8 Z"
                , fill "currentColor"
                , SvgAttrs.class "octo-body"
                ]
                []
            ]
        ]


simpleCode =
    """import Croppie

view =
   Croppie.croppie [] [ src "images/demo-1.jpg" ]
"""


usageCodeElm =
    """import Croppie

port croppie : Croppie.Data -> Cmd msg

view =
   Croppie.croppie opts [ id "item" ]

update msg model =
   ( model
   , croppie <|
       Croppie.bind "item" bindOpts
   )
"""


usageCodeJs =
    """const app = Elm.Main.init({
   node: document.getElementById("elm"));
});

app.ports.croppie.subscribe((data) => {
   ElmCroppie.port(data);
});
"""


eventsExample =
    """type alias Model =
   { cropData : CropData
   , result : String
   }

type Msg
   = GotCropData CropData
   | GotResult String

view =
   Croppie.croppie
       []
       [ id "events-example"
       , onUpdate GotCropData
       , onGet GotCropData
       , onResult GotResult
       ]
   
update msg model =
   case msg of
       GotCropData cropData ->
           ( { model | cropData = cropData }
           , Cmd.none
           )
       GorResult url ->
           ( { model | result = url }
           , Cmd.none
           )
"""


basicCode =
    """import Croppie
import Croppie.Options exposing (..)
import Croppie.BindOptions exposing (..)

port croppie : Croppie.Data -> Cmd msg

type Msg
   = Bind
   | Result

view =
   Croppie.croppie
       [ viewport
           { width = 150
           , height = 200
           , type_ = Square
           }
       ]
       [ id "demo-basic" ]

update msg model =
   case msg of
       Bind ->
           ( model
           , croppie <|
               Croppie.bind "demo-basic"
                   [ url "images/cat.jpg"
                   , points [ 77, 469, 280, 739]
                   ]
           )
       
       Result ->
           ( model
           , croppie <|
               Croppie.result "demo-basic" []
           )
"""


vanillaCode =
    """import Croppie
import Croppie.Options exposing (..)
import Croppie.BindOptions exposing (..)

port croppie : Croppie.Data -> Cmd msg

type Msg
   = Bind
   | Result

view =
   Croppie.croppie
       [ viewport
           { width = 200
           , height = 100
           , type_ = Square
           }
       , boundary
           { width = 300
           , height = 300
           }
       , showZoomer False
       , enableOrientation True
       ]
       [ id "vanilla-demo" ]

update msg model =
   case msg of
       Bind ->
           ( model
           , croppie <|
               Croppie.bind "vanilla-demo"
                   [ url "images/cat.jpg"
                   , orientation 4
                   ]
           )
       
       Result ->
           ( model
           , croppie <|
               Croppie.result "vanilla-demo" []
           )
"""


resizerCode =
    """import Croppie
import Croppie.Options exposing (..)
import Croppie.BindOptions exposing (..)

port croppie : Croppie.Data -> Cmd msg

type Msg
   = Bind
   | Result

view =
   Croppie.croppie
       [ viewport
           { width = 100
           , height = 100
           , type_ = Square
           }
       , boundary
           { width = 300
           , height = 300
           }
       , showZoomer False
       , enableResize True
       , enableOrientation True
       , mouseWheelZoom Ctrl
       ]
       [ id "resizer-demo" ]

update msg model =
   case msg of
       Bind ->
           ( model
           , croppie <|
               Croppie.bind "resizer-demo"
                   [ url "demo/demo-2.jpg" ]
           )
       
       Result ->
           ( model
           , croppie <|
               Croppie.result "resizer-demo" []
           )
"""


uploadCode =
    """import Croppie
import Croppie.Options exposing (..)

view =
   Croppie.croppie
       [ viewport
           { width = 200
           , height = 200
           , type_ = Circle
           }
       , boundary
           { width = 300
           , height = 300
           }
       ]
"""


hiddenCode =
    """import Croppie

port croppie : Croppie.Data -> Cmd msg

view =
   Croppie.croppie [] [ id "hidden-demo" ]

bind =
   croppie <|
       Croppie.bind "hidden-demo" []

"""
