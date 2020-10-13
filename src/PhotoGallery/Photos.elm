module PhotoGallery.Photos exposing (Model, Msg(..), init, main, moveOrdinal, update, view)

import Browser
import Element exposing (alignRight, centerX, column, el, fill, fillPortion, image, none, row, shrink, text, width, wrappedRow)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type Msg
    = MovePhoto Int Int
    | ChangeMode


type alias Photo =
    { ordinal : Int
    , url : String
    }


type Mode
    = Edit
    | Show


type alias Model =
    { photos : List Photo, mode : Mode }


init : Model
init =
    { photos =
        [ Photo 0 "l1.jpg"
        , Photo 1 "l2.jpg"
        , Photo 2 "l3.jpg"
        , Photo 3 "s1.jpg"
        , Photo 4 "s2.jpg"
        , Photo 5 "s3.jpg"
        ]
    , mode = Edit
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        MovePhoto ordinal delta ->
            { model | photos = moveOrdinal ordinal delta model.photos }

        ChangeMode ->
            { model
                | mode =
                    case model.mode of
                        Show ->
                            Edit

                        Edit ->
                            Show
            }


view : Model -> Html.Html Msg
view model =
    Element.layout []
        (column []
            [ Input.button []
                { onPress = Just ChangeMode
                , label = el [ Font.size 24 ] <| text "Change mode"
                }
            , viewPhotos model
            ]
        )


viewPhotos : Model -> Element.Element Msg
viewPhotos model =
    wrappedRow [] (List.map (viewPhoto model.mode) <| List.sortBy .ordinal model.photos)


viewPhoto : Mode -> Photo -> Element.Element Msg
viewPhoto mode photo =
    column [ width (fillPortion 4), centerX ]
        [ case mode of
            Edit ->
                topRow photo.ordinal

            Show ->
                none
        , image [ centerX ] { src = photo.url, description = "xxx" }
        ]


topRow : Int -> Element.Element Msg
topRow ordinal =
    row [ Border.width 1, width fill ]
        [ Input.button [ width fill ]
            { onPress = Just (MovePhoto ordinal -1)
            , label = el [] <| text "Left"
            }
        , el [ width fill, centerX ] <| text "top"
        , Input.button [ width shrink, alignRight ]
            { onPress = Just (MovePhoto ordinal 1)
            , label = el [] <| text "Right"
            }
        ]



-- reorder list


moveOrdinal : Int -> Int -> List { a | ordinal : Int } -> List { a | ordinal : Int }
moveOrdinal n delta list =
    if (n + delta >= List.length list) || (n + delta < 0) then
        list

    else
        swapOrdinal n (n + delta) list


swapOrdinal : Int -> Int -> List { a | ordinal : Int } -> List { a | ordinal : Int }
swapOrdinal currentNumber newNumber list =
    case list of
        [] ->
            []

        p :: ps ->
            if p.ordinal == currentNumber then
                { p | ordinal = newNumber } :: swapOrdinal currentNumber newNumber ps

            else if p.ordinal == newNumber then
                { p | ordinal = currentNumber } :: swapOrdinal currentNumber newNumber ps

            else
                p :: swapOrdinal currentNumber newNumber ps
