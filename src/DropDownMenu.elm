module DropDownMenu exposing (main)

import Browser
import Element exposing (Element, below, column, fill, text, width, wrappedRow)
import Element.Border as Border
import Element.Input as Input
import Html


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type alias Model =
    { showDropDownMenu : Bool
    , lastAction : String
    }


type Msg
    = ShowDropDownMenu
    | SelectAction String


init : Model
init =
    Model False ""


update : Msg -> Model -> Model
update msg model =
    case msg of
        ShowDropDownMenu ->
            { model | showDropDownMenu = not model.showDropDownMenu }

        SelectAction action ->
            { model | lastAction = action }


view : Model -> Html.Html Msg
view model =
    Element.layout [] (viewPage model)


viewPage : Model -> Element Msg
viewPage model =
    wrappedRow []
        [ Input.button [ below <| dropDownMenu model.showDropDownMenu ]
            { onPress = Just ShowDropDownMenu
            , label = text <| "Press me"
            }
        ]


dropDownMenu : Bool -> Element Msg
dropDownMenu show =
    if show then
        column [ Border.solid, width fill ]
            [ menu "chose 1"
            , menu "other 2"
            , menu "third 3"
            ]

    else
        text "no"


menu label =
    Input.button [ width fill ]
        { onPress = Just (SelectAction label)
        , label = text label
        }
