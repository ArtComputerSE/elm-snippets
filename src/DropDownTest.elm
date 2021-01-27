module DropDownTest exposing (Model, Msg(..), dropdownConfig, init, main, options, subscriptions, update, view)

import Browser
import DropDownWithInit as DropDown
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { dropdownState : DropDown.State Sex
    , selectedOption : Maybe Sex
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { dropdownState = DropDown.init "dropdown" [ NonBinary ]
      , selectedOption = Just NonBinary
      }
    , Cmd.none
    )


options : List Sex
options =
    [ Male, Female, NonBinary ]


sexToStr : Sex -> String
sexToStr sex =
    case sex of
        Male ->
            "Man"

        Female ->
            "Kvinna"

        NonBinary ->
            "Icke-binär"



-- UPDATE


type Msg
    = OptionPicked (Maybe Sex)
    | DropdownMsg (DropDown.Msg Sex)


type Sex
    = Male
    | Female
    | NonBinary


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OptionPicked option ->
            ( { model | selectedOption = option }, Cmd.none )

        DropdownMsg subMsg ->
            let
                ( state, cmd ) =
                    DropDown.update dropdownConfig subMsg model.dropdownState options
            in
            ( { model | dropdownState = state }, cmd )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    DropDown.view dropdownConfig model.dropdownState options
        |> el []
        |> layout []


dropdownConfig : DropDown.Config Sex Msg
dropdownConfig =
    let
        itemToPrompt : Sex -> Element msg
        itemToPrompt item =
            el [ Font.size 16 ] <| text <| sexToStr item

        itemToElement : Bool -> Bool -> Sex -> Element msg
        itemToElement selected highlighted item =
            let
                bgColor =
                    if highlighted then
                        rgb255 248 132 132

                    else
                        rgb255 255 255 255
            in
            el
                [ Background.color bgColor
                , padding 8
                , spacing 10
                , width fill
                ]
                (if selected then
                    text <| "> " ++ sexToStr item

                 else
                    text <| sexToStr item
                )

        selectAttrs =
            [ Border.width 1, Border.rounded 5, paddingXY 16 8, spacing 10, width fill ]
    in
    DropDown.basic DropdownMsg OptionPicked itemToPrompt itemToElement
        |> DropDown.withPromptElement (el [ Font.size 16, alignRight ] <| text <| "Välj")
        |> DropDown.withOpenCloseButtons
            { openButton = el [ paddingXY 3 3, Font.color <| rgb255 109 109 236 ] <| text "▼"
            , closeButton = el [ paddingXY 3 3 ] <| text "▲"
            }
        |> DropDown.withListAttributes [ Border.width 1 ]
        |> DropDown.withContainerAttributes []
        |> DropDown.withSelectAttributes selectAttrs
