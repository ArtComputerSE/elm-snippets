module AnimatedButton.AnimatedButton exposing (main)

import Animator
import Browser
import Color
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Html exposing (Html)
import Time


type State
    = Default
    | Hover


type alias Id =
    String


type Msg
    = RuntimeTriggeredAnimationStep Time.Posix
    | UserHoveredButton Id
    | UserUnhoveredButton Id


type alias Model =
    { buttonStates : Animator.Timeline (Dict Id State) }


animator : Animator.Animator Model
animator =
    Animator.animator
        |> Animator.watchingWith
            .buttonStates
            (\newButtonStates model ->
                { model | buttonStates = newButtonStates }
            )
            (\buttonStates ->
                List.any ((==) Hover) <| Dict.values buttonStates
            )


init : () -> ( Model, Cmd Msg )
init _ =
    ( { buttonStates =
            Animator.init <|
                Dict.fromList [ ( "Uno", Default ), ( "Dos", Default ), ( "Tres", Default ) ]
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Animator.toSubscription RuntimeTriggeredAnimationStep model animator


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        maybeAlways value =
            Maybe.map (\_ -> value)

        setButtonState id newState =
            Dict.update id (maybeAlways newState) <| Animator.current model.buttonStates
    in
    case msg of
        RuntimeTriggeredAnimationStep newTime ->
            ( Animator.update newTime animator model
            , Cmd.none
            )

        UserHoveredButton id ->
            ( { model
                | buttonStates =
                    Animator.go Animator.slowly (setButtonState id Hover) model.buttonStates
              }
            , Cmd.none
            )

        UserUnhoveredButton id ->
            ( { model
                | buttonStates =
                    Animator.go Animator.slowly (setButtonState id Default) model.buttonStates
              }
            , Cmd.none
            )


buttons : Model -> Element Msg
buttons model =
    let
        buttonState id =
            Maybe.withDefault Default <| Dict.get id <| Animator.current model.buttonStates

        borderColor id =
            fromRgb <|
                Color.toRgba <|
                    if buttonState id == Hover then
                        Color.blue

                    else
                        Color.black

        fontColor id =
            fromRgb <|
                Color.toRgba <|
                    if buttonState id == Hover then
                        Color.white

                    else
                        Color.black

        bgColor id =
            fromRgb <|
                Color.toRgba <|
                    Animator.color model.buttonStates <|
                        \buttonStates ->
                            if (Maybe.withDefault Default <| Dict.get id buttonStates) == Hover then
                                Color.lightBlue

                            else
                                Color.white

        fontSize id =
            round <|
                Animator.linear model.buttonStates <|
                    \buttonStates ->
                        Animator.at <|
                            if (Maybe.withDefault Default <| Dict.get id buttonStates) == Hover then
                                28

                            else
                                20

        button id =
            el
                [ width <| px 200
                , height <| px 60
                , Border.width 3
                , Border.rounded 6
                , Border.color <| borderColor id
                , Background.color <| bgColor id
                , Font.color <| fontColor id
                , Font.size <| fontSize id
                , padding 10
                , onMouseEnter <| UserHoveredButton id
                , onMouseLeave <| UserUnhoveredButton id
                ]
            <|
                (el [ centerX, centerY ] <| text <| "Button " ++ id)
    in
    [ "Uno", "Dos", "Tres" ]
        |> List.map button
        |> column [ spacing 10, centerX, centerY ]


view : Model -> Html Msg
view model =
    layout [ width fill, height fill ] <|
        buttons model


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
