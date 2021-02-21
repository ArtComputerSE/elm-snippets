module Calendar.Calendar exposing (Model, Msg(..), init, initialModel, main)

import Browser
import Element exposing (Element, centerX, column, el, fill, height, layout, none, paddingXY, row, spacingXY, text, width)
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Time


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = Tick Time.Posix


type alias CalendarDate =
    { year : Int, month : Int, day : Int }


type alias Model =
    { currentMonth : String
    }


type alias CalendarEvent =
    { eventTime : Time.Posix
    , description : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    { currentMonth = "Februari" }



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( { model | currentMonth = "Februari (Tick)" }, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    layout [ height fill, Font.size 24 ] <| viewCalendar model


viewCalendar : Model -> Element Msg
viewCalendar model =
    column [ width fill ]
        [ viewMonthSelector model
        , viewMonth model
        ]


viewMonthSelector : Model -> Element Msg
viewMonthSelector model =
    row [ width fill ]
        [ el [ centerX ] <| text model.currentMonth
        ]


viewMonth : Model -> Element Msg
viewMonth _ =
    column [ width fill, Border.width 1, paddingXY 10 10, spacingXY 0 5 ]
        (row [ width fill ] (List.range 1 7 |> List.map viewWeekDay)
            :: (List.range 1 4 |> List.map viewWeek)
        )


viewWeekDay : Int -> Element Msg
viewWeekDay weekDay =
    el
        [ width fill
        , Border.widthEach { left = 1, bottom = 1, top = 0, right = 0 }
        , Border.roundEach { topLeft = 0, bottomLeft = 5, topRight = 0, bottomRight = 0 }
        , paddingXY 5 5
        ]
    <|
        text <|
            case weekDay of
                1 ->
                    "Mån"

                2 ->
                    "Tis"

                3 ->
                    "Ons"

                4 ->
                    "Tor"

                5 ->
                    "Fre"

                6 ->
                    "Lör"

                7 ->
                    "Sön"

                _ ->
                    "XXXX"


viewWeek : Int -> Element Msg
viewWeek week =
    row [ width fill, spacingXY 5 0 ]
        (List.range 1 7 |> List.map (\d -> (week - 1) * 7 + d) |> List.map viewDay)


viewDay : Int -> Element Msg
viewDay day =
    column [ width fill, height fill, Border.width 1, Border.rounded 5, paddingXY 5 5 ]
        [ row [] [ el [] <| text <| String.fromInt day ]
        , if day == 11 then
            row [ width fill, Font.size 12 ]
                [ el
                    [ Font.alignRight
                    , width fill
                    ]
                  <|
                    text <|
                        "Hejsan svejsan"
                ]

          else
            none
        ]



-- Subscription


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every second Tick


second =
    1000
