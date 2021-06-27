module GameOfLife.GameOfLife exposing (Cell(..), Model)

import Array
import Browser
import Element exposing (Element, column, el, fill, height, layout, padding, px, rgb, row, spacing, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Matrix exposing (Matrix)
import Time


type alias Model =
    { grid : Matrix Cell
    , running : Bool
    }


type Msg
    = ClickedCell Int Int
    | ClickedNextGeneration
    | ClickedRun
    | ClickedStartPattern
    | Tick Time.Posix


type Cell
    = Empty
    | Filled


init : () -> ( Model, Cmd Msg )
init _ =
    ( { grid = Matrix.initialize 50 50 (\_ _ -> Empty)
      , running = False
      }
    , Cmd.none
    )


nextGeneration : Matrix Cell -> Matrix Cell
nextGeneration grid =
    Matrix.indexedMap (cellDestiny grid) grid



{- Any live cell with two or three neighbours survives
   Any dead cell with three live neighbours becomes a live cell
   Everything else is now a dead cell
-}


cellDestiny : Matrix Cell -> Int -> Int -> Cell -> Cell
cellDestiny grid x y value =
    let
        neighbourCount =
            countNeighbours grid x y
    in
    case value of
        Empty ->
            if neighbourCount == 3 then
                Filled

            else
                Empty

        Filled ->
            if neighbourCount == 2 || neighbourCount == 3 then
                Filled

            else
                Empty


countNeighbours : Matrix Cell -> Int -> Int -> Int
countNeighbours grid x y =
    let
        mayBeCellCount : Maybe Cell -> Int
        mayBeCellCount maybeCell =
            case maybeCell of
                Just Filled ->
                    1

                _ ->
                    0
    in
    Matrix.neighbours grid x y |> Array.map mayBeCellCount |> Array.toList |> List.sum


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        ClickedCell x y ->
            let
                currentValue =
                    Matrix.get model.grid x y |> Maybe.withDefault Empty

                newValue =
                    case currentValue of
                        Empty ->
                            Filled

                        Filled ->
                            Empty
            in
            ( { model | grid = Matrix.set model.grid x y newValue }, Cmd.none )

        ClickedNextGeneration ->
            ( { model | grid = nextGeneration model.grid }, Cmd.none )

        ClickedStartPattern ->
            ( { model | grid = setPattern model.grid }, Cmd.none )

        ClickedRun ->
            ( { model | running = not model.running }, Cmd.none )

        Tick _ ->
            ( { model | grid = nextGeneration model.grid }, Cmd.none )


setPattern : Matrix Cell -> Matrix Cell
setPattern grid =
    List.foldr (\( x, y ) g -> Matrix.set g x y Filled)
        grid
        [ ( 25, 25 ), ( 26, 25 ), ( 27, 25 ), ( 27, 26 ), ( 26, 27 ), ( 30, 25 ), ( 31, 25 ), ( 30, 26 ), ( 32, 26 ), ( 32, 27 ), ( 31, 27 ) ]


view : Model -> Html Msg
view model =
    layout [ height fill, Font.size 12 ] <|
        row [ spacing 10, padding 10 ]
            [ viewGrid model.grid
            , viewControl model.running
            ]


viewGrid : Matrix Cell -> Element Msg
viewGrid grid =
    row []
        (Array.indexedMap viewX grid |> Array.toList)


viewX : Int -> Array.Array Cell -> Element Msg
viewX x array =
    column [] (Array.indexedMap (viewCell x) array |> Array.toList)


viewCell : Int -> Int -> Cell -> Element Msg
viewCell x y cell =
    let
        grey =
            rgb 0.8 0.8 0.8
    in
    Input.button []
        { onPress = Just <| ClickedCell x y
        , label =
            el [ width (px 15), height (px 15), Border.width 1, Border.color grey, Font.center ] <|
                text <|
                    case cell of
                        Empty ->
                            " "

                        Filled ->
                            "X"
        }


viewControl : Bool -> Element Msg
viewControl running =
    column []
        [ Input.button []
            { onPress = Just ClickedStartPattern
            , label = el [ Font.size 24, Border.width 1, Border.rounded 5, padding 5 ] <| text "Load Pattern"
            }
        , Input.button []
            { onPress = Just ClickedNextGeneration
            , label = el [ Font.size 24, Border.width 1, Border.rounded 5, padding 5 ] <| text "Next generation"
            }
        , Input.button []
            { onPress = Just ClickedRun
            , label =
                el [ Font.size 24, Border.width 1, Border.rounded 5, padding 5 ] <|
                    text
                        (if running then
                            "Stop"

                         else
                            "Run"
                        )
            }
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.running then
        Time.every second Tick

    else
        Sub.none


second =
    1000


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
