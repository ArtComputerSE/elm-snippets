module Sudoku.Main exposing (Model, Msg(..), init, main, subscriptions, update, view, viewAll)

import Array exposing (Array)
import Browser
import Element exposing (Element, centerX, centerY, column, el, height, padding, px, rgb, row, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Matrix exposing (Matrix)
import Set exposing (Set)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { grid : Matrix Cell }


type alias Cell =
    { row : Int
    , col : Int
    , options : Set Int
    , conflict : Bool
    }


type Msg
    = ClickedCell Int Int Int
    | PressedResetCell Cell


init : () -> ( Model, Cmd msg )
init _ =
    ( { grid = Matrix.initialize 9 9 initCell
      }
    , Cmd.none
    )


initCell : Int -> Int -> Cell
initCell row col =
    { row = row
    , col = col
    , options = Set.fromList [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]
    , conflict = False
    }


emptyCell : Cell
emptyCell =
    { row = 0
    , col = 0
    , options = Set.empty
    , conflict = False
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedCell x y option ->
            let
                currentCell =
                    Matrix.get model.grid x y |> Maybe.withDefault emptyCell

                newSet =
                    if Set.member option currentCell.options then
                        Set.remove option currentCell.options

                    else
                        Set.insert option currentCell.options

                newCell =
                    { currentCell | options = newSet }
            in
            ( checkGrid { model | grid = Matrix.set model.grid x y newCell }, Cmd.none )

        PressedResetCell cell ->
            ( checkGrid { model | grid = Matrix.set model.grid cell.row cell.col (initCell cell.row cell.col) }, Cmd.none )


type alias SingelCell =
    { row : Int
    , col : Int
    , value : Int
    }


checkGrid : Model -> Model
checkGrid model =
    let
        singles =
            singleCells model.grid

        newGrid =
            Matrix.map (checkCell singles) model.grid
    in
    { model | grid = newGrid }


singleCells : Array (Array Cell) -> List SingelCell
singleCells array =
    Array.map Array.toList array |> Array.toList |> List.concat |> List.filterMap isSingleCell


isSingleCell : Cell -> Maybe SingelCell
isSingleCell cell =
    if Set.size cell.options == 1 then
        Just
            { row = cell.row
            , col = cell.col
            , value = Set.toList cell.options |> List.head |> Maybe.withDefault 0
            }

    else
        Nothing


checkCell : List SingelCell -> Cell -> Cell
checkCell sc cell =
    let
        rowConflict =
            0 < (List.filter (\s -> s.row == cell.row && s.col /= cell.col && Set.member s.value cell.options) sc |> List.length)

        colConflict =
            0 < (List.filter (\s -> s.col == cell.col && s.row /= cell.row && Set.member s.value cell.options) sc |> List.length)

        squareConflict =
            0
                < (List.filter
                    (\s ->
                        inSquare s
                            == inSquare cell
                            && s.col
                            /= cell.col
                            && s.row
                            /= cell.row
                            && Set.member s.value cell.options
                    )
                    sc
                    |> List.length
                  )
    in
    { cell | conflict = rowConflict || colConflict || squareConflict }


inSquare : { a | row : Int, col : Int } -> Int
inSquare { row, col } =
    (row // 3) * 3 + (col // 3)


view : Model -> Html.Html Msg
view model =
    Element.layout [] (viewAll model)


viewAll : Model -> Element Msg
viewAll model =
    viewGrid model.grid


viewGrid : Matrix Cell -> Element Msg
viewGrid grid =
    column []
        [ row [] [ viewSquare grid 0 0, viewSquare grid 0 3, viewSquare grid 0 6 ]
        , row [] [ viewSquare grid 3 0, viewSquare grid 3 3, viewSquare grid 3 6 ]
        , row [] [ viewSquare grid 6 0, viewSquare grid 6 3, viewSquare grid 6 6 ]
        ]


viewSquare grid sr sc =
    let
        getCell r c =
            Matrix.get grid r c |> Maybe.withDefault emptyCell
    in
    column [ Border.width 2, Border.color blue ]
        [ row [] [ viewCell (getCell sr sc), viewCell (getCell sr (sc + 1)), viewCell (getCell sr (sc + 2)) ]
        , row [] [ viewCell (getCell (sr + 1) sc), viewCell (getCell (sr + 1) (sc + 1)), viewCell (getCell (sr + 1) (sc + 2)) ]
        , row [] [ viewCell (getCell (sr + 2) sc), viewCell (getCell (sr + 2) (sc + 1)), viewCell (getCell (sr + 2) (sc + 2)) ]
        ]


viewCell : Cell -> Element Msg
viewCell cell =
    let
        show n =
            Set.member n cell.options

        cellValue =
            Set.toList cell.options |> List.head |> Maybe.withDefault 0 |> String.fromInt
    in
    if isSingleCell cell /= Nothing then
        column
            [ width (px 100)
            , height (px 94)
            , padding 5
            , Border.width 1
            , Border.color grey
            , Font.center
            ]
            [ el [ centerY, centerX, Font.size 28 ] <| text cellValue
            , Input.button [ centerX ]
                { onPress = Just (PressedResetCell cell)
                , label = el [] <| text "X"
                }
            ]

    else
        column
            [ padding 5
            , Border.width 1
            , if cell.conflict then
                Border.color red

              else
                Border.color grey
            ]
            [ row [] [ viewOption cell.row cell.col 1 (show 1), viewOption cell.row cell.col 2 (show 2), viewOption cell.row cell.col 3 (show 3) ]
            , row [] [ viewOption cell.row cell.col 4 (show 4), viewOption cell.row cell.col 5 (show 5), viewOption cell.row cell.col 6 (show 6) ]
            , row [] [ viewOption cell.row cell.col 7 (show 7), viewOption cell.row cell.col 8 (show 8), viewOption cell.row cell.col 9 (show 9) ]
            ]


viewOption : Int -> Int -> Int -> Bool -> Element Msg
viewOption x y value show =
    Input.button []
        { onPress = Just <| ClickedCell x y value
        , label =
            el [ width (px 30), height (px 30), Border.width 1, Border.color grey, Font.center, Font.size 16 ] <|
                text <|
                    if show then
                        String.fromInt value

                    else
                        ""
        }


red =
    rgb 1.0 0 0


blue =
    rgb 0.5 0.5 0.9


grey =
    rgb 0.8 0.8 0.8


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none
