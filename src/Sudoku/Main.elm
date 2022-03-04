module Sudoku.Main exposing (Model, Msg(..), init, main, subscriptions, update, view, viewAll)

import Array exposing (Array)
import Browser
import Element exposing (Element, centerX, centerY, column, el, fill, height, padding, px, rgb, row, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Matrix exposing (Matrix)
import Set exposing (Set)
import Sudoku.Port exposing (setStorage)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { viewMode : ViewMode
    , grid : Matrix Cell
    }


type ViewMode
    = SetUp
    | Solve


type alias Cell =
    { row : Int
    , col : Int
    , options : Set Int
    , conflict : Bool
    , constant : Bool
    }


type Msg
    = ClickedCell Int Int Int
    | PressedResetCell Cell
    | PressedDone
    | PressedSetUp
    | ChangedSetUpCell Cell String


type alias Flags =
    String


init : Flags -> ( Model, Cmd msg )
init flags =
    ( restoreModel flags
    , Cmd.none
    )


initCell : Int -> Int -> Cell
initCell row col =
    { row = row
    , col = col
    , options = Set.empty
    , conflict = False
    , constant = False
    }


emptyCell : Cell
emptyCell =
    { row = 0
    , col = 0
    , options = Set.empty
    , conflict = False
    , constant = True
    }


resetNonConstant : Matrix Cell -> Matrix Cell
resetNonConstant matrix =
    Matrix.map
        (\c ->
            if c.constant then
                c

            else
                { c | options = Set.empty }
        )
        matrix


initNonConstant : Matrix Cell -> Matrix Cell
initNonConstant matrix =
    Matrix.map
        (\c ->
            if c.constant then
                c

            else
                { c | options = Set.fromList [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ] }
        )
        matrix


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

                newModel =
                    checkGrid { model | grid = Matrix.set model.grid x y newCell }
            in
            ( newModel, saveModel newModel )

        PressedResetCell cell ->
            let
                newModel =
                    checkGrid { model | grid = Matrix.set model.grid cell.row cell.col (initCell cell.row cell.col) }
            in
            ( newModel, saveModel newModel )

        PressedDone ->
            ( checkGrid { model | viewMode = Solve, grid = initNonConstant model.grid }, Cmd.none )

        PressedSetUp ->
            ( { model | viewMode = SetUp, grid = resetNonConstant model.grid }, Cmd.none )

        ChangedSetUpCell cell string ->
            let
                defaultCell =
                    { cell | options = Set.empty, constant = False }

                defaultModel =
                    { model | grid = Matrix.set model.grid cell.row cell.col defaultCell }
            in
            case string |> String.trim |> String.toInt of
                Nothing ->
                    ( defaultModel, Cmd.none )

                Just n ->
                    if n < 0 || n > 9 then
                        ( defaultModel, Cmd.none )

                    else
                        let
                            newCell =
                                { cell | options = Set.fromList [ n ], constant = True }

                            newModel =
                                { model | grid = Matrix.set model.grid cell.row cell.col newCell }
                        in
                        ( newModel, saveModel newModel )


saveModel : Model -> Cmd msg
saveModel model =
    setStorage (encodeModel model)


restoreModel : String -> Model
restoreModel string =
    case Decode.decodeString decodeModel string of
        Ok model ->
            model

        Err _ ->
            { viewMode = SetUp
            , grid = Matrix.initialize 9 9 initCell
            }


encodeModel : Model -> Encode.Value
encodeModel model =
    Encode.object
        [ ( "viewMode", encodeViewMode model.viewMode )
        , ( "grid", encodeGrid model.grid )
        ]


decodeModel : Decode.Decoder Model
decodeModel =
    Decode.succeed Model
        |> required "viewMode" decodeViewMode
        |> required "grid" decodeGrid


encodeViewMode : ViewMode -> Encode.Value
encodeViewMode viewMode =
    case viewMode of
        SetUp ->
            Encode.string "SetUp"

        Solve ->
            Encode.string "Solve"


decodeViewMode : Decode.Decoder ViewMode
decodeViewMode =
    Decode.string
        |> Decode.andThen
            (\s ->
                case s of
                    "SetUp" ->
                        Decode.succeed SetUp

                    _ ->
                        Decode.succeed Solve
            )


encodeGrid : Matrix Cell -> Encode.Value
encodeGrid matrix =
    Encode.array encodeRow matrix


decodeGrid : Decode.Decoder (Matrix Cell)
decodeGrid =
    Decode.array decodeRow


encodeRow : Array Cell -> Encode.Value
encodeRow cells =
    Encode.array encodeCell cells


decodeRow : Decode.Decoder (Array Cell)
decodeRow =
    Decode.array decodeCell


encodeCell : Cell -> Encode.Value
encodeCell cell =
    Encode.object
        [ ( "row", Encode.int cell.row )
        , ( "col", Encode.int cell.col )
        , ( "options", Encode.list Encode.int (Set.toList cell.options) )
        , ( "conflict", Encode.bool cell.conflict )
        , ( "constant", Encode.bool cell.constant )
        ]


decodeCell : Decode.Decoder Cell
decodeCell =
    Decode.succeed Cell
        |> required "row" Decode.int
        |> required "col" Decode.int
        |> required "options" (Decode.list Decode.int |> Decode.map Set.fromList)
        |> required "conflict" Decode.bool
        |> required "constant" Decode.bool


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
    case model.viewMode of
        SetUp ->
            viewSetUp model.grid

        Solve ->
            viewSolve model.grid


viewSetUp : Matrix Cell -> Element Msg
viewSetUp grid =
    column []
        [ row [ width fill, padding 5 ]
            [ el [ centerX ] <| text "Set up"
            , Input.button buttonAttr
                { onPress = Just PressedDone
                , label = text "Done"
                }
            ]
        , row [] [ viewSetUpSquare grid 0 0, viewSetUpSquare grid 0 3, viewSetUpSquare grid 0 6 ]
        , row [] [ viewSetUpSquare grid 3 0, viewSetUpSquare grid 3 3, viewSetUpSquare grid 3 6 ]
        , row [] [ viewSetUpSquare grid 6 0, viewSetUpSquare grid 6 3, viewSetUpSquare grid 6 6 ]
        ]


buttonAttr : List (Element.Attribute msg)
buttonAttr =
    [ Border.rounded 5, Border.color grey, Border.width 1, padding 5, Background.color lightRed ]


viewSetUpSquare : Matrix Cell -> Int -> Int -> Element Msg
viewSetUpSquare grid sr sc =
    let
        getCell r c =
            Matrix.get grid r c |> Maybe.withDefault emptyCell
    in
    column [ Border.width 2, Border.color blue ]
        [ row [] [ viewSetUpCell (getCell sr sc), viewSetUpCell (getCell sr (sc + 1)), viewSetUpCell (getCell sr (sc + 2)) ]
        , row [] [ viewSetUpCell (getCell (sr + 1) sc), viewSetUpCell (getCell (sr + 1) (sc + 1)), viewSetUpCell (getCell (sr + 1) (sc + 2)) ]
        , row [] [ viewSetUpCell (getCell (sr + 2) sc), viewSetUpCell (getCell (sr + 2) (sc + 1)), viewSetUpCell (getCell (sr + 2) (sc + 2)) ]
        ]


viewSetUpCell : Cell -> Element Msg
viewSetUpCell cell =
    let
        label =
            Set.toList cell.options |> List.head |> Maybe.map String.fromInt |> Maybe.withDefault " "
    in
    column
        [ width (px 100)
        , height (px 94)
        , padding 5
        , Border.width 1
        , Border.color grey
        , Font.center
        ]
        [ Input.text []
            { text = label
            , onChange = ChangedSetUpCell cell
            , label = Input.labelHidden "cell value"
            , placeholder = Nothing
            }
        ]


viewSolve : Matrix Cell -> Element Msg
viewSolve grid =
    column []
        [ row [ width fill, padding 5 ]
            [ el [ centerX ] <| text "Solving"
            , Input.button buttonAttr { onPress = Just PressedSetUp, label = text "Set up" }
            ]
        , row [] [ viewSquare grid 0 0, viewSquare grid 0 3, viewSquare grid 0 6 ]
        , row [] [ viewSquare grid 3 0, viewSquare grid 3 3, viewSquare grid 3 6 ]
        , row [] [ viewSquare grid 6 0, viewSquare grid 6 3, viewSquare grid 6 6 ]
        ]


viewSquare : Matrix Cell -> Int -> Int -> Element Msg
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
            [ if cell.constant then
                column [ width fill, height fill, Background.color lightGreen ]
                    [ el [ centerY, centerX, Font.size 28 ] <| text cellValue ]

              else
                column [ width fill, height fill ]
                    [ el [ centerY, centerX, Font.size 28 ] <| text cellValue
                    , Input.button [ centerX ]
                        { onPress = Just (PressedResetCell cell)
                        , label = el [] <| text "X"
                        }
                    ]
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


red : Element.Color
red =
    rgb 1.0 0 0


lightRed : Element.Color
lightRed =
    rgb 0.9 0.7 0.7


blue : Element.Color
blue =
    rgb 0.5 0.5 0.9


grey : Element.Color
grey =
    rgb 0.8 0.8 0.8


lightGreen : Element.Color
lightGreen =
    rgb 0.7 0.9 0.7


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none
