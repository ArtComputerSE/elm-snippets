module Sudoku.Main exposing (Model, Msg(..), init, main, subscriptions, update, view, viewAll)

import Array exposing (Array)
import Browser
import Element exposing (Element, FocusStyle, centerX, centerY, column, el, fill, height, newTabLink, padding, paragraph, px, rgb, row, spacing, text, width)
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
    | Help


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
    | PressedHelp
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
    , options = Set.fromList [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]
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

        PressedHelp ->
            ( { model | viewMode = Help, grid = resetNonConstant model.grid }, Cmd.none )

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

        Help ->
            Encode.string "Help"


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
    Element.layoutWith { options = [ Element.focusStyle noFocusStyle ] } [ centerX, width fill ] (viewAll model)


noFocusStyle : FocusStyle
noFocusStyle =
    { borderColor = Nothing
    , backgroundColor = Nothing
    , shadow = Nothing
    }


viewAll : Model -> Element Msg
viewAll model =
    case model.viewMode of
        SetUp ->
            viewSetUp model.grid

        Solve ->
            viewSolve model.grid

        Help ->
            viewHelp


viewSetUp : Matrix Cell -> Element Msg
viewSetUp grid =
    column [ centerX, width fill ]
        [ row [ centerX, width (px 900), padding 5, Font.family [ Font.serif ] ]
            [ helpButton
            , el [ centerX ] <| text "Set up"
            , Input.button buttonAttr
                { onPress = Just PressedDone
                , label = text "Done"
                }
            ]
        , row [ centerX, width fill ] [ viewSetUpSquare borderTopLeft grid 0 0, viewSetUpSquare borderTopMiddle grid 0 3, viewSetUpSquare borderTopRight grid 0 6 ]
        , row [ centerX, width fill ] [ viewSetUpSquare borderMiddleLeft grid 3 0, viewSetUpSquare borderMiddleMiddle grid 3 3, viewSetUpSquare borderMiddleRight grid 3 6 ]
        , row [ centerX, width fill ] [ viewSetUpSquare borderBottomLeft grid 6 0, viewSetUpSquare borderBottomMiddle grid 6 3, viewSetUpSquare borderMiddleRight grid 6 6 ]
        ]


helpButton : Element Msg
helpButton =
    Input.button []
        { onPress = Just PressedHelp
        , label =
            column
                [ Border.width 1
                , Border.color grey
                , Border.rounded 25
                , Border.shadow { offset = ( 2, 2 ), size = 1, color = blue, blur = 1 }
                , padding 10
                ]
                [ text "?" ]
        }


viewSetUpSquare : BorderConfig -> Matrix Cell -> Int -> Int -> Element Msg
viewSetUpSquare borders grid sr sc =
    let
        getCell r c =
            Matrix.get grid r c |> Maybe.withDefault emptyCell
    in
    column
        [ centerX
        , Border.widthEach borders.width
        , Border.roundEach borders.round
        , Border.color blue
        ]
        [ row [ centerX ] [ viewSetUpCell (getCell sr sc), viewSetUpCell (getCell sr (sc + 1)), viewSetUpCell (getCell sr (sc + 2)) ]
        , row [ centerX ] [ viewSetUpCell (getCell (sr + 1) sc), viewSetUpCell (getCell (sr + 1) (sc + 1)), viewSetUpCell (getCell (sr + 1) (sc + 2)) ]
        , row [ centerX ] [ viewSetUpCell (getCell (sr + 2) sc), viewSetUpCell (getCell (sr + 2) (sc + 1)), viewSetUpCell (getCell (sr + 2) (sc + 2)) ]
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
    column [ centerX, width fill ]
        [ row [ centerX, width (px 900), padding 5, Font.family [ Font.serif ] ]
            [ helpButton
            , el [ centerX ] <| text "Solving"
            , Input.button buttonAttr { onPress = Just PressedSetUp, label = text "Set up" }
            ]
        , row [ centerX, width fill ] [ viewSquare borderTopLeft grid 0 0, viewSquare borderTopMiddle grid 0 3, viewSquare borderTopRight grid 0 6 ]
        , row [ centerX, width fill ] [ viewSquare borderMiddleLeft grid 3 0, viewSquare borderMiddleMiddle grid 3 3, viewSquare borderMiddleRight grid 3 6 ]
        , row [ centerX, width fill ] [ viewSquare borderBottomLeft grid 6 0, viewSquare borderBottomMiddle grid 6 3, viewSquare borderBottomRight grid 6 6 ]
        ]


viewSquare : BorderConfig -> Matrix Cell -> Int -> Int -> Element Msg
viewSquare borders grid sr sc =
    let
        getCell r c =
            Matrix.get grid r c |> Maybe.withDefault emptyCell
    in
    column
        [ centerX
        , Border.widthEach borders.width
        , Border.roundEach borders.round
        , Border.color blue
        , width (px 308)
        , height (px 308)
        ]
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
            [ width (px 101)
            , height (px 101)
            , padding 5
            , Border.width 1
            , if cell.conflict then
                Border.color red

              else
                Border.color grey
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


viewHelp : Element Msg
viewHelp =
    column [ centerX, width (px 900), Font.family [ Font.serif ], spacing 10 ]
        [ row [ centerX, width (px 900), padding 5, spacing 5 ]
            [ Input.button buttonAttr { onPress = Just PressedDone, label = text "Solve" }
            , Input.button buttonAttr { onPress = Just PressedSetUp, label = text "Set up" }
            ]
        , paragraph [ Font.center, width fill, Font.size 24, Font.bold ] [ text "The Sudoku Notebook" ]
        , paragraph [] [ text "Here's an aid to solve Sudoku puzzles. It keeps track of where there are conflicts while you still do the thinking, the whole point of spending time with puzzles." ]
        , paragraph [] [ text "Start in Setup mode and enter your puzzle.  Press Done and you will be in Solve mode. The puzzle values will be in green." ]
        , paragraph [] [ text "All other cells have numbers 1 - 9 in them. A cell with a red border has at least one number that is in conflict with another cell that has a single number." ]
        , paragraph [] [ text "Remove numbers from a cell that cause conflict. The remaining numbers are those which can be in that cell." ]
        , paragraph [] [ text "Example, if a cell has the single value 5, then no other cell in the same row, the same column or same square, may have the value 5. Click on 5 in all the cells in the same row, column and square. Those cells will now have the numbers 1 - 4 and 6 - 9 as options. Keep going until there are no more conflicts. Eventually cells will have a single value and reduce the options for adjacent cells." ]
        , paragraph [] [ text "The notebook is saved in your browser so if you come back here, your last puzzle will be restored. " ]
        , paragraph [] [ text "You can find the source code here: ", newTabLink [ Font.color blue ] { label = text "GitHub", url = "https://github.com/ArtComputerSE/elm-snippets/tree/master/src/Sudoku" } ]
        ]


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


buttonAttr : List (Element.Attribute msg)
buttonAttr =
    [ Border.rounded 5, Border.color grey, Border.width 1, padding 5, Background.color lightRed ]


type alias BorderConfig =
    { width :
        { bottom : Int
        , left : Int
        , right : Int
        , top : Int
        }
    , round :
        { topLeft : Int
        , topRight : Int
        , bottomLeft : Int
        , bottomRight : Int
        }
    }


defaultWidth :
    { bottom : Int
    , left : Int
    , right : Int
    , top : Int
    }
defaultWidth =
    { bottom = 2
    , left = 2
    , right = 2
    , top = 2
    }


defaultRound :
    { topLeft : Int
    , topRight : Int
    , bottomLeft : Int
    , bottomRight : Int
    }
defaultRound =
    { topLeft = 0
    , topRight = 0
    , bottomLeft = 0
    , bottomRight = 0
    }


borderTopLeft : BorderConfig
borderTopLeft =
    { width = { defaultWidth | top = 4, left = 4 }
    , round = { defaultRound | topLeft = 4 }
    }


borderTopMiddle : BorderConfig
borderTopMiddle =
    { width = { defaultWidth | top = 4 }
    , round = defaultRound
    }


borderTopRight : BorderConfig
borderTopRight =
    { width = { defaultWidth | top = 4, right = 4 }
    , round = { defaultRound | topRight = 4 }
    }


borderMiddleLeft : BorderConfig
borderMiddleLeft =
    { width = { defaultWidth | left = 4 }
    , round = defaultRound
    }


borderMiddleMiddle : BorderConfig
borderMiddleMiddle =
    { width = defaultWidth
    , round = defaultRound
    }


borderMiddleRight : BorderConfig
borderMiddleRight =
    { width = { defaultWidth | right = 4 }
    , round = defaultRound
    }


borderBottomLeft : BorderConfig
borderBottomLeft =
    { width = { defaultWidth | left = 4, bottom = 4 }
    , round = { defaultRound | bottomLeft = 4 }
    }


borderBottomMiddle : BorderConfig
borderBottomMiddle =
    { width = { defaultWidth | bottom = 4 }
    , round = defaultRound
    }


borderBottomRight : BorderConfig
borderBottomRight =
    { width = { defaultWidth | right = 4, bottom = 4 }
    , round = { defaultRound | bottomRight = 4 }
    }


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none
