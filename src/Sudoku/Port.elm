port module Sudoku.Port exposing (setStorage)

import Json.Encode as Encode


port setStorage : Encode.Value -> Cmd msg
