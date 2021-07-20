module CropImageTest exposing (currentOffset, moveEnd, moveStart, suite, zoomedWidth)

import CropImage.ElmUIExample exposing (clampOffset)
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Test clamp offset"
        [ test "offset x above zero becomes zero" <|
            \() -> Expect.equal ( 0.0, 0.0 ) (clampOffset zoomedWidth moveStart moveEnd currentOffset)
        , test "offset x below zero stays below" <|
            \() -> Expect.equal ( -10.0, 0.0 ) (clampOffset zoomedWidth ( 30, 0 ) ( 20, 0 ) currentOffset)
        , test "when zoomed width is not enough then adjust" <|
            \() -> Expect.equal ( -5.0, 0.0 ) (clampOffset 305 ( 30, 0 ) ( 20, 0 ) currentOffset)
        ]


zoomedWidth =
    400


moveStart =
    ( 10, 0 )


moveEnd =
    ( 20, 0 )


currentOffset =
    ( 0, 0 )
