module RangedIntTest exposing (suite)

import Expect
import Json.Decode exposing (Decoder, Error(..), decodeString)
import Json.Encode
import RangedInt exposing (RangedInt)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "RangedInt suite"
        [ describe "Construct"
            [ test "Construct two identical" <|
                \() -> Expect.equal (RangedInt.create 1 8 6) (RangedInt.create 1 8 6)
            , test "Construct two different value" <|
                \() -> Expect.notEqual (RangedInt.create 1 5 2) (RangedInt.create 1 5 3)
            , test "Construct with illegal value returns Nothing" <|
                \() -> Expect.equal Nothing (RangedInt.create 1 5 6)
            ]
        , describe "Value of"
            [ test "Value of" <|
                \() -> Expect.equal 42 (RangedInt.valueOf (RangedInt.create 1 90 42) -999)
            ]
        , describe "Decoding"
            [ test "Simple" <|
                \() -> decodeString testDecoder testString |> Expect.equal testStringValue
            , test "Failing" <|
                \() -> decodeString testDecoder "76" |> Expect.equal failed
            ]
        ]


testString =
    "4711"


testDecoder : Decoder RangedInt
testDecoder =
    RangedInt.decoder 1000 9999


testStringValue : Result Json.Decode.Error RangedInt
testStringValue =
    Ok (RangedInt.crude 1000 9999 4711)


failed : Result Json.Decode.Error RangedInt
failed =
    Err (Json.Decode.Failure "Value 76 outside range (1000,9999)" (Json.Encode.int 76))
