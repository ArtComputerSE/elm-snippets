module RangedIntTest exposing (suite)

import Expect
import Json.Decode exposing (Decoder, Error(..), decodeString)
import Json.Encode
import RangedInt exposing (RangedInt)
import Test exposing (Test, concat, describe, test)


suite : Test
suite =
    concat
        [ describe "Decoding"
            [ test "Simple" <|
                \() -> Expect.equal 4711 (decodeRangedInt "4711")
            , test "Failing" <|
                \() -> decodeString testDecoder1000To9999 "76" |> Expect.equal (failed 76)
            ]
        , describe "Adding"
            [ test "Adding two ranged ints" <|
                \() -> Expect.equal 4 (RangedInt.toInt (RangedInt.add ranged2 ranged2))
            , test "Adding creates new minimum" <|
                \() -> Expect.equal 6 (RangedInt.theMin (RangedInt.add (RangedInt.minimum 1 10) (RangedInt.minimum 5 20)))
            , test "Adding creates new maximum" <|
                \() -> Expect.equal 30 (RangedInt.theMax (RangedInt.add (RangedInt.minimum 1 10) (RangedInt.minimum 5 20)))
            ]
        , describe "Multiplying"
            [ test "Multiplying twp ranged ints" <|
                \() -> Expect.equal 42 (RangedInt.toInt (RangedInt.multiply (RangedInt.minimum 6 7) (RangedInt.minimum 7 8)))
            , test "Multiplying creates new minimum" <|
                \() -> Expect.equal (1 * 5) (RangedInt.theMin (RangedInt.multiply (RangedInt.minimum 1 10) (RangedInt.minimum 5 20)))
            , test "Multiplying creates new maximum" <|
                \() -> Expect.equal (10 * 20) (RangedInt.theMax (RangedInt.multiply (RangedInt.minimum 1 10) (RangedInt.minimum 5 20)))
            ]
        , describe "Subtracting"
            [ test "Subtracting twp ranged ints" <|
                \() -> Expect.equal -1 (RangedInt.toInt (RangedInt.subtract (RangedInt.minimum 6 7) (RangedInt.minimum 7 8)))
            , test "Subtracting creates new minimum" <|
                \() -> Expect.equal (1 - 20) (RangedInt.theMin (RangedInt.subtract (RangedInt.minimum 1 10) (RangedInt.minimum 5 20)))
            , test "Subtracting creates new maximum" <|
                \() -> Expect.equal (10 - 5) (RangedInt.theMax (RangedInt.subtract (RangedInt.minimum 1 10) (RangedInt.minimum 5 20)))
            ]
        , describe "Dividing"
            [ test "Dividing twp ranged ints" <|
                \() -> Expect.equal 2 (RangedInt.toInt (RangedInt.divide (RangedInt.minimum 10 11) (RangedInt.minimum 5 6)))
            , test "Dividing creates new minimum" <|
                \() -> Expect.equal (12 // 4) (RangedInt.theMin (RangedInt.divide (RangedInt.minimum 12 13) (RangedInt.minimum 2 4)))
            , test "Dividing creates new maximum" <|
                \() -> Expect.equal (20 // 2) (RangedInt.theMax (RangedInt.divide (RangedInt.minimum 10 20) (RangedInt.minimum 2 5)))
            ]
        ]



{-
   This is actually a test that the compiler does not allow you to breach in.

   breach : RangedInt
   breach =
       RangedInt { min = 1, max = 6, value = 12 }
-}


decodeRangedInt testString =
    case decodeString testDecoder1000To9999 testString of
        Ok value ->
            RangedInt.toInt value

        Err _ ->
            64


testDecoder1000To9999 : Decoder RangedInt
testDecoder1000To9999 =
    RangedInt.decoder 1000 9999


failed : Int -> Result Json.Decode.Error RangedInt
failed val =
    Err (Json.Decode.Failure ("Value " ++ String.fromInt val ++ " outside range (1000,9999)") (Json.Encode.int val))


ranged2 : RangedInt
ranged2 =
    RangedInt.minimum 2 2
