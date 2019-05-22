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
    case decodeString (RangedInt.decoder 1 2) "2" of
        Ok value ->
            value

        Err _ ->
            RangedInt.minimum 1 2
