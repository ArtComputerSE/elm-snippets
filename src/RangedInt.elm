module RangedInt exposing (RangedInt, create, crude, decoder, valueOf)

{- Integers with range -}

import Json.Decode as Decoder
import String


type RangedInt
    = RangedInt
        { min : Int
        , max : Int
        , value : Int
        }


create : Int -> Int -> Int -> Maybe RangedInt
create min max value =
    if min > value || max < value then
        Nothing

    else
        Just <| RangedInt { min = min, max = max, value = value }


valueOf : Maybe RangedInt -> Int -> Int
valueOf ranged default =
    case ranged of
        Nothing ->
            default

        Just (RangedInt { min, max, value }) ->
            value


decoder : Int -> Int -> Decoder.Decoder RangedInt
decoder min max =
    Decoder.int |> Decoder.andThen (helper min max)


helper : Int -> Int -> Int -> Decoder.Decoder RangedInt
helper min max value =
    if min > value || max < value then
        Decoder.fail <| outOfRangeErrorMessage min max value

    else
        Decoder.map (crude min max) Decoder.int


crude : Int -> Int -> Int -> RangedInt
crude min max value =
    RangedInt { min = min, max = max, value = value }


outOfRangeErrorMessage : Int -> Int -> Int -> String
outOfRangeErrorMessage min max value =
    "Value "
        ++ String.fromInt value
        ++ " outside range ("
        ++ String.fromInt min
        ++ ","
        ++ String.fromInt max
        ++ ")"
