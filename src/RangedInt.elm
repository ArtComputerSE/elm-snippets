module RangedInt exposing (RangedInt, add, decoder, divide, minimum, multiply, subtract, theMax, theMin, toInt)

{- Integers with range -}

import Json.Decode as Decoder
import String


type RangedInt
    = RangedInt PayLoad


type alias PayLoad =
    { min : Int
    , max : Int
    , value : Int
    }


toInt : RangedInt -> Int
toInt r =
    (payload r).value


theMin : RangedInt -> Int
theMin r =
    (payload r).min


theMax : RangedInt -> Int
theMax r =
    (payload r).max


add : RangedInt -> RangedInt -> RangedInt
add r1 r2 =
    let
        ( p1, p2 ) =
            payloads r1 r2
    in
    construct (p1.min + p2.min) (p1.max + p2.max) (p1.value + p2.value)


multiply : RangedInt -> RangedInt -> RangedInt
multiply r1 r2 =
    let
        ( p1, p2 ) =
            payloads r1 r2
    in
    construct (p1.min * p2.min) (p1.max * p2.max) (p1.value * p2.value)


subtract : RangedInt -> RangedInt -> RangedInt
subtract r1 r2 =
    let
        ( p1, p2 ) =
            payloads r1 r2
    in
    construct (p1.min - p2.max) (p1.max - p2.min) (p1.value - p2.value)


divide : RangedInt -> RangedInt -> RangedInt
divide r1 r2 =
    let
        ( p1, p2 ) =
            payloads r1 r2
    in
    construct (p1.min // p2.max) (p1.max // p2.min) (p1.value // p2.value)


minimum : Int -> Int -> RangedInt
minimum min max =
    construct min max min


decoder : Int -> Int -> Decoder.Decoder RangedInt
decoder min max =
    Decoder.int |> Decoder.andThen (helper min max)


helper : Int -> Int -> Int -> Decoder.Decoder RangedInt
helper min max value =
    if min > value || max < value then
        Decoder.fail <| outOfRangeErrorMessage min max value

    else
        Decoder.map (construct min max) Decoder.int


construct : Int -> Int -> Int -> RangedInt
construct min max value =
    RangedInt { min = min, max = max, value = value }


payload : RangedInt -> PayLoad
payload r =
    case r of
        RangedInt p ->
            p


payloads : RangedInt -> RangedInt -> ( PayLoad, PayLoad )
payloads r1 r2 =
    case ( r1, r2 ) of
        ( RangedInt p1, RangedInt p2 ) ->
            ( p1, p2 )


outOfRangeErrorMessage : Int -> Int -> Int -> String
outOfRangeErrorMessage min max value =
    "Value "
        ++ String.fromInt value
        ++ " outside range ("
        ++ String.fromInt min
        ++ ","
        ++ String.fromInt max
        ++ ")"
