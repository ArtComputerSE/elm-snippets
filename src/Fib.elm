module Fib exposing (fib, fibSequence)

-- The Fibonacci value for a given position in the sequence.


fib : Int -> Int
fib position =
    case position of
        0 ->
            0

        1 ->
            1

        _ ->
            fib (position - 1) + fib (position - 2)



-- A list of Fibonacci numbers of a given length, starting from zero.


fibSequence : Int -> List Int
fibSequence length =
    if length <= 1 then
        [ 0 ]

    else
        fibSequence (length - 1) ++ [ fib (length - 1) ]
