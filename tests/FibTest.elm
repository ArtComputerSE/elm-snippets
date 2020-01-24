module FibTest exposing (suite)

import Expect
import Fib exposing (fib, fibSequence)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Fibonacci"
        [ describe "Single"
            [ test "The fibonacci of 0 is 0" <|
                \() -> Expect.equal 0 (fib 0)
            , test "The fibonacci of 1 is 1" <|
                \() -> Expect.equal 1 (fib 1)
            , test "The fibonacci of 4 is 3" <|
                \() -> Expect.equal 3 (fib 4)
            ]
        , describe "Lists"
            [ test "The fibs of length 1" <|
                \() -> Expect.equalLists [ 0 ] (fibSequence 1)
            , test "The fibs of length 2" <|
                \() -> Expect.equalLists [ 0, 1 ] (fibSequence 2)
            , test "The fibs of length 6" <|
                \() -> Expect.equalLists [ 0, 1, 1, 2, 3, 5 ] (fibSequence 6)
            , test "The fibs of length 30" <|
                \() -> Expect.equalLists fib30 (fibSequence 30)
            ]
        ]


fib30 =
    [ 0
    , 1
    , 1
    , 2
    , 3
    , 5
    , 8
    , 13
    , 21
    , 34
    , 55
    , 89
    , 144
    , 233
    , 377
    , 610
    , 987
    , 1597
    , 2584
    , 4181
    , 6765
    , 10946
    , 17711
    , 28657
    , 46368
    , 75025
    , 121393
    , 196418
    , 317811
    , 514229
    ]
