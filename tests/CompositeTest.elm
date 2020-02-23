module CompositeTest exposing (suite, testFunc)

import Composite exposing (Composite(..))
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Composite test"
        [ test "A leaf map to leaf" <|
            \() -> Expect.equal (Leaf "42") (Composite.map testFunc (Leaf 42))
        , test "A Composite with leafs map to Composite" <|
            \() ->
                Expect.equal
                    (Composite
                        { children = [ Leaf "4711", Leaf "1337" ]
                        }
                    )
                    (Composite.map testFunc
                        (Composite
                            { children = [ Leaf 4711, Leaf 1337 ]
                            }
                        )
                    )
        , test "A Composite with composite map to Composite" <|
            \() ->
                Expect.equal
                    (Composite
                        { children =
                            [ Composite
                                { children = [ Leaf "666" ]
                                }
                            , Leaf "1337"
                            ]
                        }
                    )
                    (Composite.map testFunc
                        (Composite
                            { children =
                                [ Composite
                                    { children = [ Leaf 666 ]
                                    }
                                , Leaf 1337
                                ]
                            }
                        )
                    )
        ]


testFunc : Int -> String
testFunc i =
    String.fromInt i
