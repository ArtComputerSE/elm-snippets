module OrdinalListTest exposing (suite)

import Expect exposing (equal)
import OrdinalList exposing (moveLeft)
import Test exposing (describe, test)


type alias TestType =
    { payLoad : String
    , ordinal : Int
    }


elem1 : TestType
elem1 =
    TestType "elem1" 0


elem2 : TestType
elem2 =
    TestType "elem2" 1


elem3 : TestType
elem3 =
    TestType "elem3" 2


suite : Test.Test
suite =
    describe "move left suite"
        [ test "move left of an empty list is empty" <|
            \() -> equal [] (moveLeft [] 9)
        , test "move left of list of one element changes nothing " <|
            \() -> equal [ TestType "x" 0 ] (moveLeft [ TestType "x" 0 ] 42)
        , test "move left of two list element should swap them." <|
            \() -> equal [ TestType elem2.payLoad 0, TestType elem1.payLoad 1 ] (moveLeft [ elem1, elem2 ] 1)
        , test "move left of three list element should swap them." <|
            \() ->
                equal [ TestType elem1.payLoad 0, TestType elem3.payLoad 1, TestType elem2.payLoad 2 ]
                    (moveLeft [ elem1, elem2, elem3 ] 2)
        , test "move left beyond list length changes nothing " <|
            \() -> equal [ elem1, elem2 ] (moveLeft [ elem1, elem2 ] 3)
        ]
