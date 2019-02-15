module OrdinalListTest exposing (suite)

import Expect exposing (equal)
import OrdinalList exposing (moveLeft, moveRight)
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


broken1 : TestType
broken1 =
    TestType "broken1" 0


broken2 : TestType
broken2 =
    TestType "broken2" 0


broken3 : TestType
broken3 =
    TestType "broken3" 0


suite : Test.Test
suite =
    describe "Ordinal list test suite."
        [ describe "move left suite"
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
        , describe "move right suite"
            [ test "move right of an empty list is empty" <|
                \() -> equal [] (moveRight [] 9)
            , test "move right of list of one element changes nothing " <|
                \() -> equal [ TestType "x" 0 ] (moveRight [ TestType "x" 0 ] 42)
            , test "move right of two list element should swap them." <|
                \() ->
                    equal [ TestType elem2.payLoad 0, TestType elem1.payLoad 1 ]
                        (moveRight [ elem1, elem2 ] 0)
            , test "move right of three list element should swap them." <|
                \() ->
                    equal [ TestType elem1.payLoad 0, TestType elem3.payLoad 1, TestType elem2.payLoad 2 ]
                        (moveRight [ elem1, elem2, elem3 ] 1)
            , test "move right equal to  list length changes nothing " <|
                \() -> equal [ elem1, elem2 ] (moveRight [ elem1, elem2 ] 2)
            , test "move right beyond list length changes nothing " <|
                \() -> equal [ elem1, elem2 ] (moveRight [ elem1, elem2 ] 3)
            ]
        , describe "self repairing suite, list with all ordinal zero"
            [ test "repair when moving first element right" <|
                \() ->
                    equal [ TestType broken2.payLoad 0, TestType broken1.payLoad 1 ]
                        (moveRight [ broken1, broken2 ] 0)
            , test "repair when moving second element left" <|
                \() ->
                    equal [ TestType broken2.payLoad 0, TestType broken1.payLoad 1 ]
                        (moveLeft [ broken1, broken2 ] 1)
            , test "repair when moving third element left" <|
                \() ->
                    equal [ TestType broken1.payLoad 0, TestType broken3.payLoad 1, TestType broken2.payLoad 2 ]
                        (moveLeft [ broken1, broken2, broken3 ] 2)
            ]
        ]
