module OrdinalListTest2 exposing (suite)

import Expect exposing (equal)
import PhotoGallery.Photos exposing (moveOrdinal)
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
                \() -> equal [] (moveOrdinal 0 -1 [])
            , test "move left of list of one element changes nothing " <|
                \() -> equal [ TestType "x" 0 ] (moveOrdinal 0 -1 [ TestType "x" 0 ])
            , test "move left of two list element should swap them." <|
                \() ->
                    equal [ TestType elem1.payLoad 1, TestType elem2.payLoad 0 ]
                        (moveOrdinal 1 -1 [ elem1, elem2 ])
            , test "move left of three list element should swap them." <|
                \() ->
                    equal [ TestType elem1.payLoad 0, TestType elem2.payLoad 2, TestType elem3.payLoad 1 ]
                        (moveOrdinal 2 -1 [ elem1, elem2, elem3 ])
            , test "move left beyond list length changes nothing " <|
                \() -> equal [ elem1, elem2 ] (moveOrdinal 3 -1 [ elem1, elem2 ])
            ]
        , describe "move right suite"
            [ test "move right of an empty list is empty" <|
                \() -> equal [] (moveOrdinal 9 1 [])
            , test "move right of list of one element changes nothing " <|
                \() -> equal [ TestType "x" 0 ] (moveOrdinal 42 1 [ TestType "x" 0 ])
            , test "move right of two list element should swap them." <|
                \() ->
                    equal [ TestType elem1.payLoad 1, TestType elem2.payLoad 0 ]
                        (moveOrdinal 0 1 [ elem1, elem2 ])
            , test "move right of three list element should swap them." <|
                \() ->
                    equal [ TestType elem1.payLoad 0, TestType elem2.payLoad 2, TestType elem3.payLoad 1 ]
                        (moveOrdinal 1 1 [ elem1, elem2, elem3 ])
            , test "move right equal to  list length changes nothing " <|
                \() -> equal [ elem1, elem2 ] (moveOrdinal 2 1 [ elem1, elem2 ])
            , test "move right beyond list length changes nothing " <|
                \() -> equal [ elem1, elem2 ] (moveOrdinal 3 1 [ elem1, elem2 ])
            ]
        ]
