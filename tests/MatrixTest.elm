module MatrixTest exposing (suite)

import Array
import Expect
import Matrix
import Test exposing (Test, describe, test)


threeByThree : Matrix.Matrix String
threeByThree =
    Matrix.initialize 3 3 initFn


tenBy15 : Matrix.Matrix String
tenBy15 =
    Matrix.initialize 10 15 initFn


twoByThree : Matrix.Matrix String
twoByThree =
    Matrix.initialize 2 3 initFn


twoByThreeInt : Matrix.Matrix Int
twoByThreeInt =
    Matrix.initialize 2 3 initFnInt


initFn : Int -> Int -> String
initFn r c =
    "(" ++ String.fromInt r ++ "," ++ String.fromInt c ++ ")"


initFnInt : Int -> Int -> Int
initFnInt r c =
    r * c


suite : Test
suite =
    describe "Matrix suite"
        [ describe "Test Matrix get function"
            [ test "get from empty Matrix will return Nothing" <|
                \() -> Expect.equal Nothing (Matrix.get Matrix.empty 0 0)
            , test "get where the cell exists" <|
                \() -> Expect.equal (Just "(2,1)") (Matrix.get threeByThree 2 1)
            , test "get where the cell does not exists" <|
                \() -> Expect.equal Nothing (Matrix.get threeByThree 3 99)
            ]
        , describe "Test Matrix set function"
            [ test "Insert into empty matrix is empty" <|
                \() -> Expect.equal Matrix.empty (Matrix.set Matrix.empty 0 0 42)
            , test "Insert correctly and get" <|
                \() -> Expect.equal (Just "X") (Matrix.get (Matrix.set threeByThree 1 2 "X") 1 2)
            ]
        , describe "Test Matrix size function"
            [ test "Empty matrix" <|
                \() -> Expect.equal ( 0, 0 ) (Matrix.size Matrix.empty)
            , test "Three by three matrix" <|
                \() -> Expect.equal ( 3, 3 ) (Matrix.size threeByThree)
            , test "Ten by 15 matrix" <|
                \() -> Expect.equal ( 10, 15 ) (Matrix.size tenBy15)
            ]
        , describe "Test to get all the X values"
            [ test "A (2,3) sized" <|
                \() -> Expect.equal [ "(1,0)", "(1,1)", "(1,2)" ] (Array.toList (Matrix.getXs twoByThree 1))
            , test "Of limits is empty" <|
                \() -> Expect.equal Array.empty (Matrix.getXs threeByThree 999)
            ]
        , describe "Test to get all the Y values"
            [ test "A (2,3) sized" <|
                \() -> Expect.equal [ "(0,2)", "(1,2)" ] (Array.toList (Matrix.getYs twoByThree 2))
            , test "Of limits is empty" <|
                \() -> Expect.equal Array.empty (Matrix.getYs threeByThree 999)
            ]
        , describe "Map function"
            [ test "Double contents" <|
                \() -> Expect.equal (Array.fromList [ Array.fromList [ 0, 0, 0 ], Array.fromList [ 0, 2, 4 ] ]) (Matrix.map (\n -> n * 2) twoByThreeInt)
            ]
        , describe "IndexMap function"
            [ test "Contents depending on x, y" <|
                \() -> Expect.equal (Array.fromList [ Array.fromList [ "0(0,0)0", "0(0,1)1", "0(0,2)2" ], Array.fromList [ "1(1,0)0", "1(1,1)1", "1(1,2)2" ] ]) (Matrix.indexMap indexedConvert twoByThree)
            ]
        ]


indexedConvert : Int -> Int -> String -> String
indexedConvert x y value =
    String.fromInt x ++ value ++ String.fromInt y
