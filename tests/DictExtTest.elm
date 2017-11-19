module DictExtTest exposing (..)

import DictExt
import Test exposing (..)
import Expect exposing (Expectation)

type TestType =
    TestType1
    | TestType2

suite : Test
suite =
    describe "DictExt suite" [
        describe "Test DictExt get function"
            [ test "get from empty list will return Nothing" <|
                  \() -> Expect.equal Nothing (DictExt.get TestType1 [])
              , test "get where the key exists" <|
                \() -> Expect.equal (Just 42)  (DictExt.get TestType1 [(TestType1, 42)])
              , test "get where the key does not exists" <|
                \() -> Expect.equal Nothing  (DictExt.get TestType2 [(TestType1, 42)])

            ]
        ,describe "Test DictExt size function"
             [
                 test "size of an empty dictionary is zero" <|
                     \() -> Expect.equal 0 (DictExt.size [])
                 , test "size of a dictionary with a single element" <|
                     \() -> Expect.equal 1 (DictExt.size [(TestType1, "some string")])
             ]
        ,describe "Test DictExt insert function"
            [
                test "Inser into empty dictionary" <|
                    \() -> Expect.equal [(TestType2, "some string")] (DictExt.insert(TestType2, "some string")[] )

                ,test "Inser and replace dictionary" <|
                     \() -> Expect.equal [(TestType2, "new string")] (DictExt.insert(TestType2, "new string")[(TestType2, "some string")] )
                            ]

        ]

