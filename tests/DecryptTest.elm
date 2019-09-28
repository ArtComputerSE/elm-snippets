module DecryptTest exposing (someString, suite)

import Decrypt exposing (decrypt, encrypt)
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Decrypt suite"
        [ test "Given encrypted string then the decrypted is the same" <|
            \() -> Expect.equal someString (decrypt <| encrypt someString)
        ]


someString : String
someString =
    "some string"
