module ParsePhoneTest exposing (suite)

import Expect
import ParsePhone exposing (phoneParser)
import Parser
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Cases from the article"
        [ test "Number with all parts. " <|
            \() -> Expect.equal expectedFullNumber (Parser.run phoneParser actualFullNumber)
        , test "Number with local only. " <|
            \() -> Expect.equal expectedLocalNumber (Parser.run phoneParser actualLocalNumber)
        ]


expectedFullNumber =
    Ok
        { areaCode = Just 4, countryCode = Just 64, phone = 1234567 }


actualFullNumber =
    "  +64  ( 04 )  123 45 67   "


expectedLocalNumber =
    Ok
        { areaCode = Nothing, countryCode = Nothing, phone = 1234567 }


actualLocalNumber =
    "1234567"
