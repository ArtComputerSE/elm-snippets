module Parse.ParsePhone exposing (Phone, phoneParser)

-- Demonstrate the parser by parsing phone numbers. Alex Korban.
-- https://korban.net/posts/elm/2018-09-07-introduction-elm-parser/

import Parser exposing (..)



{-
   Example input:
   123 4567 - local
   (04) 123 4567 - with region
   +64 (04) 123 4567 - with country
-}


type alias Phone =
    { countryCode : Maybe Int
    , areaCode : Maybe Int
    , phone : Int
    }


phoneParser : Parser Phone
phoneParser =
    succeed Phone
        |. whitespace
        |= countryCode
        |= areaCode
        |= localNumber


whitespace : Parser ()
whitespace =
    chompWhile (\c -> c == ' ')



{-
   |. means skip
   \= means keep
-}
{-
   To handle an optional country code, we need to provide two parsing alternatives:
   one for when it’s present, and one when it’s not.

   Note that the parsers given to oneOf are tried in order.
-}


countryCode : Parser (Maybe Int)
countryCode =
    oneOf
        [ succeed Just
            |. whitespace
            |. symbol "+"
            |= int
            |. whitespace
        , succeed Nothing
        ]


areaCode : Parser (Maybe Int)
areaCode =
    oneOf
        [ succeed String.toInt
            |. symbol "("
            |. whitespace
            |= (getChompedString <| chompWhile Char.isDigit)
            |. whitespace
            |. symbol ")"
            |. whitespace
        , succeed Nothing
        ]


localNumberStr : Parser String
localNumberStr =
    loop [] localHelp
        |> Parser.map String.concat


localHelp : List String -> Parser (Step (List String) (List String))
localHelp nums =
    let
        checkNum numsSoFar num =
            if String.length num > 0 then
                Loop (num :: numsSoFar)

            else
                Done (List.reverse numsSoFar)
    in
    succeed (checkNum nums)
        |= (getChompedString <| chompWhile Char.isDigit)
        |. whitespace


localNumber : Parser Int
localNumber =
    let
        checkDigits s =
            if String.length s == 7 then
                succeed s

            else
                problem "A NZ phone number has 7 digits"
    in
    localNumberStr
        |> andThen checkDigits
        |> Parser.map String.toInt
        |> andThen
            (\maybe ->
                case maybe of
                    Just n ->
                        succeed n

                    Nothing ->
                        problem "Invalid local number"
            )
