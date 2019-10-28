module CurrencyTest exposing (eur, suite)

import Currency exposing (Exchange)
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Currency suite"
        [ test "The value of two euros together." <|
            \() -> Expect.equal (Currency.add (eur 10) (eur 15)) (eur 25)
        , test "The value of two sek together." <|
            \() -> Expect.equal (Currency.add (sek 10) (sek 15)) (sek 25)
        , test "Convert eur to sek with a ratio of 10." <|
            \() -> Expect.equal (Currency.convert eur10Is100Sek (eur 456)) (sek 4560)
        ]



{-
   Does not compile

   x =
       Currency.add (eur 5) (sek 5)
-}


eur : Int -> Currency.Currency Currency.Eur
eur n =
    Currency.Currency n


sek : Int -> Currency.Currency Currency.Sek
sek n =
    Currency.Currency n


eur10Is100Sek : Currency.Exchange Currency.Eur Currency.Sek
eur10Is100Sek =
    Currency.Exchange 10.0
