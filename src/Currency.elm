module Currency exposing (Currency(..), Eur, Exchange(..), Sek, add, addDifferent, convert)


type Eur
    = Eur


type Sek
    = Sek


type Usd
    = Usd



-- Demonstrate phantom types by the problem of mixing currencies.
-- The `c` is not used by any of the type's data
-- constructors so `Currency` is called a "phantom" type.


type Currency c
    = Currency Int



-- Note that values like Currency 500 don’t tell you what kind of currency
-- we’re dealing with.
-- The only way to know is by adding a type signature.


price : Currency Eur
price =
    Currency 500



-- Can only add two currencies of the same kind


add : Currency a -> Currency a -> Currency a
add (Currency c1) (Currency c2) =
    Currency (c1 + c2)



-- Exchange is the way to add different currencies


type Exchange from to
    = Exchange Float



-- We don't allow fractions of a cent so we need to round.
-- Note that some money may be lost or gained by this


convert : Exchange from to -> Currency from -> Currency to
convert (Exchange rate) (Currency c) =
    Currency <| round (rate * toFloat c)



-- Add two amounts of different currencies, given the
-- exchange rate.


addDifferent : Exchange a b -> Currency a -> Currency b -> Currency b
addDifferent rate c1 c2 =
    add c2 (convert rate c1)



{- exchangeRates =
   [ ( Eur, Sek, 10.2 ), ( Eur, Usd, 1.1 ) ]
-}


type ExchangeRate c
    = ExchangeRate Float


eurRate : ExchangeRate Eur
eurRate =
    findCurrencyRate "EUR"


usdRate : ExchangeRate Usd
usdRate =
    findCurrencyRate "USD"


adder : ExchangeRate a -> ExchangeRate b -> Currency a -> Currency b -> Currency b
adder (ExchangeRate e1) (ExchangeRate e2) (Currency c1) (Currency c2) =
    let
        i1 =
            e1 * toFloat c1

        i2 =
            e2 * toFloat c2

        sum =
            i1 + i2
    in
    Currency (round (sum / e2))


type alias CurrencyRate =
    { code : String
    , rate : Float
    }


findCurrencyRate : String -> ExchangeRate c
findCurrencyRate codeName =
    let
        maybeRate =
            List.filter (\c -> c.code == codeName) currencies
                |> List.map .rate
                |> List.head
    in
    ExchangeRate (Maybe.withDefault 1.0 maybeRate)


currencies : List CurrencyRate
currencies =
    [ CurrencyRate "SEK" 1.0
    , CurrencyRate "EUR" 10.57
    , CurrencyRate "USD" 9.5
    , CurrencyRate "CHF" 9.9
    , CurrencyRate "INR" 0.13
    ]
