module Currency exposing (Currency(..), Eur, Exchange(..), Sek, add, addDifferent, convert)

-- Demonstrate phantom types by the problem of mixing currencies.


type Currency c
    = Currency Int


type Eur
    = Eur


type Sek
    = Sek



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
