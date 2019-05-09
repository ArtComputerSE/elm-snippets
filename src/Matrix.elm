module Matrix exposing (Matrix, empty, get, getXs, getYs, initialize, set, size)

import Array exposing (Array)



{-
   Implement a two dimensional matrix using arrays.
-}


type alias Matrix a =
    Array (Array a)


empty : Matrix a
empty =
    Array.empty


size : Matrix a -> ( Int, Int )
size matrix =
    let
        sizeX =
            Array.length matrix

        sizeY =
            case Array.get 0 matrix of
                Just aCol ->
                    Array.length aCol

                Nothing ->
                    0
    in
    ( sizeX, sizeY )


initialize : Int -> Int -> (Int -> Int -> a) -> Matrix a
initialize sizeX sizeY fn =
    Array.initialize sizeX (\col -> Array.initialize sizeY (fn col))


set : Matrix a -> Int -> Int -> a -> Matrix a
set matrix x y v =
    case Array.get x matrix of
        Just aCol ->
            Array.set x (Array.set y v aCol) matrix

        Nothing ->
            matrix


get : Matrix a -> Int -> Int -> Maybe a
get matrix x y =
    getXs matrix x |> Array.get y


getXs : Matrix a -> Int -> Array a
getXs matrix x =
    Maybe.withDefault Array.empty (Array.get x matrix)


getYs : Matrix a -> Int -> Array a
getYs matrix y =
    Array.toList matrix |> pickY y |> Array.fromList


pickY : Int -> List (Array a) -> List a
pickY y arrays =
    case List.head arrays of
        Nothing ->
            []

        Just array0 ->
            case Array.get y array0 of
                Nothing ->
                    []

                Just v ->
                    case List.tail arrays of
                        Nothing ->
                            []

                        Just tail ->
                            v :: pickY y tail
