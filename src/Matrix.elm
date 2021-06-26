module Matrix exposing
    ( Matrix
    , empty, initialize, repeat
    , size, get, getXs, getYs
    , set
    , map
    , indexMap
    )

{-| Two dimensional matrix backed by Array from the Elm core, the fast immutable array
implementation.


# Matrix

@docs Matrix


# Creation

@docs empty, initialize, repeat


# Query

@docs size, get, getXs, getYs


# Manipulate

@docs set


# Transform

@docs map, indexedMap

-}

import Array exposing (Array)


{-| Representation of immutable, two dimensional matrix. You can create a matrix of integers
(`Matrix Int`) or strings (`Matrix String`) or any other type of value you can dream up.
-}
type alias Matrix a =
    Array (Array a)


{-| Return an empty matrix.

    size empty == ( 0, 0 )

-}
empty : Matrix a
empty =
    Array.empty


{-| Return the size of a matrix in the form of a tuple, (sizeX, sizeY).
-}
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


{-| Initialize a matrix, given desired size and a function for the value of a cell,
given its x and y.
-}
initialize : Int -> Int -> (Int -> Int -> a) -> Matrix a
initialize sizeX sizeY fn =
    Array.initialize sizeX (\col -> Array.initialize sizeY (fn col))


{-| Initialize a matrix, given desired size and the value for every cell.
-}
repeat : Int -> Int -> a -> Matrix a
repeat sizeX sizeY value =
    Array.repeat sizeX (Array.repeat sizeY value)


{-| Set the cell at (x,y) to a new value. If the (x,y) is out of bounds, silently do nothing,
-}
set : Matrix a -> Int -> Int -> a -> Matrix a
set matrix x y v =
    case Array.get x matrix of
        Just aCol ->
            Array.set x (Array.set y v aCol) matrix

        Nothing ->
            matrix


{-| Maybe get the value of the cell at (x,y).
-}
get : Matrix a -> Int -> Int -> Maybe a
get matrix x y =
    getXs matrix x |> Array.get y


{-| Get all values along a given x as an array. If x is out of bounds, return an empty array.
-}
getXs : Matrix a -> Int -> Array a
getXs matrix x =
    Maybe.withDefault Array.empty (Array.get x matrix)


{-| Get all values along a given y as an array. If y is out of bounds, return an empty array.
-}
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


{-| Apply a function on every element in a matrix.

    Matrix.map (\n -> n * 2) [ [ 0, 0, 0 ], [ 0, 1, 2 ] ] => [ [ 0, 0, 0 ], [ 0, 2, 4 ] ]

-}
map : (a -> b) -> Matrix a -> Matrix b
map function matrix =
    matrix |> Array.map (Array.map function)


{-| Apply a function on every element with its x and y as first arguments.

    Matrix.indexedMap (\x y _ -> (String.fromInt x + "," + String.fromInt y) [["","",""]["","",""]]
    => [[ "0,0", "0,1", "0,2" ],[ "1,0", "1,1", "1,2" ] ]

-}
indexMap : (Int -> Int -> a -> b) -> Matrix a -> Matrix b
indexMap function matrix =
    matrix |> Array.indexedMap (\x array -> Array.indexedMap (function x) array)
