module Composite exposing (Composite(..), map)

-- The composite pattern, a tree structure


type Composite a
    = Composite
        { children : List (Composite a)
        }
    | Leaf a


map : (a -> b) -> Composite a -> Composite b
map fn compA =
    case compA of
        Composite { children } ->
            Composite { children = List.map (map fn) children }

        Leaf a ->
            Leaf (fn a)
