module DictExt exposing (get, insert, size)

-- Dict does not support keys that are not comparable. But this version does. Uses a list of tuples.


get : k -> List ( k, v ) -> Maybe v
get key tupleList =
    case tupleList of
        [] ->
            Maybe.Nothing

        ( head, value ) :: tail ->
            if head == key then
                Maybe.Just value

            else
                get key tail


size : List a -> Int
size dict =
    List.length dict


insert : ( k, v ) -> List ( k, v ) -> List ( k, v )
insert ( k, v ) tupleList =
    case tupleList of
        [] ->
            [ ( k, v ) ]

        ( head, value ) :: tail ->
            if head == k then
                ( k, v ) :: tail

            else
                ( head, value ) :: insert ( k, v ) tail
