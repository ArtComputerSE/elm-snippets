
-- Dict does not support keys that are not comparable. But this version does. Uses a list of tuples.

get : k -> List (k, v) -> Maybe v
get key tupleList =
    case tupleList of
        [] ->
            Maybe.Nothing

        ( head, value ) :: tail ->
            if head == key then
                Maybe.Just value
            else
                get key tail
