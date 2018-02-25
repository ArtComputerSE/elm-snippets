module MoveValueInList exposing (moveValue)


{-
    Move a value in a list, given the current position and the offset.
-}

moveValue : Int -> Int -> List a -> List a
moveValue fromPos offset list =
    let
        listWithoutMoved =
            List.take fromPos list ++ List.drop (fromPos + 1) list

        moved =
            List.take 1 <| List.drop fromPos list
    in
    List.take (fromPos + offset) listWithoutMoved
        ++ moved
        ++ List.drop (fromPos + offset) listWithoutMoved