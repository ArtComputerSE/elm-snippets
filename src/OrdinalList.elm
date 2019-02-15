module OrdinalList exposing (moveLeft, moveRight)


moveLeft : List { a | ordinal : Int } -> Int -> List { a | ordinal : Int }
moveLeft list ordinal =
    moveLeftHelper list ordinal 0


moveLeftHelper : List { a | ordinal : Int } -> Int -> Int -> List { a | ordinal : Int }
moveLeftHelper list ordinal index =
    case list of
        a :: b :: tail ->
            if index + 1 >= ordinal then
                { b | ordinal = index } :: { a | ordinal = index + 1 } :: tail

            else
                a :: moveLeftHelper (b :: tail) ordinal (index + 1)

        _ ->
            list


moveRight : List { a | ordinal : Int } -> Int -> List { a | ordinal : Int }
moveRight list ordinal =
    moveLeft list (ordinal + 1)
