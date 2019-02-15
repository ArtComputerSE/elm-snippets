module OrdinalList exposing (moveLeft, moveRight)


moveLeft : List { a | ordinal : Int } -> Int -> List { a | ordinal : Int }
moveLeft list ordinal =
    case list of
        [] ->
            []

        [ single ] ->
            [ single ]

        a :: b :: tail ->
            if b.ordinal == ordinal then
                { b | ordinal = a.ordinal } :: { a | ordinal = b.ordinal } :: tail

            else
                a :: moveLeft (b :: tail) ordinal


moveRight : List { a | ordinal : Int } -> Int -> List { a | ordinal : Int }
moveRight list ordinal =
    case list of
        [] ->
            []

        [ single ] ->
            [ single ]

        a :: b :: tail ->
            if a.ordinal == ordinal then
                { b | ordinal = a.ordinal } :: { a | ordinal = b.ordinal } :: tail

            else
                a :: moveRight (b :: tail) ordinal
