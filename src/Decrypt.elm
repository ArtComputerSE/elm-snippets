module Decrypt exposing (decrypt, encrypt)


decrypt : String -> String
decrypt text =
    crypt decryptChar text


encrypt : String -> String
encrypt text =
    crypt encryptChar text


crypt : (Char -> Char) -> String -> String
crypt cryptFn text =
    String.fromList <| cryptList cryptFn <| String.toList text


cryptList : (Char -> Char) -> List Char -> List Char
cryptList cryptFn list =
    List.map cryptFn list


decryptChar : Char -> Char
decryptChar char =
    Char.toCode char - 1 |> Char.fromCode


encryptChar : Char -> Char
encryptChar char =
    Char.toCode char + 1 |> Char.fromCode
