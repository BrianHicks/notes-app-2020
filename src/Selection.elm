module Selection exposing (Selection, atEnd, atStart, decoder)

import Json.Decode as Decode exposing (Decoder)


type Selection
    = Selection Int Int Int


atStart : Selection -> Bool
atStart (Selection start _ _) =
    start == 0


atEnd : Selection -> Bool
atEnd (Selection _ end length) =
    end == length


decoder : Decoder Selection
decoder =
    Decode.map3 Selection
        (Decode.field "selectionStart" Decode.int)
        (Decode.field "selectionEnd" Decode.int)
        (Decode.field "length" Decode.int)
