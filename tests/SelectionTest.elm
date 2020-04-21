module SelectionTest exposing (..)

import Expect
import Fuzz
import Json.Decode as Decode
import Json.Encode as Encode
import Selection exposing (..)
import Test exposing (..)


atStartTest : Test
atStartTest =
    describe "atStart"
        [ fuzz2 (Fuzz.intRange 0 10) (Fuzz.intRange 0 10) "is always True at the beginning of the selection" <|
            \end length ->
                selection { start = 0, end = end, length = length }
                    |> atStart
                    |> Expect.equal True
        , fuzz3 (Fuzz.intRange 1 10) (Fuzz.intRange 0 10) (Fuzz.intRange 0 10) "is never True after the beginning of the selection" <|
            \start end length ->
                selection { start = start, end = end, length = length }
                    |> atStart
                    |> Expect.equal False
        ]


atEndTest : Test
atEndTest =
    describe "atEnd"
        [ fuzz2 (Fuzz.intRange 0 10) (Fuzz.intRange 0 10) "is always True at the end of the selection" <|
            \start endAndLength ->
                selection { start = start, end = endAndLength, length = endAndLength }
                    |> atEnd
                    |> Expect.equal True
        , fuzz2 (Fuzz.intRange 1 10) (Fuzz.intRange 0 10) "is never True before the end of the selection" <|
            \start end ->
                selection { start = start, end = end, length = end + 1 }
                    |> atEnd
                    |> Expect.equal False
        ]


selection : { start : Int, end : Int, length : Int } -> Selection
selection { start, end, length } =
    case
        Decode.decodeValue decoder
            (Encode.object
                [ ( "selectionStart", Encode.int start )
                , ( "selectionEnd", Encode.int end )
                , ( "length", Encode.int length )
                ]
            )
    of
        Ok sel ->
            sel

        Err err ->
            Debug.todo (Decode.errorToString err)
