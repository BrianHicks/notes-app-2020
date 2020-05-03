module Database.LogTest exposing (..)

import Database.LWW as LWW
import Database.Log exposing (..)
import Database.Timestamp as Timestamp
import Dict
import Expect
import Fuzz exposing (Fuzzer)
import Json.Decode
import Json.Encode as Encode
import Random
import Test exposing (..)
import Time exposing (Posix)


logTest : Test
logTest =
    describe "Database.Log"
        [ describe "setting content"
            [ test "can set content from log messages" <|
                \_ ->
                    case insert (Time.millisToPosix 0) "value" empty of
                        Ok ( id, log, _ ) ->
                            get id log
                                |> Maybe.andThen .content
                                |> Maybe.map LWW.value
                                |> Expect.equal (Just "value")

                        Err err ->
                            Expect.fail (Debug.toString err)

            -- , test "can overwrite content from a newer log message" <|
            --     \_ ->
            --         empty
            --             |> insert (Time.millisToPosix 0) "row" (SetContent "a")
            --             |> Result.map Tuple.first
            --             |> Result.andThen (insert (Time.millisToPosix 1) "row" (SetContent "b"))
            --             |> Result.map (Tuple.first >> .state >> Dict.get "row" >> Maybe.andThen .content >> Maybe.map LWW.value)
            --             |> Expect.equal (Ok (Just "b"))
            -- , test "receiving an older log message does not overwrite content" <|
            --     \_ ->
            --         empty
            --             |> insert (Time.millisToPosix 1) "row" (SetContent "a")
            --             |> Result.map Tuple.first
            --             |> Result.andThen
            --                 (receive (Time.millisToPosix 1)
            --                     { timestamp = unwrap (Timestamp.init { millis = 0, counter = 0, node = Timestamp.nodeIdFromInt 1 })
            --                     , row = "row"
            --                     , operation = SetContent "b"
            --                     }
            --                 )
            --             |> Result.map (.state >> Dict.get "row" >> Maybe.andThen .content >> Maybe.map LWW.value)
            --             |> Expect.equal (Ok (Just "a"))
            ]
        , describe "serialization"
            [ fuzz entryFuzzer "encode -> decode is symmetrical" <|
                \entry ->
                    encode entry
                        |> Json.Decode.decodeValue decoder
                        |> Expect.equal (Ok entry)
            ]
        ]


generator =
    Timestamp.generator (Timestamp.nodeIdFromInt 0)


unwrap : Result x a -> a
unwrap result =
    case result of
        Ok good ->
            good

        Err err ->
            Debug.todo (Debug.toString err)


entryFuzzer : Fuzzer Entry
entryFuzzer =
    Fuzz.map3
        (\timestamp row operation ->
            { timestamp = timestamp
            , row = row
            , operation = operation
            }
        )
        (Fuzz.map3
            (\millis counter node ->
                unwrap
                    (Timestamp.init
                        { millis = 1588276543000 + millis
                        , counter = counter
                        , node = Timestamp.nodeIdFromInt node
                        }
                    )
            )
            Fuzz.int
            (Fuzz.intRange 0 (2 ^ 16 - 1))
            Fuzz.int
        )
        Fuzz.string
        operationFuzzer


empty : Log
empty =
    init (Random.initialSeed 0) (Timestamp.nodeIdFromInt 0)


operationFuzzer : Fuzzer Operation
operationFuzzer =
    Fuzz.map SetContent Fuzz.string
