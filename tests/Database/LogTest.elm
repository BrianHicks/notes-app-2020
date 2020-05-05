module Database.LogTest exposing (..)

import Database.ID as ID
import Database.LWW as LWW
import Database.Log exposing (..)
import Database.Timestamp as Timestamp
import Dict
import Expect
import Fuzz exposing (Fuzzer)
import Json.Decode
import Json.Encode as Encode
import Node.Content as Content exposing (Content)
import Random
import Test exposing (..)
import Time exposing (Posix)


logTest : Test
logTest =
    describe "Database.Log"
        [ describe "setting content"
            [ test "can set content from log messages" <|
                \_ ->
                    case newNode (Time.millisToPosix 0) (plain "value") empty of
                        Ok ( id, log, _ ) ->
                            get id log
                                |> Maybe.andThen .content
                                |> Maybe.map LWW.value
                                |> Expect.equal (Just (plain "value"))

                        Err err ->
                            Expect.fail (Debug.toString err)
            , test "can overwrite content from a later log message" <|
                \_ ->
                    case newNode (Time.millisToPosix 0) (plain "a") empty of
                        Ok ( id, log, _ ) ->
                            log
                                |> edit (Time.millisToPosix 0) id (plain "b")
                                |> Result.map
                                    (Tuple.first
                                        >> get id
                                        >> Maybe.andThen .content
                                        >> Maybe.map LWW.value
                                    )
                                |> Expect.equal (Ok (Just (plain "b")))

                        Err err ->
                            Expect.fail (Debug.toString err)
            , test "receiving an older log message does not overwrite content" <|
                \_ ->
                    case newNode (Time.millisToPosix 1) (plain "a") empty of
                        Ok ( id, log, _ ) ->
                            log
                                |> receive (Time.millisToPosix 1)
                                    { timestamp = unwrap (Timestamp.init { millis = 0, counter = 0, node = Timestamp.nodeIdFromInt 1 })
                                    , id = ID.fromInt 1
                                    , operation = SetContent (plain "b")
                                    }
                                |> Result.map
                                    (get id
                                        >> Maybe.andThen .content
                                        >> Maybe.map LWW.value
                                    )
                                |> Expect.equal (Ok (Just (plain "a")))

                        Err err ->
                            Expect.fail (Debug.toString err)
            ]
        , describe "serialization"
            [ fuzz eventFuzzer "encode -> decode is symmetrical" <|
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


eventFuzzer : Fuzzer Event
eventFuzzer =
    Fuzz.map3
        (\timestamp id operation ->
            { timestamp = timestamp
            , id = id
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
        (Fuzz.map ID.fromInt Fuzz.int)
        operationFuzzer


empty : Log
empty =
    init (Random.initialSeed 0) (Timestamp.nodeIdFromInt 0)


operationFuzzer : Fuzzer Operation
operationFuzzer =
    Fuzz.map (plain >> SetContent) Fuzz.string


plain : String -> Content
plain =
    Content.text >> List.singleton >> Content.fromList
