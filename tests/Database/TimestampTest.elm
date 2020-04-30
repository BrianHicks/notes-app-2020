module Database.TimestampTest exposing (..)

import Database.Timestamp exposing (..)
import Expect
import Fuzz
import Test exposing (..)


timestampTest : Test
timestampTest =
    describe "Database.Timestamp.Timestamp"
        [ describe "init"
            [ test "accepts values in a reasonable range" <|
                \_ -> Expect.ok (init { millis = 0, counter = 0, node = nodeIdFromInt 0 })
            , test "does not allow counter values higher than 2^16" <|
                \_ ->
                    init { millis = 0, counter = 2 ^ 16 + 1, node = nodeIdFromInt 0 }
                        |> Expect.equal
                            (Err
                                (CounterTooHigh
                                    { got = 2 ^ 16 + 1
                                    , limit = 2 ^ 16
                                    }
                                )
                            )
            ]
        , let
            low =
                0

            high =
                1
          in
          describe "compare"
            [ test "compares first by lowest millis" <|
                \_ ->
                    Result.map2 Database.Timestamp.compare
                        (init { millis = low, counter = high, node = nodeIdFromInt high })
                        (init { millis = high, counter = low, node = nodeIdFromInt low })
                        |> Expect.equal (Ok LT)
            , test "compares second by lowest counter" <|
                \_ ->
                    Result.map2 Database.Timestamp.compare
                        (init { millis = high, counter = low, node = nodeIdFromInt high })
                        (init { millis = high, counter = high, node = nodeIdFromInt low })
                        |> Expect.equal (Ok LT)
            , test "compares third by lowest node ID" <|
                \_ ->
                    Result.map2 Database.Timestamp.compare
                        (init { millis = high, counter = high, node = nodeIdFromInt low })
                        (init { millis = high, counter = high, node = nodeIdFromInt high })
                        |> Expect.equal (Ok LT)
            ]
        , describe "toString"
            [ test "starts with the ISO8601 time" <|
                \_ ->
                    init { millis = 1483205400000, counter = 0, node = nodeIdFromInt 0 }
                        |> Result.map toString
                        |> Expect.equal (Ok "2016-12-31T17:30:00.000Z-0000-0000000000000000")
            , test "has the counter in the second position" <|
                \_ ->
                    init { millis = 0, counter = 0xABCD, node = nodeIdFromInt 0 }
                        |> Result.map toString
                        |> Expect.equal (Ok "1970-01-01T00:00:00.000Z-abcd-0000000000000000")
            , test "has the node ID in the third position" <|
                \_ ->
                    init { millis = 0, counter = 0, node = nodeIdFromInt 0xD00DF00D }
                        |> Result.map toString
                        |> Expect.equal (Ok "1970-01-01T00:00:00.000Z-0000-00000000d00df00d")
            ]
        , describe "fromString"
            [ fuzz3 Fuzz.int (Fuzz.intRange 0 (2 ^ 16 - 1)) Fuzz.int "can always parse the output of toString" <|
                \millisOffset counter node ->
                    let
                        baseDate =
                            1588276543000
                    in
                    case init { millis = baseDate + millisOffset, counter = abs counter, node = nodeIdFromInt node } of
                        Err err ->
                            Expect.fail (problemToString err)

                        Ok timestamp ->
                            timestamp
                                |> toString
                                |> fromString
                                |> Expect.equal (Ok timestamp)
            ]
        ]
