module Database.TimestampTest exposing (..)

import Database.Timestamp exposing (..)
import Expect
import Test exposing (..)


timestampTest : Test
timestampTest =
    describe "Database.Timestamp.Timestamp"
        [ describe "init"
            [ test "accepts values in a reasonable range" <|
                \_ -> Expect.ok (init { millis = 0, counter = 0, node = 0 })
            , test "does not allow counter values higher than 2^16" <|
                \_ ->
                    init { millis = 0, counter = 2 ^ 16 + 1, node = 0 }
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
                        (init { millis = low, counter = high, node = high })
                        (init { millis = high, counter = low, node = low })
                        |> Expect.equal (Ok LT)
            , test "compares second by lowest counter" <|
                \_ ->
                    Result.map2 Database.Timestamp.compare
                        (init { millis = high, counter = low, node = high })
                        (init { millis = high, counter = high, node = low })
                        |> Expect.equal (Ok LT)
            , test "compares third by lowest node ID" <|
                \_ ->
                    Result.map2 Database.Timestamp.compare
                        (init { millis = high, counter = high, node = low })
                        (init { millis = high, counter = high, node = high })
                        |> Expect.equal (Ok LT)
            ]
        ]
