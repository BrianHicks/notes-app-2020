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
        ]
