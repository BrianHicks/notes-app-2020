module Database.TimestampTest exposing (..)

import Database.Timestamp exposing (..)
import Expect
import Test exposing (..)


timestampTest : Test
timestampTest =
    describe "Database.Timestamp"
        [ test "I can create a new timestamp generator given a node ID" <|
            \_ -> Expect.pass
        ]
