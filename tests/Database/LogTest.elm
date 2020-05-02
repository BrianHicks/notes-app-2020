module Database.LogTest exposing (..)

import Database.Log exposing (..)
import Database.Timestamp as Timestamp
import Dict
import Expect
import Json.Encode as Encode
import Test exposing (..)
import Time exposing (Posix)


logTest : Test
logTest =
    describe "Database.Log"
        [ test "can set a column in a row from log messages" <|
            \_ ->
                init (Timestamp.nodeIdFromInt 0)
                    |> insert
                        { now = Time.millisToPosix 0
                        , row = "row"
                        , operation = SetContent "value"
                        }
                    |> Result.map .state
                    |> Expect.equal (Ok (Dict.singleton "row" { content = Just "value" }))
        , test "can overwrite a column in a row from a newer log message" <|
            \_ ->
                init (Timestamp.nodeIdFromInt 0)
                    |> insert
                        { now = Time.millisToPosix 0
                        , row = "row"
                        , operation = SetContent "a"
                        }
                    |> Result.andThen
                        (insert
                            { now = Time.millisToPosix 1
                            , row = "row"
                            , operation = SetContent "b"
                            }
                        )
                    |> Result.map .state
                    |> Expect.equal (Ok (Dict.singleton "row" { content = Just "b" }))
        ]


generator =
    Timestamp.generator (Timestamp.nodeIdFromInt 0)
