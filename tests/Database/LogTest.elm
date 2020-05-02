module Database.LogTest exposing (..)

import Database.LWW as LWW
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
                    |> insert (Time.millisToPosix 0) "row" (SetContent "value")
                    |> Result.map (.state >> Dict.get "row" >> Maybe.andThen .content >> Maybe.map LWW.value)
                    |> Expect.equal (Ok (Just "value"))
        , test "can overwrite a column in a row from a newer log message" <|
            \_ ->
                init (Timestamp.nodeIdFromInt 0)
                    |> insert (Time.millisToPosix 0) "row" (SetContent "a")
                    |> Result.andThen (insert (Time.millisToPosix 1) "row" (SetContent "b"))
                    |> Result.map (.state >> Dict.get "row" >> Maybe.andThen .content >> Maybe.map LWW.value)
                    |> Expect.equal (Ok (Just "b"))
        , test "receiving an older log message does not overwrite a column" <|
            \_ ->
                init (Timestamp.nodeIdFromInt 0)
                    |> insert (Time.millisToPosix 1) "row" (SetContent "a")
                    |> Result.andThen
                        (receive (Time.millisToPosix 1)
                            { timestamp = unwrap (Timestamp.init { millis = 0, counter = 0, node = Timestamp.nodeIdFromInt 1 })
                            , row = "row"
                            , operation = SetContent "b"
                            }
                        )
                    |> Result.map (.state >> Dict.get "row" >> Maybe.andThen .content >> Maybe.map LWW.value)
                    |> Expect.equal (Ok (Just "a"))
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
