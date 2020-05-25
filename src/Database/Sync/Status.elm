module Database.Sync.Status exposing (Status(..), decoder)

import Json.Decode as Decode exposing (Decoder)


type Status
    = Active
    | Idle
    | Error String


decoder : Decoder Status
decoder =
    Decode.andThen
        (\event ->
            case event of
                "active" ->
                    Decode.succeed Active

                "paused" ->
                    Decode.succeed Idle

                "error" ->
                    Decode.map Error (Decode.field "message" Decode.string)

                "complete" ->
                    Decode.succeed Idle

                _ ->
                    Decode.fail ("I don't know how to deal with a '" ++ event ++ "' event")
        )
        (Decode.field "event" Decode.string)
