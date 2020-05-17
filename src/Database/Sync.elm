module Database.Sync exposing (Sync, decoder, encode, isValid, sorter)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (required)
import Json.Encode as Encode
import Sort exposing (Sorter)


type alias Sync =
    { host : String
    , database : String
    , username : String
    , password : String
    }


isValid : Sync -> Bool
isValid { host, database, username, password } =
    (host /= "")
        && (database /= "")
        && (username /= "")
        && (password /= "")


sorter : Sorter Sync
sorter =
    Sort.by .host Sort.alphabetical
        |> Sort.tiebreaker (Sort.by .database Sort.alphabetical)
        |> Sort.tiebreaker (Sort.by .username Sort.alphabetical)
        |> Sort.tiebreaker (Sort.by .password Sort.alphabetical)


decoder : Decoder Sync
decoder =
    Decode.succeed Sync
        |> required "host" Decode.string
        |> required "database" Decode.string
        |> required "username" Decode.string
        |> required "password" Decode.string


encode : Sync -> Encode.Value
encode sync =
    Encode.object
        [ ( "host", Encode.string sync.host )
        , ( "database", Encode.string sync.database )
        , ( "username", Encode.string sync.username )
        , ( "password", Encode.string sync.password )
        ]
