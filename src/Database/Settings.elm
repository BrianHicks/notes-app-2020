module Database.Settings exposing (Settings, decoder, encoder, init)

import Database.Sync as Sync exposing (Sync)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (required)
import Json.Encode as Encode


type alias Settings =
    { revision : Maybe String
    , syncs : List Sync
    }


init : Settings
init =
    { revision = Nothing
    , syncs = []
    }


decoder : Decoder Settings
decoder =
    Decode.succeed Settings
        |> required "_rev" (Decode.map Just Decode.string)
        |> required "syncs" (Decode.list Sync.decoder)


encoder : Settings -> Encode.Value
encoder settings =
    Encode.object
        [ ( "_id", Encode.string "_local/settings" )
        , ( "_rev"
          , case settings.revision of
                Just revision ->
                    Encode.string revision

                Nothing ->
                    Encode.null
          )
        , ( "syncs", Encode.list Sync.encode settings.syncs )
        ]
