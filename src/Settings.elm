module Settings exposing
    ( Settings, decoder, encode, init
    , insertSync, removeSync, updateRevision
    , id, idDecoder
    )

{-|

@docs Settings, decoder, encode, init

@docs insertSync, removeSync, updateRevision

@docs id, idDecoder

-}

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


idDecoder : Decoder ()
idDecoder =
    Decode.andThen
        (\idString ->
            if idString == id then
                Decode.succeed ()

            else
                Decode.fail ("Bad ID for settings: " ++ idString)
        )
        Decode.string


encode : Settings -> Encode.Value
encode settings =
    Encode.object
        [ ( "_id", Encode.string id )
        , ( "_rev"
          , case settings.revision of
                Just revision ->
                    Encode.string revision

                Nothing ->
                    Encode.null
          )
        , ( "syncs", Encode.list Sync.encode settings.syncs )
        ]


insertSync : Sync -> Settings -> Settings
insertSync sync settings =
    { settings | syncs = sync :: settings.syncs }


removeSync : Sync -> Settings -> Settings
removeSync sync settings =
    { settings | syncs = List.filter ((/=) sync) settings.syncs }


updateRevision : String -> Settings -> Settings
updateRevision revision settings =
    { settings | revision = Just revision }


id : String
id =
    "_local/settings"
