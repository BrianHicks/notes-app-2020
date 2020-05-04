module Database.ID exposing (ID, decoder, encode, fromInt, fromString, generator, sorter, toString)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Random exposing (Generator)
import Sort exposing (Sorter)
import UUID exposing (UUID)


type ID
    = ID UUID


fromInt : Int -> ID
fromInt seed =
    Random.initialSeed seed
        |> Random.step (Random.map ID UUID.generator)
        |> Tuple.first


fromString : String -> Result UUID.Error ID
fromString string =
    Result.map ID (UUID.fromString string)


toString : ID -> String
toString (ID id) =
    UUID.toString id


sorter : Sorter ID
sorter =
    Sort.by (\(ID id) -> UUID.toString id) Sort.alphabetical


generator : Generator ID
generator =
    Random.map ID UUID.generator


encode : ID -> Encode.Value
encode (ID id) =
    Encode.string (UUID.toString id)


decoder : Decoder ID
decoder =
    Decode.andThen
        (\string ->
            case UUID.fromString string of
                Ok uuid ->
                    Decode.succeed (ID uuid)

                Err UUID.WrongFormat ->
                    Decode.fail "wrong format"

                Err UUID.WrongLength ->
                    Decode.fail "wrong length"

                Err UUID.UnsupportedVariant ->
                    Decode.fail "unsupported variant"

                Err UUID.IsNil ->
                    Decode.fail "UUID is nil"

                Err UUID.NoVersion ->
                    Decode.fail "no version"
        )
        Decode.string
