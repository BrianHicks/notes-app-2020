module Database.ID exposing (ID, decoder, encode, fromInt, fromString, generator, sorter, toString)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Random exposing (Generator)
import Sort exposing (Sorter)
import UUID exposing (UUID)


type ID
    = ID String


fromInt : Int -> ID
fromInt seed =
    Random.initialSeed seed
        |> Random.step (Random.map (ID << UUID.toString) UUID.generator)
        |> Tuple.first


fromString : String -> Result UUID.Error ID
fromString string =
    Result.map (ID << UUID.toString) (UUID.fromString string)


toString : ID -> String
toString (ID id) =
    id


sorter : Sorter ID
sorter =
    Sort.by (\(ID id) -> id) Sort.alphabetical


generator : Generator ID
generator =
    Random.map (ID << UUID.toString) UUID.generator


encode : ID -> Encode.Value
encode id =
    Encode.string (toString id)


decoder : Decoder ID
decoder =
    Decode.andThen
        (\string ->
            case fromString string of
                Ok id ->
                    Decode.succeed id

                Err err ->
                    Decode.fail (errToString err)
        )
        Decode.string


errToString : UUID.Error -> String
errToString err =
    case err of
        UUID.WrongFormat ->
            "wrong format"

        UUID.WrongLength ->
            "wrong length"

        UUID.UnsupportedVariant ->
            "unsupported variant"

        UUID.IsNil ->
            "UUID is nil"

        UUID.NoVersion ->
            "no version"
