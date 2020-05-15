module Database.ID exposing (ID, decoder, encode, fromInt, fromString, generator, namespace, sorter, toString)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Random exposing (Generator)
import Sort exposing (Sorter)
import UUID exposing (UUID)


type ID
    = ID (Maybe String) UUID


namespace : ID -> Maybe String
namespace (ID namespace_ _) =
    namespace_


fromInt : Int -> ID
fromInt seed =
    Random.initialSeed seed
        |> Random.step (Random.map (ID Nothing) UUID.generator)
        |> Tuple.first


fromString : String -> Result UUID.Error ID
fromString string =
    case String.split "/" string of
        [ namespace_, maybeID ] ->
            Result.map (ID (Just namespace_)) (UUID.fromString maybeID)

        _ ->
            Result.map (ID Nothing) (UUID.fromString string)


toString : ID -> String
toString (ID maybeNamespace id) =
    case maybeNamespace of
        Nothing ->
            UUID.toString id

        Just namespace_ ->
            namespace_ ++ "/" ++ UUID.toString id


sorter : Sorter ID
sorter =
    Sort.alphabetical
        |> Sort.by (\(ID namespace_ _) -> Maybe.withDefault "" namespace_)
        |> Sort.tiebreaker
            (Sort.alphabetical
                |> Sort.by (\(ID namespace_ id) -> UUID.toString id)
            )


generator : Generator ID
generator =
    Random.map (ID Nothing) UUID.generator


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
