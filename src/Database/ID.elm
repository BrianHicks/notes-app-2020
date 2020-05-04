module Database.ID exposing (ID, fromInt, fromString, generator, sorter, toString)

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
