module Node exposing (ID(..), Metadata(..), Node, idSorter)

import Sort exposing (Sorter)
import Time exposing (Posix)
import UUID exposing (UUID)


type ID
    = ID UUID


idSorter : Sorter ID
idSorter =
    Sort.by (\(ID id) -> UUID.toString id) Sort.alphabetical


type alias Node =
    { id : ID
    , metadata : Maybe Metadata
    , content : String
    , children : List ID
    , updated : Posix
    }


type Metadata
    = Note
