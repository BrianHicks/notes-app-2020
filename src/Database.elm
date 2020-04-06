module Database exposing (..)

import Node exposing (Node)
import Sort.Dict as Dict exposing (Dict)


type alias Database =
    { content : Dict Node.ID Node
    }


empty : Database
empty =
    { content = Dict.empty Node.idSorter }
