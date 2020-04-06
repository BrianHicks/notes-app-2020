module Database exposing (Database, init)

import Node exposing (Node)
import Sort.Dict as Dict exposing (Dict)


type alias Database =
    { content : Dict Node.ID Node
    }


init : Database
init =
    { content = Dict.empty Node.idSorter }
