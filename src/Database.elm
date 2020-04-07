module Database exposing (Database, init, insert, notes)

import Node exposing (Node)
import Sort.Dict as Dict exposing (Dict)


type Database
    = Database (Dict Node.ID Node)


init : Database
init =
    Database (Dict.empty Node.idSorter)


notes : Database -> List Node
notes (Database database) =
    database
        |> Dict.values
        |> List.filter (\node -> node.metadata == Just Node.Note)


insert : Node -> Database -> Database
insert ({ id } as node) (Database database) =
    Database (Dict.insert id node database)
