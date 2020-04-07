module Database exposing (Database, get, init, insert, notes, update)

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


get : Node.ID -> Database -> Maybe Node
get id (Database database) =
    Dict.get id database


update : Node.ID -> (Node -> Node) -> Database -> Database
update id fn (Database database) =
    Database (Dict.update id (Maybe.map fn) database)
