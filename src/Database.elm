module Database exposing (Database, get, init, insert, isEmpty, notes)

import Array exposing (Array)
import Node exposing (Node)



-- performance note: push/get to arrays is faster than dicts. https://ellie-app.com/8xpcZ9KbFRWa1


type Database
    = Database
        { nodes : Array Node
        , nextID : Node.ID
        }


init : Database
init =
    Database { nodes = Array.empty, nextID = Node.ID 0 }


isEmpty : Database -> Bool
isEmpty (Database database) =
    Array.isEmpty database.nodes


insert : Node -> Database -> ( Node.ID, Database )
insert node (Database database) =
    ( database.nextID
    , Database
        { nodes = Array.push { node | id = Just database.nextID } database.nodes
        , nextID = Node.nextID database.nextID
        }
    )


get : Node.ID -> Database -> Maybe Node
get (Node.ID id) (Database database) =
    Array.get id database.nodes


notes : Database -> List Node
notes (Database database) =
    database.nodes
        |> Array.filter (\{ metadata } -> metadata == Just Node.Note)
        |> Array.toList
