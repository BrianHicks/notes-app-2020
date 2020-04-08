module Database exposing (Database, get, init, insert, isEmpty)

import Array exposing (Array)
import Node exposing (Node)



-- performance note: push/get to arrays is faster than dicts. https://ellie-app.com/8xpcZ9KbFRWa1


type Database
    = Database
        { nodes : Array Node
        , nextID : Int
        }


init : Database
init =
    Database { nodes = Array.empty, nextID = 0 }


isEmpty : Database -> Bool
isEmpty (Database database) =
    Array.isEmpty database.nodes


insert : Node -> Database -> ( Int, Database )
insert node (Database database) =
    ( database.nextID
    , Database
        { nodes = Array.push { node | id = Just database.nextID } database.nodes
        , nextID = database.nextID + 1
        }
    )


get : Int -> Database -> Maybe Node
get id (Database database) =
    Array.get id database.nodes
