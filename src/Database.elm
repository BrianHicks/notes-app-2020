module Database exposing
    ( Database, empty, isEmpty, get, insert, notes
    , ID, idFromInt
    )

{-|

@docs Database, empty, isEmpty, get, insert, notes

@docs ID, idFromInt

-}

import Array exposing (Array)
import Node exposing (Node)



-- performance note: push/get to arrays is faster than dicts. https://ellie-app.com/8xpcZ9KbFRWa1


type Database
    = Database
        { nodes : Array Node
        , nextID : ID
        }


empty : Database
empty =
    Database
        { nodes = Array.empty
        , nextID = ID 0
        }


isEmpty : Database -> Bool
isEmpty (Database database) =
    Array.isEmpty database.nodes


insert : Node -> Database -> ( ID, Database )
insert node (Database database) =
    ( database.nextID
    , Database
        { nodes = Array.push node database.nodes
        , nextID = nextID database.nextID
        }
    )


get : ID -> Database -> Maybe Node
get (ID id) (Database database) =
    Array.get id database.nodes


notes : Database -> List Node
notes (Database database) =
    database.nodes
        |> Array.filter (\{ metadata } -> metadata == Just Node.Note)
        |> Array.toList



-- ID


type ID
    = ID Int


nextID : ID -> ID
nextID (ID id) =
    ID (id + 1)


idFromInt : Int -> ID
idFromInt =
    ID
