module Database exposing
    ( Database, empty, isEmpty, get, children, insert, appendChild
    , ID, idFromInt
    )

{-|

@docs Database, empty, isEmpty, get, children, insert, appendChild

@docs ID, idFromInt

-}

import Array exposing (Array)
import Array.Extra
import Node exposing (Node)



-- performance note: push/get to arrays is faster than dicts. https://ellie-app.com/8xpcZ9KbFRWa1


type Database
    = Database
        { nodes : Array Node
        , children : Array (Array ID)
        , nextID : ID
        }


empty : Database
empty =
    Database
        { nodes = Array.empty
        , children = Array.empty
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
        , children = Array.push Array.empty database.children
        , nextID = nextID database.nextID
        }
    )


appendChild : ID -> ID -> Database -> Database
appendChild (ID parentID) ((ID childID) as child) (Database database) =
    if parentID == childID then
        Database database

    else
        Database
            { database
                | children =
                    Array.Extra.update parentID
                        (Array.push child)
                        database.children
            }


get : ID -> Database -> Maybe Node
get (ID id) (Database database) =
    Array.get id database.nodes


children : ID -> Database -> Array ID
children (ID id) (Database database) =
    database.children
        |> Array.get id
        |> Maybe.withDefault Array.empty



-- ID


type ID
    = ID Int


nextID : ID -> ID
nextID (ID id) =
    ID (id + 1)


idFromInt : Int -> ID
idFromInt =
    ID
