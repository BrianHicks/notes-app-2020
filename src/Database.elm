module Database exposing
    ( Database, empty, isEmpty, insert, appendChild, delete, get, children
    , ID, idFromInt
    )

{-|

@docs Database, empty, isEmpty, insert, appendChild, delete, get, children

@docs ID, idFromInt

-}

import Array exposing (Array)
import Array.Extra
import Node exposing (Node)



-- performance note: push/get to arrays is faster than dicts. https://ellie-app.com/8xpcZ9KbFRWa1


type Database
    = Database
        { nodes : Array (Maybe ( Node, Array ID ))
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
        { nodes = Array.push (Just ( node, Array.empty )) database.nodes
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
                | nodes =
                    Array.Extra.update parentID
                        (Maybe.map (Tuple.mapSecond (Array.push child)))
                        database.nodes
            }


delete : ID -> Database -> Database
delete (ID id) (Database database) =
    Database { database | nodes = Array.set id Nothing database.nodes }


get : ID -> Database -> Maybe Node
get (ID id) (Database database) =
    database.nodes
        |> Array.get id
        |> Maybe.andThen identity
        |> Maybe.map Tuple.first


children : ID -> Database -> Array ID
children (ID id) (Database database) =
    -- TODO: this and get are pretty similar. Should they be combined?
    database.nodes
        |> Array.get id
        |> Maybe.andThen identity
        |> Maybe.map Tuple.second
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
