module Database exposing
    ( Database, empty, isEmpty, insert, moveToLastChild, delete, get
    , ID, idFromInt
    )

{-|

@docs Database, empty, isEmpty, insert, moveToLastChild, delete, get

@docs ID, idFromInt

-}

import Array exposing (Array)
import Array.Extra
import Node exposing (Node)



-- performance note: push/get to arrays is faster than dicts. https://ellie-app.com/8xpcZ9KbFRWa1


type Database
    = Database
        { nodes :
            Array
                (Maybe
                    { node : Node
                    , parent : Maybe ID
                    , children : Array ID
                    }
                )
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
        { nodes =
            Array.push
                (Just
                    { node = node
                    , parent = Nothing
                    , children = Array.empty
                    }
                )
                database.nodes
        , nextID = nextID database.nextID
        }
    )


moveToLastChild : ID -> ID -> Database -> Database
moveToLastChild ((ID parentID) as parent) ((ID childID) as child) database =
    if parent == child || get parent database == Nothing || get child database == Nothing then
        database

    else
        database
            |> detachChild child
            |> appendChild parent child


detachChild : ID -> Database -> Database
detachChild ((ID childID) as child) (Database database) =
    Database
        { database
            | nodes =
                database.nodes
                    |> Array.get childID
                    |> Maybe.andThen identity
                    |> Maybe.andThen .parent
                    |> Maybe.map
                        (\(ID oldParentID) ->
                            Array.Extra.update oldParentID
                                (Maybe.map
                                    (\node ->
                                        { node | children = Array.filter ((/=) child) node.children }
                                    )
                                )
                                database.nodes
                        )
                    |> Maybe.withDefault database.nodes
                    |> Array.Extra.update childID (Maybe.map (\node -> { node | parent = Nothing }))
        }


appendChild : ID -> ID -> Database -> Database
appendChild ((ID parentID) as parent) ((ID childID) as child) (Database database) =
    Database
        { database
            | nodes =
                database.nodes
                    |> Array.Extra.update parentID (Maybe.map (\node -> { node | children = Array.push child node.children }))
                    |> Array.Extra.update childID (Maybe.map (\node -> { node | parent = Just (ID parentID) }))
        }


delete : ID -> Database -> Database
delete (ID id) (Database database) =
    Database { database | nodes = Array.set id Nothing database.nodes }


get : ID -> Database -> Maybe { node : Node, parent : Maybe ID, children : Array ID }
get (ID id) (Database database) =
    database.nodes
        |> Array.get id
        |> Maybe.andThen identity



-- ID


type ID
    = ID Int


nextID : ID -> ID
nextID (ID id) =
    ID (id + 1)


idFromInt : Int -> ID
idFromInt =
    ID
