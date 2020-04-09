module Database exposing
    ( Database, empty, isEmpty, insert, moveInto, moveAfter, get
    , ID, idFromInt
    )

{-|

@docs Database, empty, isEmpty, insert, moveInto, moveAfter, delete, get

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
                    , children : List ID
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
                    , children = []
                    }
                )
                database.nodes
        , nextID = nextID database.nextID
        }
    )


moveInto : ID -> ID -> Database -> Database
moveInto parent child database =
    if parent == child || get parent database == Nothing || get child database == Nothing then
        database

    else
        database
            |> detachChild child
            |> prependChild parent child


moveAfter : ID -> ID -> Database -> Database
moveAfter sibling target database =
    if
        (sibling == target)
            || (get sibling database == Nothing)
            || (get target database == Nothing)
            || (Maybe.andThen .parent (get sibling database) == Nothing)
    then
        database

    else
        database
            |> detachChild target
            |> appendSibling sibling target


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
                                        { node | children = List.filter ((/=) child) node.children }
                                    )
                                )
                                database.nodes
                        )
                    |> Maybe.withDefault database.nodes
                    |> Array.Extra.update childID (Maybe.map (\node -> { node | parent = Nothing }))
        }


prependChild : ID -> ID -> Database -> Database
prependChild ((ID parentID) as parent) ((ID childID) as child) (Database database) =
    Database
        { database
            | nodes =
                database.nodes
                    |> Array.Extra.update parentID (Maybe.map (\node -> { node | children = child :: node.children }))
                    |> Array.Extra.update childID (Maybe.map (\node -> { node | parent = Just (ID parentID) }))
        }


appendSibling : ID -> ID -> Database -> Database
appendSibling sibling ((ID targetID) as target) ((Database db) as database) =
    case get sibling database |> Maybe.andThen .parent of
        Nothing ->
            database

        Just (ID parentID) ->
            Database
                { db
                    | nodes =
                        db.nodes
                            |> Array.Extra.update parentID (Maybe.map (\node -> { node | children = insertAfter sibling target node.children }))
                            |> Array.Extra.update targetID (Maybe.map (\node -> { node | parent = Just (ID parentID) }))
                }


get : ID -> Database -> Maybe { node : Node, parent : Maybe ID, children : List ID }
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



-- UTILITY


insertAfter : a -> a -> List a -> List a
insertAfter target toInsert items =
    insertAfterHelp target toInsert items []


insertAfterHelp : a -> a -> List a -> List a -> List a
insertAfterHelp target toInsert items soFar =
    case items of
        [] ->
            List.reverse soFar

        candidate :: rest ->
            if candidate == target then
                List.reverse soFar ++ candidate :: toInsert :: rest

            else
                insertAfterHelp target toInsert rest (candidate :: soFar)
