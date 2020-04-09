module Database exposing
    ( Database, empty, isEmpty, insert, moveToLastChild, moveAfter, delete, get
    , ID, idFromInt
    )

{-|

@docs Database, empty, isEmpty, insert, moveToLastChild, moveAfter, delete, get

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


{-| TODO: rename to... moveInside? moveUnder?
-}
moveToLastChild : ID -> ID -> Database -> Database
moveToLastChild parent child database =
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



-- UTILITY


insertAfter : a -> a -> Array a -> Array a
insertAfter target toInsert items =
    case arrayFind target items of
        Just index ->
            Array.append
                (Array.slice 0 (index + 1) items |> Array.push toInsert)
                (Array.slice (index + 1) (Array.length items) items)

        Nothing ->
            items


arrayFind : a -> Array a -> Maybe Int
arrayFind item items =
    arrayFindHelp 0 item (Array.toList items)


arrayFindHelp : Int -> a -> List a -> Maybe Int
arrayFindHelp index item items =
    case items of
        [] ->
            Nothing

        candidate :: rest ->
            if candidate == item then
                Just index

            else
                arrayFindHelp (index + 1) item rest
