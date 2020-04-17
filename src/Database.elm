module Database exposing
    ( Database, empty, isEmpty, insert, update, delete, get, filter, previousSibling
    , moveInto, moveBefore, moveAfter
    , ID, idFromInt, idToString
    )

{-|

@docs Database, empty, isEmpty, insert, update, delete, get, filter, previousSibling

@docs moveInto, moveBefore, moveAfter

@docs ID, idFromInt, idToString

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
                    { id : ID
                    , node : Node
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
                    { id = database.nextID
                    , node = node
                    , parent = Nothing
                    , children = []
                    }
                )
                database.nodes
        , nextID = nextID database.nextID
        }
    )


delete : ID -> Database -> Database
delete ((ID id) as toDelete) ((Database database) as db) =
    case get toDelete db of
        Just node ->
            let
                withParentHandled =
                    case node.parent of
                        Just (ID parentID) ->
                            Array.Extra.update
                                parentID
                                (Maybe.map (\parent -> { parent | children = List.filter ((/=) toDelete) parent.children }))
                                database.nodes

                        Nothing ->
                            database.nodes
            in
            Database
                { database | nodes = Array.set id Nothing withParentHandled }

        Nothing ->
            db


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


moveBefore : ID -> ID -> Database -> Database
moveBefore sibling target database =
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
            |> prependSibling sibling target


previousSibling : ID -> Database -> Maybe ID
previousSibling id database =
    get id database
        |> Maybe.andThen .parent
        |> Maybe.andThen (\parentID -> get parentID database)
        |> Maybe.andThen
            (\{ children } ->
                let
                    helper : Maybe ID -> List ID -> Maybe ID
                    helper current nextChildren =
                        case nextChildren of
                            [] ->
                                Nothing

                            nextChild :: restChildren ->
                                if nextChild == id then
                                    current

                                else
                                    helper (Just nextChild) restChildren
                in
                helper Nothing children
            )


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


prependSibling : ID -> ID -> Database -> Database
prependSibling sibling ((ID targetID) as target) ((Database db) as database) =
    case get sibling database |> Maybe.andThen .parent of
        Nothing ->
            database

        Just (ID parentID) ->
            Database
                { db
                    | nodes =
                        db.nodes
                            |> Array.Extra.update parentID (Maybe.map (\node -> { node | children = insertBefore sibling target node.children }))
                            |> Array.Extra.update targetID (Maybe.map (\node -> { node | parent = Just (ID parentID) }))
                }


get : ID -> Database -> Maybe { id : ID, node : Node, parent : Maybe ID, children : List ID }
get (ID id) (Database database) =
    database.nodes
        |> Array.get id
        |> Maybe.andThen identity


update : ID -> (Node -> Node) -> Database -> Database
update (ID id) updater (Database database) =
    Database { database | nodes = Array.Extra.update id (Maybe.map (\node -> { node | node = updater node.node })) database.nodes }


filter : (Node -> Bool) -> Database -> List { id : ID, node : Node, parent : Maybe ID, children : List ID }
filter shouldInclude (Database database) =
    database.nodes
        |> Array.Extra.filterMap
            (Maybe.andThen
                (\node ->
                    if shouldInclude node.node then
                        Just node

                    else
                        Nothing
                )
            )
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


idToString : ID -> String
idToString (ID id) =
    String.fromInt id



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


insertBefore : a -> a -> List a -> List a
insertBefore target toInsert items =
    insertBeforeHelp target toInsert items []


insertBeforeHelp : a -> a -> List a -> List a -> List a
insertBeforeHelp target toInsert items soFar =
    case items of
        [] ->
            List.reverse soFar

        candidate :: rest ->
            if candidate == target then
                List.reverse soFar ++ toInsert :: candidate :: rest

            else
                insertBeforeHelp target toInsert rest (candidate :: soFar)
