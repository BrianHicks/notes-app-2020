module Database exposing
    ( Database, empty, isEmpty, insert, update, delete, get, filter, previousSibling, nextSibling, nextNode
    , moveInto, moveBefore, moveAfter
    , ID, idFromInt, idFromString, idToString
    )

{-|

@docs Database, empty, isEmpty, insert, update, delete, get, filter, previousSibling, nextSibling, nextNode

@docs moveInto, moveBefore, moveAfter

@docs ID, idFromInt, idFromString, idToString

-}

import Dict exposing (Dict)
import Node exposing (Node)
import Random
import UUID exposing (UUID)



-- performance note: push/get to arrays is faster than dicts. https://ellie-app.com/8xpcZ9KbFRWa1


type Database
    = Database
        { nodes :
            Dict String
                { id : ID
                , node : Node
                , parent : Maybe ID
                , children : List ID
                }
        , seed : Random.Seed
        }


empty : Random.Seed -> Database
empty seed =
    Database
        { nodes = Dict.empty
        , seed = seed
        }


isEmpty : Database -> Bool
isEmpty (Database database) =
    Dict.isEmpty database.nodes


insert : Node -> Database -> ( ID, Database )
insert node (Database database) =
    let
        ( id, seed ) =
            Random.step (Random.map ID UUID.generator) database.seed
    in
    ( id
    , Database
        { nodes =
            Dict.insert (idToString id)
                { id = id
                , node = node
                , parent = Nothing
                , children = []
                }
                database.nodes
        , seed = seed
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
                            Dict.update
                                (UUID.toString parentID)
                                (Maybe.map (\parent -> { parent | children = List.filter ((/=) toDelete) parent.children }))
                                database.nodes

                        Nothing ->
                            database.nodes
            in
            Database
                { database | nodes = Dict.remove (UUID.toString id) withParentHandled }

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


nextSibling : ID -> Database -> Maybe ID
nextSibling id database =
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
                                if current == Just id then
                                    Just nextChild

                                else
                                    helper (Just nextChild) restChildren
                in
                helper Nothing children
            )


nextNode : ID -> Database -> Maybe ID
nextNode id database =
    case nextSibling id database of
        Just sibling ->
            Just sibling

        Nothing ->
            get id database
                |> Maybe.andThen .parent
                |> Maybe.andThen (\parentId -> nextNode parentId database)


detachChild : ID -> Database -> Database
detachChild ((ID childID) as child) (Database database) =
    Database
        { database
            | nodes =
                database.nodes
                    |> Dict.get (UUID.toString childID)
                    |> Maybe.andThen .parent
                    |> Maybe.map
                        (\(ID oldParentID) ->
                            Dict.update (UUID.toString oldParentID)
                                (Maybe.map
                                    (\node ->
                                        { node | children = List.filter ((/=) child) node.children }
                                    )
                                )
                                database.nodes
                        )
                    |> Maybe.withDefault database.nodes
                    |> Dict.update (UUID.toString childID) (Maybe.map (\node -> { node | parent = Nothing }))
        }


prependChild : ID -> ID -> Database -> Database
prependChild ((ID parentID) as parent) ((ID childID) as child) (Database database) =
    Database
        { database
            | nodes =
                database.nodes
                    |> Dict.update (UUID.toString parentID) (Maybe.map (\node -> { node | children = child :: node.children }))
                    |> Dict.update (UUID.toString childID) (Maybe.map (\node -> { node | parent = Just (ID parentID) }))
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
                            |> Dict.update (UUID.toString parentID) (Maybe.map (\node -> { node | children = insertAfter sibling target node.children }))
                            |> Dict.update (UUID.toString targetID) (Maybe.map (\node -> { node | parent = Just (ID parentID) }))
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
                            |> Dict.update (UUID.toString parentID) (Maybe.map (\node -> { node | children = insertBefore sibling target node.children }))
                            |> Dict.update (UUID.toString targetID) (Maybe.map (\node -> { node | parent = Just (ID parentID) }))
                }


get : ID -> Database -> Maybe { id : ID, node : Node, parent : Maybe ID, children : List ID }
get id (Database database) =
    Dict.get (idToString id) database.nodes


update : ID -> (Node -> Node) -> Database -> Database
update (ID id) updater (Database database) =
    Database { database | nodes = Dict.update (UUID.toString id) (Maybe.map (\node -> { node | node = updater node.node })) database.nodes }


filter : (Node -> Bool) -> Database -> List { id : ID, node : Node, parent : Maybe ID, children : List ID }
filter shouldInclude (Database database) =
    Dict.foldr
        (\_ current previous ->
            if shouldInclude current.node then
                current :: previous

            else
                previous
        )
        []
        database.nodes



-- ID


type ID
    = ID UUID


idFromInt : Int -> ID
idFromInt seed =
    Random.initialSeed seed
        |> Random.step (Random.map ID UUID.generator)
        |> Tuple.first


idFromString : String -> Result UUID.Error ID
idFromString string =
    Result.map ID (UUID.fromString string)


idToString : ID -> String
idToString (ID id) =
    UUID.toString id



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
