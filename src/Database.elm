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

import Database.LWW as LWW exposing (LWW)
import Database.Timestamp as Timestamp exposing (Timestamp)
import Node exposing (Node)
import Random
import Sort exposing (Sorter)
import Sort.Dict as Dict exposing (Dict)
import Time
import UUID exposing (UUID)


type Database
    = Database
        { nodes :
            Dict ID
                { id : ID
                , node : LWW (Maybe Node)
                , parent : Maybe ID
                , children : List ID
                }
        , seed : Random.Seed
        , generator : Timestamp.Generator
        }


empty : Random.Seed -> Timestamp.NodeID -> Database
empty seed nodeID =
    Database
        { nodes = Dict.empty idSorter
        , seed = seed
        , generator = Timestamp.generator nodeID
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
            Dict.insert id
                { id = id
                , node = LWW.init (Just node) -- TODO: get a timestamp first!
                , parent = Nothing
                , children = []
                }
                database.nodes
        , seed = seed
        , generator = database.generator
        }
    )


delete : ID -> Database -> Database
delete toDelete ((Database database) as db) =
    case get toDelete db of
        Just node ->
            let
                withParentHandled =
                    case node.parent of
                        Just parentID ->
                            Dict.update
                                parentID
                                (Maybe.map (\parent -> { parent | children = List.filter ((/=) toDelete) parent.children }))
                                database.nodes

                        Nothing ->
                            database.nodes
            in
            Database
                { database | nodes = Dict.remove toDelete withParentHandled }

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
detachChild child (Database database) =
    Database
        { database
            | nodes =
                database.nodes
                    |> Dict.get child
                    |> Maybe.andThen .parent
                    |> Maybe.map
                        (\oldParent ->
                            Dict.update oldParent
                                (Maybe.map
                                    (\node ->
                                        { node | children = List.filter ((/=) child) node.children }
                                    )
                                )
                                database.nodes
                        )
                    |> Maybe.withDefault database.nodes
                    |> Dict.update child (Maybe.map (\node -> { node | parent = Nothing }))
        }


prependChild : ID -> ID -> Database -> Database
prependChild parent child (Database database) =
    Database
        { database
            | nodes =
                database.nodes
                    |> Dict.update parent (Maybe.map (\node -> { node | children = child :: node.children }))
                    |> Dict.update child (Maybe.map (\node -> { node | parent = Just parent }))
        }


appendSibling : ID -> ID -> Database -> Database
appendSibling sibling target ((Database db) as database) =
    case get sibling database |> Maybe.andThen .parent of
        Nothing ->
            database

        Just parent ->
            Database
                { db
                    | nodes =
                        db.nodes
                            |> Dict.update parent (Maybe.map (\node -> { node | children = insertAfter sibling target node.children }))
                            |> Dict.update target (Maybe.map (\node -> { node | parent = Just parent }))
                }


prependSibling : ID -> ID -> Database -> Database
prependSibling sibling target ((Database db) as database) =
    case get sibling database |> Maybe.andThen .parent of
        Nothing ->
            database

        Just parent ->
            Database
                { db
                    | nodes =
                        db.nodes
                            |> Dict.update parent (Maybe.map (\node -> { node | children = insertBefore sibling target node.children }))
                            |> Dict.update target (Maybe.map (\node -> { node | parent = Just parent }))
                }


get : ID -> Database -> Maybe { id : ID, node : Node, parent : Maybe ID, children : List ID }
get id (Database database) =
    database.nodes
        |> Dict.get id
        |> Maybe.andThen
            (\guts ->
                Maybe.map
                    (\node ->
                        { id = guts.id
                        , node = node
                        , parent = guts.parent
                        , children = guts.children
                        }
                    )
                    (LWW.value guts.node)
            )


update : ID -> (Node -> Node) -> Database -> Database
update id updater (Database database) =
    Database { database | nodes = Dict.update id (Maybe.map (\node -> { node | node = LWW.map (Maybe.map updater) node.node })) database.nodes }


filter : (Node -> Bool) -> Database -> List { id : ID, node : Node, parent : Maybe ID, children : List ID }
filter shouldInclude (Database database) =
    Dict.foldr
        (\_ current previous ->
            case LWW.value current.node of
                Just node ->
                    if shouldInclude node then
                        { id = current.id
                        , node = node
                        , parent = current.parent
                        , children = current.children
                        }
                            :: previous

                    else
                        previous

                Nothing ->
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


idSorter : Sorter ID
idSorter =
    Sort.by (\(ID id) -> UUID.toString id) Sort.alphabetical



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
