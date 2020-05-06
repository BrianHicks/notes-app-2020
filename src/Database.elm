module Database exposing
    ( Database, empty, isEmpty, insert, update, updateRevision, delete, get, filter, previousSibling, nextSibling, nextNode
    , moveInto, moveBefore, moveAfter
    , encode
    )

{-|

@docs Database, empty, isEmpty, insert, update, updateRevision, delete, get, filter, previousSibling, nextSibling, nextNode

@docs moveInto, moveBefore, moveAfter

@docs encode

-}

import Database.ID as ID exposing (ID)
import Json.Encode as Encode
import Node exposing (Node)
import Random
import Sort exposing (Sorter)
import Sort.Dict as Dict exposing (Dict)
import Time
import UUID exposing (UUID)


type Database
    = Database
        { nodes : Dict ID Row
        , seed : Random.Seed
        }


type alias Row =
    { id : ID
    , node : Node
    , parent : Maybe ID
    , children : List ID

    -- PouchDB state
    , revision : Maybe String
    }


empty : Random.Seed -> Database
empty seed =
    Database
        { nodes = Dict.empty ID.sorter
        , seed = seed
        }


isEmpty : Database -> Bool
isEmpty (Database database) =
    Dict.isEmpty database.nodes


insert : Node -> Database -> ( Row, Database )
insert node (Database database) =
    let
        ( id, seed ) =
            Random.step ID.generator database.seed

        row =
            { id = id
            , node = node
            , parent = Nothing
            , children = []
            , revision = Nothing
            }
    in
    ( row
    , Database
        { nodes = Dict.insert id row database.nodes
        , seed = seed
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


get : ID -> Database -> Maybe Row
get id (Database database) =
    Dict.get id database.nodes


update : ID -> (Node -> Node) -> Database -> ( Maybe Row, Database )
update id updater ((Database database) as db) =
    case get id db of
        Just row ->
            let
                updated =
                    { row | node = updater row.node }
            in
            ( Just updated
            , Database { database | nodes = Dict.insert id updated database.nodes }
            )

        Nothing ->
            ( Nothing
            , db
            )


{-| This shouldn't get re-persisted just because of a revision update,
so we don't return the updated Row this time!
-}
updateRevision : ID -> String -> Database -> Database
updateRevision id revision (Database database) =
    Database
        { database
            | nodes =
                Dict.update id
                    (Maybe.map (\row -> { row | revision = Just revision }))
                    database.nodes
        }


filter : (Node -> Bool) -> Database -> List Row
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



-- JSON


encode : Row -> Encode.Value
encode row =
    Encode.object
        [ -- TODO: this is _id specifically for the pouchdb storage. Is that OK?
          ( "_id", ID.encode row.id )
        , ( "node", Node.encode row.node )
        , ( "parent"
          , case row.parent of
                Nothing ->
                    Encode.null

                Just parent ->
                    ID.encode parent
          )
        , ( "children", Encode.list ID.encode row.children )
        ]
