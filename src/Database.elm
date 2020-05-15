module Database exposing
    ( Database, Row, empty, load, isEmpty, get, insert, update, updateRevision, delete, filter, previousSibling, nextSibling, nextNode, backlinksTo
    , moveInto, moveBefore, moveAfter
    , decoder, encode, toPersist
    )

{-|

@docs Database, Row, empty, load, isEmpty, get, insert, update, updateRevision, delete, filter, previousSibling, nextSibling, nextNode, backlinksTo

@docs moveInto, moveBefore, moveAfter

@docs decoder, encode, toPersist

-}

import Content
import Database.ID as ID exposing (ID)
import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Node exposing (Node)
import Random
import Sort.Dict as Dict exposing (Dict)
import Sort.Set as Set exposing (Set)
import Time


type Database
    = Database
        { nodes : Dict ID Row
        , seed : Random.Seed
        , toPersist : Set ID
        }


type alias Row =
    { id : ID
    , node : Node
    , parent : Maybe ID
    , children : List ID
    , created : Time.Posix
    , updated : Time.Posix

    -- PouchDB state
    , revision : Maybe String
    , deleted : Bool
    }


empty : Random.Seed -> Database
empty seed =
    Database
        { nodes = Dict.empty ID.sorter
        , seed = seed
        , toPersist = Set.empty ID.sorter
        }


load : Random.Seed -> List Row -> Database
load seed rows =
    let
        (Database database) =
            empty seed
    in
    Database
        { database
            | nodes =
                List.foldl
                    (\row -> Dict.insert row.id row)
                    database.nodes
                    rows
        }


isEmpty : Database -> Bool
isEmpty (Database database) =
    Dict.isEmpty database.nodes


get : ID -> Database -> Maybe Row
get id (Database database) =
    Dict.get id database.nodes
        |> Maybe.andThen
            (\row ->
                if row.deleted then
                    Nothing

                else
                    Just row
            )


insert : Time.Posix -> Node -> Database -> ( Row, Database )
insert created node (Database database) =
    let
        ( id, seed ) =
            Random.step ID.generator database.seed

        row =
            { id = id
            , node = node
            , parent = Nothing
            , children = []
            , created = created
            , updated = created
            , revision = Nothing
            , deleted = False
            }
    in
    ( row
    , Database
        { nodes = Dict.insert id row database.nodes
        , seed = seed
        , toPersist = Set.insert id database.toPersist
        }
    )


update : Time.Posix -> ID -> (Node -> Node) -> Database -> Database
update updated id updater ((Database database) as db) =
    case get id db of
        Just oldRow ->
            let
                newNode =
                    updater oldRow.node
            in
            if newNode == oldRow.node then
                db

            else
                Database
                    { database
                        | nodes =
                            Dict.insert id
                                { oldRow | node = updater oldRow.node, updated = updated }
                                database.nodes
                        , toPersist = Set.insert id database.toPersist
                    }
                    |> updateNoteLinks updated oldRow.node newNode

        Nothing ->
            db


updateNoteLinks : Time.Posix -> Node -> Node -> Database -> Database
updateNoteLinks updated old new database =
    if not (Node.isTitle old) then
        database

    else
        database
            -- optimization idea, if/when it becomes necessary: updating a node
            -- could keep a separate index of note links. Then updating them
            -- can avoid a full scan of the database.
            |> filter
                (\node -> Content.hasNoteLink (Node.content old) (Node.content node))
            |> List.foldl
                (\rowToUpdate dbProgress ->
                    update updated
                        rowToUpdate.id
                        (\node ->
                            Node.setContent
                                (Content.replaceNoteLinks (Node.content old)
                                    (Node.content new)
                                    (Node.content node)
                                )
                                node
                        )
                        dbProgress
                )
                database


backlinksTo : ID -> Database -> List Row
backlinksTo id database =
    case get id database of
        Just row ->
            filter
                (\node -> Content.hasNoteLink (Node.content row.node) (Node.content node))
                database

        Nothing ->
            []


updateRevision : ID -> String -> Database -> Database
updateRevision id revision ((Database database) as db) =
    Database
        { database
            | nodes =
                Dict.update id
                    (Maybe.map (\row -> { row | revision = Just revision }))
                    database.nodes
        }


delete : ID -> Database -> Database
delete toDelete ((Database database) as db) =
    case get toDelete db of
        Just row ->
            let
                ( newNodes, newToPersist ) =
                    case Maybe.andThen (\id -> get id db) row.parent of
                        Just parent ->
                            ( Dict.insert parent.id
                                { parent | children = List.filter ((/=) toDelete) parent.children }
                                database.nodes
                            , Set.insert parent.id database.toPersist
                            )

                        Nothing ->
                            ( database.nodes
                            , database.toPersist
                            )
            in
            Database
                { database
                    | nodes = Dict.insert row.id { row | deleted = True } newNodes
                    , toPersist = Set.insert row.id newToPersist
                }

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
detachChild child ((Database db) as database) =
    case
        get child database
            |> Maybe.andThen .parent
            |> Maybe.andThen (\id -> get id database)
    of
        Just parent ->
            Database
                { db
                    | nodes =
                        db.nodes
                            |> Dict.insert parent.id { parent | children = List.filter ((/=) child) parent.children }
                            |> Dict.update child (Maybe.map (\row -> { row | parent = Nothing }))
                    , toPersist =
                        db.toPersist
                            |> Set.insert child
                            |> Set.insert parent.id
                }

        Nothing ->
            database


prependChild : ID -> ID -> Database -> Database
prependChild parent child (Database database) =
    Database
        { database
            | nodes =
                database.nodes
                    |> Dict.update parent (Maybe.map (\node -> { node | children = child :: node.children }))
                    |> Dict.update child (Maybe.map (\node -> { node | parent = Just parent }))
            , toPersist =
                database.toPersist
                    |> Set.insert parent
                    |> Set.insert child
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
                    , toPersist =
                        db.toPersist
                            |> Set.insert parent
                            |> Set.insert target
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
                    , toPersist =
                        db.toPersist
                            |> Set.insert parent
                            |> Set.insert target
                }


filter : (Node -> Bool) -> Database -> List Row
filter shouldInclude (Database database) =
    Dict.foldr
        (\_ current previous ->
            if not current.deleted && shouldInclude current.node then
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



-- Storage


decoder : Decoder Row
decoder =
    Decode.andThen
        (\id ->
            case ID.namespace id of
                -- compatibility with older PouchDB data from before rows
                -- were namespaced.
                Nothing ->
                    rowDecoder id

                Just "node" ->
                    rowDecoder id

                Just other ->
                    Decode.fail ("I don't know how to handle documents in the " ++ other ++ " namespace.")
        )
        (Decode.field "_id" ID.decoder)


rowDecoder : ID -> Decoder Row
rowDecoder id =
    Decode.succeed (Row id)
        |> Pipeline.required "node" Node.decoder
        |> Pipeline.required "parent" (Decode.nullable ID.decoder)
        |> Pipeline.required "children" (Decode.list ID.decoder)
        |> Pipeline.optional "created" Iso8601.decoder (Time.millisToPosix 0)
        |> Pipeline.optional "updated" Iso8601.decoder (Time.millisToPosix 0)
        |> Pipeline.required "_rev" (Decode.nullable Decode.string)
        |> Pipeline.optional "_deleted" Decode.bool False


encode : Row -> Encode.Value
encode row =
    Encode.object
        [ -- TODO: _id, _rev, and _deleted are specifically for the PouchDB
          -- storage. Is that OK?
          ( "_id", ID.encode row.id )
        , ( "_rev"
          , case row.revision of
                Nothing ->
                    Encode.null

                Just revision ->
                    Encode.string revision
          )
        , ( "_deleted", Encode.bool row.deleted )

        -- the rest of the fields are the document, so we get to choose
        -- these names
        , ( "node", Node.encode row.node )
        , ( "parent"
          , case row.parent of
                Nothing ->
                    Encode.null

                Just parent ->
                    ID.encode parent
          )
        , ( "children", Encode.list ID.encode row.children )
        , ( "created", Iso8601.encode row.created )
        , ( "updated", Iso8601.encode row.updated )
        ]


{-| TODO: would it make more sense for this to return a set of IDs?
-}
toPersist : Database -> ( List Row, Database )
toPersist ((Database database) as db) =
    ( -- not using `get` here since it removes deleted nodes
      List.filterMap (\id -> Dict.get id database.nodes) (Set.toList database.toPersist)
    , Database { database | toPersist = Set.empty ID.sorter }
    )
