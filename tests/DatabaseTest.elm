module DatabaseTest exposing (..)

import Array
import Content exposing (Content)
import Database exposing (..)
import Database.ID as ID exposing (ID)
import Expect
import Node exposing (Node)
import Random
import Test exposing (..)
import Time


databaseTest : Test
databaseTest =
    describe "Database"
        [ describe "empty"
            [ test "starts with a empty database" <|
                \_ -> Expect.equal True (isEmpty emptyFixture)
            ]
        , describe "insert"
            [ test "inserts a node" <|
                \_ ->
                    emptyFixture
                        |> insert_ (Node.title Content.empty)
                        |> Tuple.second
                        |> isEmpty
                        |> Expect.equal False
            , test "assigns an ID" <|
                \_ ->
                    emptyFixture
                        |> insert_ (Node.title Content.empty)
                        |> Tuple.first
                        |> .id
                        |> Expect.equal (ID.fromInt 0)
            , test "starts without children" <|
                \_ ->
                    emptyFixture
                        |> insert_ (Node.title Content.empty)
                        |> Tuple.first
                        |> .children
                        |> Expect.equal []
            , test "starts without a parent" <|
                \_ ->
                    emptyFixture
                        |> insert_ (Node.title Content.empty)
                        |> Tuple.first
                        |> .parent
                        |> Expect.equal Nothing
            , test "marks the node to be persisted" <|
                \_ ->
                    let
                        ( row, database ) =
                            insert_ (Node.title Content.empty) emptyFixture
                    in
                    database
                        |> toPersist
                        |> Tuple.first
                        |> List.map .id
                        |> Expect.equal [ row.id ]
            ]
        , describe "get"
            [ test "you can get a node again" <|
                \_ ->
                    let
                        content =
                            Content.fromList [ Content.text "Hey" ]

                        ( { id }, database ) =
                            insert_ (Node.title content) emptyFixture
                    in
                    database
                        |> get id
                        |> Maybe.map (.node >> Node.content)
                        |> Expect.equal (Just content)
            , test "getting a node that doesn't exist returns Nothing" <|
                \_ -> Expect.equal Nothing (get (ID.fromInt 0) emptyFixture)
            ]
        , describe "moving into a parent"
            [ test "shows the relationship in .children" <|
                \_ ->
                    let
                        ( parent, ( child, database ) ) =
                            emptyFixture
                                |> insert_ (Node.title (plainContent "parent"))
                                |> Tuple.mapSecond (insert_ (Node.node (plainContent "child")))
                    in
                    database
                        |> moveInto parent.id child.id
                        |> get parent.id
                        |> Maybe.map .children
                        |> Expect.equal (Just [ child.id ])
            , test "marks both the parent and child as unpersisted" <|
                \_ ->
                    let
                        ( parent, ( child, ( _, database ) ) ) =
                            emptyFixture
                                |> insert_ (Node.title (plainContent "parent"))
                                |> Tuple.mapSecond (insert_ (Node.node (plainContent "child")))
                                |> Tuple.mapSecond (Tuple.mapSecond toPersist)
                    in
                    database
                        |> moveInto parent.id child.id
                        |> toPersist
                        |> Tuple.first
                        |> List.map .id
                        |> Expect.equal [ parent.id, child.id ]
            , test "shows the relationship in .parent" <|
                \_ ->
                    let
                        ( parent, ( child, database ) ) =
                            emptyFixture
                                |> insert_ (Node.title (plainContent "parent"))
                                |> Tuple.mapSecond (insert_ (Node.node (plainContent "child")))
                    in
                    database
                        |> moveInto parent.id child.id
                        |> get child.id
                        |> Maybe.map .parent
                        |> Expect.equal (Just (Just parent.id))
            , test "will not append a node to itself" <|
                \_ ->
                    let
                        ( { id }, database ) =
                            insert_ (Node.title (plainContent "note")) emptyFixture
                    in
                    database
                        |> moveInto id id
                        |> get id
                        |> Maybe.map .children
                        |> Expect.equal (Just [])
            , test "remove the subject node from where it was before" <|
                \_ ->
                    let
                        ( siblingA, ( siblingB, dbTemp ) ) =
                            emptyFixture
                                |> insert_ (Node.title (plainContent "sibling A"))
                                |> Tuple.mapSecond (insert_ (Node.title (plainContent "sibling B")))

                        ( child, database ) =
                            insert_ (Node.node (plainContent "child")) dbTemp
                    in
                    database
                        |> moveInto siblingA.id child.id
                        |> moveInto siblingB.id child.id
                        |> get siblingA.id
                        |> Maybe.map .children
                        |> Expect.equal (Just [])
            , test "will not insert a bad child ID" <|
                \_ ->
                    let
                        ( { id }, database ) =
                            insert_ (Node.title (plainContent "note")) emptyFixture
                    in
                    database
                        |> moveInto id (ID.fromInt 0x1BAD1DEA)
                        |> Expect.equal database
            , test "will not insert a bad parent ID" <|
                \_ ->
                    let
                        ( { id }, database ) =
                            insert_ (Node.title (plainContent "note")) emptyFixture
                    in
                    database
                        |> moveInto (ID.fromInt 0x1BAD1DEA) id
                        |> Expect.equal database
            ]
        , describe "moving after a sibling"
            [ test "will not move a node after itself" <|
                \_ ->
                    let
                        ( { id }, database ) =
                            insert_ (Node.title (plainContent "note")) emptyFixture
                    in
                    database
                        |> moveAfter id id
                        |> Expect.equal database
            , test "will not move after a missing node" <|
                \_ ->
                    let
                        ( { id }, database ) =
                            insert_ (Node.title (plainContent "note")) emptyFixture
                    in
                    database
                        |> moveAfter (ID.fromInt 1000) id
                        |> Expect.equal database
            , test "will not move a missing node" <|
                \_ ->
                    let
                        ( { id }, database ) =
                            insert_ (Node.title (plainContent "note")) emptyFixture
                    in
                    database
                        |> moveAfter id (ID.fromInt 1000)
                        |> Expect.equal database
            , test "will move directly after the sibling" <|
                \_ ->
                    let
                        ( parent, ( first, ( second, ( third, database ) ) ) ) =
                            insert_ (Node.title (plainContent "parent")) emptyFixture
                                |> Tuple.mapSecond (insert_ (Node.title (plainContent "first")))
                                |> Tuple.mapSecond (Tuple.mapSecond (insert_ (Node.title (plainContent "second"))))
                                |> Tuple.mapSecond (Tuple.mapSecond (Tuple.mapSecond (insert_ (Node.title (plainContent "third")))))
                    in
                    database
                        |> moveInto parent.id third.id
                        |> moveInto parent.id second.id
                        |> moveInto parent.id first.id
                        |> moveAfter second.id first.id
                        |> get parent.id
                        |> Maybe.map .children
                        |> Expect.equal (Just [ second.id, first.id, third.id ])
            , test "will mark both the moved node and parent as needing update" <|
                \_ ->
                    let
                        ( parent, ( first, ( second, database ) ) ) =
                            insert_ (Node.title (plainContent "parent")) emptyFixture
                                |> Tuple.mapSecond (insert_ (Node.title (plainContent "first")))
                                |> Tuple.mapSecond (Tuple.mapSecond (insert_ (Node.title (plainContent "second"))))
                    in
                    database
                        |> moveInto parent.id first.id
                        |> toPersist
                        |> Tuple.second
                        -----
                        |> moveAfter first.id second.id
                        |> toPersist
                        |> Tuple.first
                        |> List.map .id
                        |> Expect.equal [ second.id, parent.id ]
            ]
        , describe "finding a sibling"
            [ test "won't work for the only note in the database" <|
                \_ ->
                    let
                        ( { id }, database ) =
                            insert_ (Node.title (plainContent "note")) emptyFixture
                    in
                    database
                        |> previousSibling id
                        |> Expect.equal Nothing
            , test "won't work for the first node in a sibling group" <|
                \_ ->
                    let
                        ( parent, ( first, ( second, database ) ) ) =
                            insert_ (Node.title (plainContent "parent")) emptyFixture
                                |> Tuple.mapSecond (insert_ (Node.title (plainContent "first")))
                                |> Tuple.mapSecond (Tuple.mapSecond (insert_ (Node.title (plainContent "second"))))
                    in
                    database
                        |> moveInto parent.id second.id
                        |> moveInto parent.id first.id
                        -----
                        |> previousSibling first.id
                        |> Expect.equal Nothing
            , test "will work for the second node in a sibling group" <|
                \_ ->
                    let
                        ( parent, ( first, ( second, database ) ) ) =
                            insert_ (Node.title (plainContent "parent")) emptyFixture
                                |> Tuple.mapSecond (insert_ (Node.title (plainContent "first")))
                                |> Tuple.mapSecond (Tuple.mapSecond (insert_ (Node.title (plainContent "second"))))
                    in
                    database
                        |> moveInto parent.id second.id
                        |> moveInto parent.id first.id
                        -----
                        |> previousSibling second.id
                        |> Expect.equal (Just first.id)
            ]
        , describe "finding a node below"
            [ test "finds the next sibling, if one is present" <|
                \_ ->
                    let
                        ( parent, ( first, ( second, database ) ) ) =
                            insert_ (Node.title (plainContent "parent")) emptyFixture
                                |> Tuple.mapSecond (insert_ (Node.title (plainContent "first")))
                                |> Tuple.mapSecond (Tuple.mapSecond (insert_ (Node.title (plainContent "second"))))
                    in
                    database
                        |> moveInto parent.id second.id
                        |> moveInto parent.id first.id
                        -----
                        |> nextNode first.id
                        |> Expect.equal (Just second.id)
            , test "finds a parent's sibling if there are siblings" <|
                \_ ->
                    let
                        ( parent, ( first, ( second, ( child, database ) ) ) ) =
                            insert_ (Node.title (plainContent "parent")) emptyFixture
                                |> Tuple.mapSecond (insert_ (Node.title (plainContent "first")))
                                |> Tuple.mapSecond (Tuple.mapSecond (insert_ (Node.title (plainContent "second"))))
                                |> Tuple.mapSecond (Tuple.mapSecond (Tuple.mapSecond (insert_ (Node.title (plainContent "child")))))
                    in
                    database
                        |> moveInto parent.id second.id
                        |> moveInto parent.id first.id
                        |> moveInto first.id child.id
                        -----
                        |> nextNode child.id
                        |> Expect.equal (Just second.id)
            , test "doesn't find anything if it's the only node" <|
                \_ ->
                    let
                        ( { id }, database ) =
                            insert_ (Node.title (plainContent "parent")) emptyFixture
                    in
                    database
                        |> nextNode id
                        |> Expect.equal Nothing
            , test "doesn't find anything if it's the last node" <|
                \_ ->
                    let
                        ( parent, ( first, ( second, database ) ) ) =
                            insert_ (Node.title (plainContent "parent")) emptyFixture
                                |> Tuple.mapSecond (insert_ (Node.title (plainContent "first")))
                                |> Tuple.mapSecond (Tuple.mapSecond (insert_ (Node.title (plainContent "second"))))
                    in
                    database
                        |> moveInto parent.id second.id
                        |> moveInto parent.id first.id
                        -----
                        |> nextNode second.id
                        |> Expect.equal Nothing
            ]
        , describe "updating nodes"
            [ test "I can update a node's content" <|
                \_ ->
                    let
                        ( { id }, database ) =
                            insert_ (Node.title Content.empty) emptyFixture
                    in
                    database
                        |> update_ id (Node.setContent (plainContent "Hey!"))
                        |> get id
                        |> Maybe.map (Node.content << .node)
                        |> Expect.equal (Just (plainContent "Hey!"))
            , test "trying to update a non-existent node doesn't change anything" <|
                \_ ->
                    emptyFixture
                        |> update_ (ID.fromInt 1000) (Node.setContent (plainContent "Hey!"))
                        |> Expect.equal emptyFixture
            , test "doing a no-op update doesn't change anything" <|
                \_ ->
                    let
                        ( { id }, database ) =
                            insert_ (Node.title Content.empty) emptyFixture
                    in
                    database
                        |> update_ id identity
                        |> Expect.equal database
            , test "updating a node marks it to be persisted" <|
                \_ ->
                    let
                        ( row, ( _, database ) ) =
                            insert_ (Node.title Content.empty) emptyFixture
                                |> Tuple.mapSecond toPersist
                    in
                    database
                        |> update_ row.id (Node.setContent (plainContent "Hey!"))
                        |> toPersist
                        |> Tuple.first
                        |> List.map .id
                        |> Expect.equal [ row.id ]
            , test "updating a title updates any links to that note as well" <|
                \_ ->
                    let
                        ( title, ( linker, database ) ) =
                            emptyFixture
                                |> insert_ (Node.title (plainContent "the original title"))
                                |> Tuple.mapSecond (insert_ (Node.node (noteLink "the original title")))
                    in
                    database
                        |> update_ title.id (Node.setContent (plainContent "the new title"))
                        |> get linker.id
                        |> Maybe.map (Node.content << .node)
                        |> Expect.equal (Just (noteLink "the new title"))
            ]
        , describe "filter nodes"
            [ test "if my filter never matches, the list will be empty" <|
                \_ ->
                    let
                        ( id, database ) =
                            insert_ (Node.title (plainContent "hey")) emptyFixture
                    in
                    database
                        |> filter (always False)
                        |> Expect.equal []
            , test "only nodes matching my filter will be returned" <|
                \_ ->
                    let
                        ( id, ( _, database ) ) =
                            emptyFixture
                                |> insert_ (Node.title (plainContent "yes"))
                                |> Tuple.mapSecond (insert_ (Node.node (plainContent "no")))
                    in
                    database
                        |> filter Node.isTitle
                        |> List.map .node
                        |> Expect.equal [ Node.title (plainContent "yes") ]
            ]
        , describe "deleting nodes"
            [ test "should make them non-gettable" <|
                \_ ->
                    let
                        ( { id }, database ) =
                            insert_ (Node.title (plainContent "hey")) emptyFixture
                    in
                    database
                        |> delete id
                        |> get id
                        |> Expect.equal Nothing
            , test "should remove nodes from their parent" <|
                \_ ->
                    let
                        ( parent, ( child, database ) ) =
                            insert_ (Node.title (plainContent "hey")) emptyFixture
                                |> Tuple.mapSecond (insert_ (Node.node (plainContent "bye!")))
                    in
                    database
                        |> moveInto parent.id child.id
                        |> delete child.id
                        |> get parent.id
                        |> Maybe.map .children
                        |> Expect.equal (Just [])
            , test "when given an invalid ID, should do nothing" <|
                \_ ->
                    let
                        ( id, database ) =
                            insert_ (Node.title (plainContent "hey")) emptyFixture
                    in
                    database
                        |> delete (ID.fromInt 1000)
                        |> Expect.equal database
            ]
        , describe "persisting nodes"
            [ test "toPersist should give a list of things that need to be persisted" <|
                \_ ->
                    let
                        ( node, database ) =
                            insert_ (Node.title (plainContent "hey")) emptyFixture
                    in
                    toPersist database
                        |> Tuple.first
                        |> Expect.equal [ node ]
            , test "toPersist should clear the list oof things to be persisted" <|
                \_ ->
                    let
                        ( node, database ) =
                            insert_ (Node.title (plainContent "hey")) emptyFixture
                    in
                    database
                        |> toPersist
                        |> Tuple.second
                        |> toPersist
                        |> Tuple.first
                        |> Expect.equal []
            ]
        ]


emptyFixture : Database
emptyFixture =
    empty (Random.initialSeed 0)


plainContent : String -> Content
plainContent string =
    Content.fromList [ Content.text string ]


noteLink : String -> Content
noteLink string =
    Content.fromList [ Content.noteLink [ Content.text string ] ]


insert_ : Node -> Database -> ( Row, Database )
insert_ =
    insert (Time.millisToPosix 0)


update_ : ID -> (Node -> Node) -> Database -> Database
update_ =
    update (Time.millisToPosix 1)
