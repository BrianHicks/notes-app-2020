module DatabaseTest exposing (..)

import Array
import Database exposing (..)
import Expect
import Node
import Test exposing (..)


databaseTest : Test
databaseTest =
    describe "Database"
        [ describe "empty"
            [ test "starts with a empty database" <|
                \_ -> Expect.equal True (isEmpty empty)
            ]
        , describe "insert"
            [ test "inserts a node" <|
                \_ ->
                    empty
                        |> insert (Node.note "hey")
                        |> Tuple.second
                        |> isEmpty
                        |> Expect.equal False
            , test "assigns an ID" <|
                \_ ->
                    empty
                        |> insert (Node.note "hey")
                        |> Tuple.first
                        |> Expect.equal (idFromInt 0)
            , test "starts without children" <|
                \_ ->
                    empty
                        |> insert (Node.note "hey")
                        |> untuple get
                        |> Maybe.map .children
                        |> Expect.equal (Just [])
            , test "starts without a parent" <|
                \_ ->
                    empty
                        |> insert (Node.note "hey")
                        |> untuple get
                        |> Maybe.map .parent
                        |> Expect.equal (Just Nothing)
            ]
        , describe "get"
            [ test "you can get a node again" <|
                \_ ->
                    let
                        ( id, database ) =
                            insert (Node.note "hey") empty
                    in
                    database
                        |> get id
                        |> Maybe.map (.node >> Node.content)
                        |> Expect.equal (Just "hey")
            , test "getting a node that doesn't exist returns Nothing" <|
                \_ -> Expect.equal Nothing (get (idFromInt 0) empty)
            ]
        , describe "moving into a parent"
            [ test "shows the relationship in .children" <|
                \_ ->
                    let
                        ( parent, ( child, database ) ) =
                            empty
                                |> insert (Node.note "parent")
                                |> Tuple.mapSecond (insert (Node.node "child"))
                    in
                    database
                        |> moveInto parent child
                        |> get parent
                        |> Maybe.map .children
                        |> Expect.equal (Just [ child ])
            , test "shows the relationship in .parent" <|
                \_ ->
                    let
                        ( parent, ( child, database ) ) =
                            empty
                                |> insert (Node.note "parent")
                                |> Tuple.mapSecond (insert (Node.node "child"))
                    in
                    database
                        |> moveInto parent child
                        |> get child
                        |> Maybe.map .parent
                        |> Expect.equal (Just (Just parent))
            , test "will not append a node to itself" <|
                \_ ->
                    let
                        ( id, database ) =
                            insert (Node.note "note") empty
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
                            empty
                                |> insert (Node.note "sibling A")
                                |> Tuple.mapSecond (insert (Node.note "sibling B"))

                        ( child, database ) =
                            insert (Node.node "child") dbTemp
                    in
                    database
                        |> moveInto siblingA child
                        |> moveInto siblingB child
                        |> get siblingA
                        |> Maybe.map .children
                        |> Expect.equal (Just [])
            , test "will not insert a bad child ID" <|
                \_ ->
                    let
                        ( id, database ) =
                            insert (Node.note "note") empty
                    in
                    database
                        |> moveInto id (idFromInt 0x1BAD1DEA)
                        |> Expect.equal database
            , test "will not insert a bad parent ID" <|
                \_ ->
                    let
                        ( id, database ) =
                            insert (Node.note "note") empty
                    in
                    database
                        |> moveInto (idFromInt 0x1BAD1DEA) id
                        |> Expect.equal database
            ]
        , describe "moving after a sibling"
            [ test "will not move a node after itself" <|
                \_ ->
                    let
                        ( id, database ) =
                            insert (Node.note "note") empty
                    in
                    database
                        |> moveAfter id id
                        |> Expect.equal database
            , test "will not move after a missing node" <|
                \_ ->
                    let
                        ( id, database ) =
                            insert (Node.note "note") empty
                    in
                    database
                        |> moveAfter (idFromInt 1000) id
                        |> Expect.equal database
            , test "will not move a missing node" <|
                \_ ->
                    let
                        ( id, database ) =
                            insert (Node.note "note") empty
                    in
                    database
                        |> moveAfter id (idFromInt 1000)
                        |> Expect.equal database
            , test "will move directly after the sibling" <|
                \_ ->
                    let
                        ( parent, ( first, ( second, ( third, database ) ) ) ) =
                            insert (Node.note "parent") empty
                                |> Tuple.mapSecond (insert (Node.note "first"))
                                |> Tuple.mapSecond (Tuple.mapSecond (insert (Node.note "second")))
                                |> Tuple.mapSecond (Tuple.mapSecond (Tuple.mapSecond (insert (Node.note "third"))))
                    in
                    database
                        |> moveInto parent third
                        |> moveInto parent second
                        |> moveInto parent first
                        |> moveAfter second first
                        |> get parent
                        |> Maybe.map .children
                        |> Expect.equal (Just [ second, first, third ])
            ]
        , describe "finding a sibling"
            [ test "won't work for the only note in the database" <|
                \_ ->
                    let
                        ( id, database ) =
                            insert (Node.note "note") empty
                    in
                    database
                        |> previousSibling id
                        |> Expect.equal Nothing
            , test "won't work for the first node in a sibling group" <|
                \_ ->
                    let
                        ( parent, ( first, ( second, database ) ) ) =
                            insert (Node.note "parent") empty
                                |> Tuple.mapSecond (insert (Node.note "first"))
                                |> Tuple.mapSecond (Tuple.mapSecond (insert (Node.note "second")))
                    in
                    database
                        |> moveInto parent second
                        |> moveInto parent first
                        -----
                        |> previousSibling first
                        |> Expect.equal Nothing
            , test "will work for the second node in a sibling group" <|
                \_ ->
                    let
                        ( parent, ( first, ( second, database ) ) ) =
                            insert (Node.note "parent") empty
                                |> Tuple.mapSecond (insert (Node.note "first"))
                                |> Tuple.mapSecond (Tuple.mapSecond (insert (Node.note "second")))
                    in
                    database
                        |> moveInto parent second
                        |> moveInto parent first
                        -----
                        |> previousSibling second
                        |> Expect.equal (Just first)
            ]
        , describe "finding a node below"
            [ test "finds the next sibling, if one is present" <|
                \_ ->
                    let
                        ( parent, ( first, ( second, database ) ) ) =
                            insert (Node.note "parent") empty
                                |> Tuple.mapSecond (insert (Node.note "first"))
                                |> Tuple.mapSecond (Tuple.mapSecond (insert (Node.note "second")))
                    in
                    database
                        |> moveInto parent second
                        |> moveInto parent first
                        -----
                        |> nextNodeBelow first
                        |> Expect.equal (Just second)
            , test "finds a parent's sibling if there are siblings" <|
                \_ ->
                    let
                        ( parent, ( first, ( second, ( child, database ) ) ) ) =
                            insert (Node.note "parent") empty
                                |> Tuple.mapSecond (insert (Node.note "first"))
                                |> Tuple.mapSecond (Tuple.mapSecond (insert (Node.note "second")))
                                |> Tuple.mapSecond (Tuple.mapSecond (Tuple.mapSecond (insert (Node.note "child"))))
                    in
                    database
                        |> moveInto parent second
                        |> moveInto parent first
                        |> moveInto first child
                        -----
                        |> nextNodeBelow child
                        |> Expect.equal (Just second)
            , test "doesn't find anything if it's the only node" <|
                \_ ->
                    let
                        ( parent, database ) =
                            insert (Node.note "parent") empty
                    in
                    database
                        |> nextNodeBelow parent
                        |> Expect.equal Nothing
            , test "doesn't find anything if it's the last node" <|
                \_ ->
                    let
                        ( parent, ( first, ( second, database ) ) ) =
                            insert (Node.note "parent") empty
                                |> Tuple.mapSecond (insert (Node.note "first"))
                                |> Tuple.mapSecond (Tuple.mapSecond (insert (Node.note "second")))
                    in
                    database
                        |> moveInto parent second
                        |> moveInto parent first
                        -----
                        |> nextNodeBelow second
                        |> Expect.equal Nothing
            ]
        , describe "updating nodes"
            [ test "I can update a node's content" <|
                \_ ->
                    let
                        ( id, database ) =
                            insert (Node.note "") empty
                    in
                    database
                        |> update id (Node.setContent "Hey!")
                        |> get id
                        |> Maybe.map .node
                        |> Maybe.map Node.content
                        |> Expect.equal (Just "Hey!")
            , test "trying to update a non-existent node doesn't change anything" <|
                \_ ->
                    empty
                        |> update (idFromInt 1000) (Node.setContent "Hey!")
                        |> Expect.equal empty
            ]
        , describe "filter nodes"
            [ test "if my filter never matches, the list will be empty" <|
                \_ ->
                    let
                        ( id, database ) =
                            insert (Node.note "hey") empty
                    in
                    database
                        |> filter (always False)
                        |> Expect.equal []
            , test "only nodes matching my filter will be returned" <|
                \_ ->
                    let
                        ( id, ( _, database ) ) =
                            empty
                                |> insert (Node.note "yes")
                                |> Tuple.mapSecond (insert (Node.node "no"))
                    in
                    database
                        |> filter Node.isNote
                        |> List.map .node
                        |> Expect.equal [ Node.note "yes" ]
            ]
        , describe "deleting nodes"
            [ test "should make them non-gettable" <|
                \_ ->
                    let
                        ( id, database ) =
                            insert (Node.note "hey") empty
                    in
                    database
                        |> delete id
                        |> get id
                        |> Expect.equal Nothing
            , test "should remove nodes from their parent" <|
                \_ ->
                    let
                        ( parent, ( child, database ) ) =
                            insert (Node.note "hey") empty
                                |> Tuple.mapSecond (insert (Node.node "bye!"))
                    in
                    database
                        |> moveInto parent child
                        |> delete child
                        |> get parent
                        |> Maybe.map .children
                        |> Expect.equal (Just [])
            , test "when given an invalid ID, should do nothing" <|
                \_ ->
                    let
                        ( id, database ) =
                            insert (Node.note "hey") empty
                    in
                    database
                        |> delete (idFromInt 1000)
                        |> Expect.equal database
            ]
        ]


untuple : (a -> b -> c) -> ( a, b ) -> c
untuple fn ( a, b ) =
    fn a b
