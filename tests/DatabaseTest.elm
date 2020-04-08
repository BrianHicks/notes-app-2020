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
                        |> Maybe.map Tuple.first
                        |> Maybe.map Node.content
                        |> Expect.equal (Just "hey")
            , test "getting a node that doesn't exist returns Nothing" <|
                \_ -> Expect.equal Nothing (get (idFromInt 0) empty)
            ]
        , describe "appending a child"
            [ test "shows the relationship in children" <|
                \_ ->
                    let
                        ( parent, ( child, database ) ) =
                            empty
                                |> insert (Node.note "parent")
                                -- TODO: this shouldn't be a note, though?
                                |> Tuple.mapSecond (insert (Node.note "child"))
                    in
                    database
                        |> appendChild parent child
                        |> get parent
                        |> Maybe.map Tuple.second
                        |> Expect.equal (Just (Array.fromList [ child ]))
            , test "will not append a node to itself" <|
                \_ ->
                    let
                        ( id, database ) =
                            insert (Node.note "note") empty
                    in
                    database
                        |> appendChild id id
                        |> get id
                        |> Maybe.map Tuple.second
                        |> Expect.equal (Just Array.empty)
            ]
        , describe "deleting"
            [ test "deleting a node should remove it from the database" <|
                \_ ->
                    let
                        ( id, database ) =
                            insert (Node.note "note") empty
                    in
                    database
                        |> delete id
                        |> get id
                        |> Expect.equal Nothing
            ]
        ]
