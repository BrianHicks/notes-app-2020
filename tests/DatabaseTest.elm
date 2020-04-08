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
                        |> Maybe.map Node.content
                        |> Expect.equal (Just "hey")
            , test "getting a node that doesn't exist returns Nothing" <|
                \_ -> Expect.equal Nothing (get (idFromInt 0) empty)
            ]
        ]
