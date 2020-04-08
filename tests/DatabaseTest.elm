module DatabaseTest exposing (..)

import Array
import Database exposing (..)
import Expect
import Node
import Test exposing (..)


databaseTest : Test
databaseTest =
    describe "Database"
        [ test "starts with a new, empty database" <|
            \_ -> Expect.equal True (isEmpty init)
        , test "you can insert a node" <|
            \_ ->
                init
                    |> insert (Node.fromString "hey")
                    |> Tuple.second
                    |> isEmpty
                    |> Expect.equal False
        , test "inserting assigns an ID" <|
            \_ ->
                init
                    |> insert (Node.fromString "hey")
                    |> Tuple.first
                    |> Expect.equal (Node.ID 0)
        , test "you can get a node again" <|
            \_ ->
                let
                    ( id, database ) =
                        insert (Node.fromString "hey") init
                in
                database
                    |> get id
                    |> Maybe.map .content
                    |> Expect.equal (Just "hey")
        , describe "notes"
            [ test "you can get a list of notes" <|
                \_ ->
                    let
                        note =
                            Node.fromString "hey" |> Node.withMetadata Node.Note

                        ( id, database ) =
                            insert note init
                    in
                    database
                        |> notes
                        |> List.map .content
                        |> Expect.equal [ note.content ]
            , test "does not include non-note nodes" <|
                \_ ->
                    init
                        |> insert (Node.fromString "hey")
                        |> Tuple.second
                        |> notes
                        |> Expect.equal []
            ]
        ]
