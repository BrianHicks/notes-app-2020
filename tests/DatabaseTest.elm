module DatabaseTest exposing (..)

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
                    |> Expect.equal 0
        , test "you can get a node again" <|
            \_ ->
                init
                    |> insert (Node.fromString "hey")
                    |> Tuple.second
                    |> get 0
                    |> Maybe.map .content
                    |> Expect.equal (Just "hey")
        ]
