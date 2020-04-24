module Database.ZipperTest exposing (..)

import Database
import Database.Zipper exposing (..)
import Expect
import Node
import Test exposing (..)


zipperTest : Test
zipperTest =
    describe "zipper"
        [ describe "getting a zipper"
            [ test "I can start at a node in a Database" <|
                \_ ->
                    startAt rootID rootDatabase
                        |> Expect.ok
            , test "I can't get a zipper from an empty database" <|
                \_ ->
                    startAt rootID Database.empty
                        |> Expect.err
            , test "I can't get a zipper starting at a missing ID" <|
                \_ ->
                    startAt (Database.idFromInt 1000) rootDatabase
                        |> Expect.err
            ]
        , case startAt rootID rootDatabase of
            Err problem ->
                todo ("I need a base zipper to test navigation, but I couldn't construct one: " ++ Debug.toString problem)

            Ok zipper ->
                afterInitializationTest zipper
        ]


afterInitializationTest : Zipper -> Test
afterInitializationTest zipper =
    describe "after initialization"
        [ gettingInformationTest zipper
        , modifyingTest zipper
        ]


gettingInformationTest : Zipper -> Test
gettingInformationTest zipper =
    describe "getting information"
        [ test "can get the ID" <|
            \_ -> Expect.equal rootID (id zipper)
        , test "I can get the node" <|
            \_ -> Expect.equal rootNode (node zipper)
        ]


modifyingTest : Zipper -> Test
modifyingTest zipper =
    describe "modifying the database"
        [ describe "updating the node"
            [ test "changes the zipper" <|
                \_ ->
                    zipper
                        |> update (Node.setContent "Hey!") rootDatabase
                        |> Result.map Tuple.first
                        |> Result.map node
                        |> Result.map Node.content
                        |> Expect.equal (Ok "Hey!")
            ]

        -- \_ ->
        --     let
        --         ( newZipper, database ) =
        --             update
        --     in
        --     database
        --         |> Database.get (id zipper)
        --         |> Maybe.map .node
        --         |> Maybe.map Node.content
        --             Expect.equal
        --             (Just "hey!")
        ]


rootNode =
    Node.note "Note"


base =
    Database.insert rootNode Database.empty


rootID =
    Tuple.first base


rootDatabase =
    Tuple.second base
