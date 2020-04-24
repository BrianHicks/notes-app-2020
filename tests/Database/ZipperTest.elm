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
                        |> Expect.notEqual Nothing
            , test "I can't get a zipper from an empty database" <|
                \_ ->
                    startAt (Database.idFromInt 0) Database.empty
                        |> Expect.equal Nothing
            , test "I can't get a zipper starting at a missing ID" <|
                \_ ->
                    startAt (Database.idFromInt 1000) rootDatabase
                        |> Expect.equal Nothing
            ]
        , case startAt rootID rootDatabase of
            Nothing ->
                todo "I need a base zipper to test navigation, but I couldn't construct one."

            Just zipper ->
                afterInitializationTest zipper
        ]


afterInitializationTest : Zipper -> Test
afterInitializationTest zipper =
    describe "after initialization"
        [ gettingInformationTest zipper ]


gettingInformationTest : Zipper -> Test
gettingInformationTest zipper =
    describe "getting information"
        [ test "can get the ID" <|
            \_ -> Expect.equal rootID (id zipper)
        , test "I can get the node" <|
            \_ -> Expect.equal rootNode (node zipper)
        ]


rootNode =
    Node.note "Note"


base =
    Database.insert rootNode Database.empty


rootID =
    Tuple.first base


rootDatabase =
    Tuple.second base
