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
                    startAt rootNode baseDatabase
                        |> Expect.notEqual Nothing
            , test "I can't get a zipper from an empty database" <|
                \_ ->
                    startAt (Database.idFromInt 0) Database.empty
                        |> Expect.equal Nothing
            , test "I can't get a zipper starting at a missing ID" <|
                \_ ->
                    startAt (Database.idFromInt 1000) baseDatabase
                        |> Expect.equal Nothing
            ]
        ]


base =
    Database.insert (Node.note "Note") Database.empty


rootNode =
    Tuple.first base


baseDatabase =
    Tuple.second base
