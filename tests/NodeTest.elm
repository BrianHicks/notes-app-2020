module NodeTest exposing (..)

import Expect
import Node exposing (..)
import Test exposing (..)


nodeTest : Test
nodeTest =
    describe "node"
        [ describe "note builder"
            [ test "creates a note with the given content" <|
                \_ ->
                    note "Hey there!"
                        |> content
                        |> Expect.equal "Hey there!"
            , test "is a note" <|
                \_ ->
                    note "Hey there!"
                        |> isNote
                        |> Expect.equal True
            ]
        , describe "node builder"
            [ test "creates a node with the given content" <|
                \_ ->
                    node "Hey there!"
                        |> content
                        |> Expect.equal "Hey there!"
            , test "is not a note" <|
                \_ ->
                    node "Hey there!"
                        |> isNote
                        |> Expect.equal False
            ]
        , describe "setting content"
            [ test "setContent sets the content" <|
                \_ ->
                    note "Hey there!"
                        |> setContent "Bye!"
                        |> content
                        |> Expect.equal "Bye!"
            ]
        ]
