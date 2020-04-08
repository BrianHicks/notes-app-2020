module NodeTest exposing (..)

import Expect
import Node exposing (..)
import Test exposing (..)


nodeTest : Test
nodeTest =
    describe "node"
        [ describe "note"
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
        ]
