module NodeTest exposing (..)

import Expect
import Node exposing (..)
import Test exposing (..)


nodeTest : Test
nodeTest =
    describe "node"
        [ test "you can create a node from a string" <|
            \_ ->
                fromString "Hey there!"
                    |> .content
                    |> Expect.equal "Hey there!"
        ]
