module NodeTest exposing (..)

import Expect
import Node exposing (..)
import Test exposing (..)


nodeTest : Test
nodeTest =
    describe "node"
        [ describe "fromString"
            [ test "uses the content passed in" <|
                \_ ->
                    fromString "Hey there!"
                        |> content
                        |> Expect.equal "Hey there!"
            ]
        , describe "isNote"
            [ test "is not true for a default note" <|
                \_ ->
                    fromString "I'm not a note!"
                        |> isNote
                        |> Expect.equal False
            , test "is true when a node is explicitly marekd as a note" <|
                \_ ->
                    fromString "I'm a note!"
                        |> asNote
                        |> isNote
                        |> Expect.equal True
            ]
        ]
