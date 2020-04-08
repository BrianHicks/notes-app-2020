module NodeTest exposing (..)

import Expect
import Node exposing (..)
import Test exposing (..)


nodeTest : Test
nodeTest =
    describe "node"
        [ describe "fromString"
            [ test "you can create a node from a string" <|
                \_ ->
                    fromString "Hey there!"
                        |> .content
                        |> Expect.equal "Hey there!"
            , test "sets no metadata by default" <|
                \_ ->
                    fromString "Hey there!"
                        |> .metadata
                        |> Expect.equal Nothing
            ]
        , describe "withMetadata"
            [ test "sets metadata" <|
                \_ ->
                    fromString "Hey there!"
                        |> withMetadata Note
                        |> .metadata
                        |> Expect.equal (Just Note)
            ]
        ]
