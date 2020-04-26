module Node.ContentTest exposing (..)

import Expect
import Node.Content exposing (..)
import Test exposing (..)


contentTest : Test
contentTest =
    describe "Content"
        [ describe "plain text"
            [ test "I can create from a string" <|
                \_ ->
                    fromString "Hey there!"
                        |> Result.map toList
                        |> Expect.equal (Ok [ text "Hey there!" ])
            , test "I can serialize content to a string" <|
                \_ ->
                    fromList [ text "Hey there!" ]
                        |> toString
                        |> Expect.equal "Hey there!"
            ]
        ]
