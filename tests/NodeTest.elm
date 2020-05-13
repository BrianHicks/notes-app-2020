module NodeTest exposing (..)

import Content exposing (Content)
import Expect
import Node exposing (..)
import Test exposing (..)


nodeTest : Test
nodeTest =
    describe "node"
        [ describe "title builder"
            [ test "creates a title with the given content" <|
                \_ ->
                    title (plainContent "Hey there!")
                        |> content
                        |> Expect.equal (plainContent "Hey there!")
            , test "is a title" <|
                \_ ->
                    title (plainContent "Hey there!")
                        |> isTitle
                        |> Expect.equal True
            ]
        , describe "node builder"
            [ test "creates a node with the given content" <|
                \_ ->
                    node (plainContent "Hey there!")
                        |> content
                        |> Expect.equal (plainContent "Hey there!")
            , test "is not a title" <|
                \_ ->
                    node (plainContent "Hey there!")
                        |> isTitle
                        |> Expect.equal False
            ]
        , describe "setting content"
            [ test "setContent sets the content" <|
                \_ ->
                    title (plainContent "Hey there!")
                        |> setContent (plainContent "Bye!")
                        |> content
                        |> Expect.equal (plainContent "Bye!")
            ]
        , describe "isEmpty"
            [ test "is true when there's no content" <|
                \_ ->
                    node Content.empty
                        |> isEmpty
                        |> Expect.equal True
            , test "is false when there's content" <|
                \_ ->
                    node (plainContent "hey")
                        |> isEmpty
                        |> Expect.equal False
            ]
        ]


plainContent : String -> Content
plainContent string =
    Content.fromList [ Content.text string ]
