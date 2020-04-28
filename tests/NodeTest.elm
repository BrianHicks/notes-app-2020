module NodeTest exposing (..)

import Expect
import Node exposing (..)
import Node.Content as Content exposing (Content)
import Test exposing (..)


nodeTest : Test
nodeTest =
    describe "node"
        [ describe "note builder"
            [ test "creates a note with the given content" <|
                \_ ->
                    note (plainContent "Hey there!")
                        |> content
                        |> Expect.equal (plainContent "Hey there!")
            , test "is a note" <|
                \_ ->
                    note (plainContent "Hey there!")
                        |> isNote
                        |> Expect.equal True
            ]
        , describe "node builder"
            [ test "creates a node with the given content" <|
                \_ ->
                    node (plainContent "Hey there!")
                        |> content
                        |> Expect.equal (plainContent "Hey there!")
            , test "is not a note" <|
                \_ ->
                    node (plainContent "Hey there!")
                        |> isNote
                        |> Expect.equal False
            ]
        , describe "setting content"
            [ test "setContent sets the content" <|
                \_ ->
                    note (plainContent "Hey there!")
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
