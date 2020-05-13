module ContentTest exposing (..)

import Content exposing (..)
import Expect
import Fuzz exposing (Fuzzer)
import Html.Attributes as Attrs
import Html.Styled as Html
import Route
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector


contentTest : Test
contentTest =
    describe "Content"
        [ describe "isEmpty"
            [ test "is true for an empty content" <|
                \_ ->
                    empty
                        |> isEmpty
                        |> Expect.equal True
            , test "is not true for a non-empty content" <|
                \_ ->
                    fromList [ text "hey" ]
                        |> isEmpty
                        |> Expect.equal False
            ]
        , describe "fromList"
            [ test "concatenates adjacent plain text nodes" <|
                \_ ->
                    fromList [ text "one ", text "two" ]
                        |> toList
                        |> Expect.equal [ text "one two" ]
            ]
        , describe "plain text"
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
        , describe "note links"
            [ test "look like [[bracketed text]]" <|
                \_ ->
                    fromList [ noteLink [ text "Note Title" ] ]
                        |> toString
                        |> Expect.equal "[[Note Title]]"
            , test "is retrievable from a string" <|
                \_ ->
                    fromString "[[Note Title]]"
                        |> Result.map toList
                        |> Expect.equal (Ok [ noteLink [ text "Note Title" ] ])
            , test "can be nested" <|
                \_ ->
                    fromString "[[before [[nesting]] after]]"
                        |> Result.map toList
                        |> Expect.equal
                            (Ok
                                [ noteLink
                                    [ text "before "
                                    , noteLink [ text "nesting" ]
                                    , text " after"
                                    ]
                                ]
                            )
            ]
        , describe "external links"
            [ test "look like [markdown links](https://www.google.com)" <|
                \_ ->
                    fromString "[markdown links](https://www.google.com)"
                        |> Result.map toList
                        |> Expect.equal (Ok [ link { children = [ text "markdown links" ], href = "https://www.google.com" } ])
            ]
        , describe "splitAt"
            [ test "splits text correctly" <|
                \_ ->
                    fromList [ text "baseball" ]
                        |> splitAt 4
                        |> Expect.equal
                            ( fromList [ text "base" ]
                            , fromList [ text "ball" ]
                            )
            , test "splits note links correctly" <|
                \_ ->
                    fromList [ noteLink [ text "baseball" ] ]
                        |> splitAt 4
                        |> Expect.equal
                            ( fromList [ noteLink [ text "base" ] ]
                            , fromList [ noteLink [ text "ball" ] ]
                            )
            , test "splits links correctly" <|
                \_ ->
                    fromList [ link { children = [ text "baseball" ], href = "https://bytes.zone" } ]
                        |> splitAt 4
                        |> Expect.equal
                            ( fromList [ link { children = [ text "base" ], href = "https://bytes.zone" } ]
                            , fromList [ link { children = [ text "ball" ], href = "https://bytes.zone" } ]
                            )
            , fuzz2 (Fuzz.intRange -1 0) contentFuzzer "no change for a split at or below 0" <|
                \splitPoint content ->
                    content
                        |> splitAt splitPoint
                        |> Expect.equal ( fromList [], content )
            , test "splits correctly at boundaries" <|
                \_ ->
                    fromList [ noteLink [ text "a" ], noteLink [ text "b" ] ]
                        |> splitAt 1
                        |> Expect.equal
                            ( fromList [ noteLink [ text "a" ] ]
                            , fromList [ noteLink [ text "b" ] ]
                            )
            , fuzz2 (Fuzz.intRange 1 10) (Fuzz.intRange 1 10) "splitting plain text behaves the same as the String module" <|
                \splitPoint contentLength ->
                    let
                        content =
                            String.concat (List.repeat contentLength "a")
                    in
                    fromList [ text content ]
                        |> splitAt splitPoint
                        |> Expect.equal
                            ( fromList [ text (String.left splitPoint content) ]
                            , fromList [ text (String.dropLeft splitPoint content) ]
                            )
            ]
        , fuzz contentFuzzer "toString and fromString roundtrip successfully" <|
            \content -> content |> toString |> fromString |> Expect.equal (Ok content)
        , describe "toHtml"
            [ test "clicking the result activates the content" <|
                \_ ->
                    fromList [ text "Hello" ]
                        |> toHtml { activate = "Clicked", navigate = \_ -> "Navigate" } []
                        |> Html.toUnstyled
                        |> Query.fromHtml
                        |> Query.find
                            [ Selector.tag "button"
                            , Selector.containing [ Selector.text "Hello" ]
                            ]
                        |> Event.simulate Event.click
                        |> Event.expect "Clicked"
            , test "renders clickable links" <|
                \_ ->
                    fromList [ link { children = [ text "bytes.zone" ], href = "https://bytes.zone" } ]
                        |> toHtml { activate = (), navigate = \_ -> () } []
                        |> Html.toUnstyled
                        |> Query.fromHtml
                        |> Query.has
                            [ Selector.tag "a"
                            , Selector.attribute (Attrs.href "https://bytes.zone")
                            , Selector.containing [ Selector.text "bytes.zone" ]
                            ]
            , test "renders clickable note links" <|
                \_ ->
                    let
                        title =
                            text "A Test Note!"
                    in
                    fromList [ noteLink [ title ] ]
                        |> toHtml { activate = (), navigate = \_ -> () } []
                        |> Html.toUnstyled
                        |> Query.fromHtml
                        |> Query.has
                            [ Selector.tag "button"
                            , Selector.containing [ Selector.text (toString (fromList [ title ])) ]
                            ]
            ]
        ]



-- FUZZERS


contentFuzzer : Fuzzer Content
contentFuzzer =
    Fuzz.oneOf
        [ Fuzz.map text simpleStringFuzzer
        , Fuzz.map (\text_ -> noteLink [ text text_ ]) simpleStringFuzzer
        , Fuzz.map2
            (\text_ href -> link { children = [ text text_ ], href = href })
            simpleStringFuzzer
            simpleStringFuzzer
        ]
        |> nonEmptyShortList
        |> Fuzz.map fromList


simpleStringFuzzer : Fuzzer String
simpleStringFuzzer =
    Fuzz.oneOf
        [ Fuzz.constant "a"
        , Fuzz.constant "b"
        , Fuzz.constant " "
        ]
        |> nonEmptyShortList
        |> Fuzz.map String.concat


nonEmptyShortList : Fuzzer a -> Fuzzer (List a)
nonEmptyShortList fuzzer =
    Fuzz.oneOf
        [ Fuzz.map (\a -> [ a ]) fuzzer
        , Fuzz.map2 (\a b -> [ a, b ]) fuzzer fuzzer
        , Fuzz.map3 (\a b c -> [ a, b, c ]) fuzzer fuzzer fuzzer
        ]
