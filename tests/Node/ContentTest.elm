module Node.ContentTest exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import Node.Content exposing (..)
import Test exposing (..)


contentTest : Test
contentTest =
    describe "Content"
        [ describe "isEmpty"
            [ test "is true for an empty content" <|
                \_ ->
                    fromList []
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
                    fromList [ noteLink "Note Title" ]
                        |> toString
                        |> Expect.equal "[[Note Title]]"
            , test "is retrievable from a string" <|
                \_ ->
                    fromString "[[Note Title]]"
                        |> Result.map toList
                        |> Expect.equal (Ok [ noteLink "Note Title" ])
            , test "can be nested" <|
                \_ ->
                    fromString "[[before [[nesting]] after]]"
                        |> Result.map toList
                        |> Expect.equal (Ok [ noteLink "before [[nesting]] after" ])
            , test "cannot contain newlines" <|
                \_ ->
                    fromString "[[\n]]"
                        |> Expect.err
            ]
        , describe "external links"
            [ test "look like [markdown links](https://www.google.com)" <|
                \_ ->
                    fromString "[markdown links](https://www.google.com)"
                        |> Result.map toList
                        |> Expect.equal (Ok [ link { text = "markdown links", href = "https://www.google.com" } ])
            ]
        , fuzz contentFuzzer "toString and fromString roundtrip successfully" <|
            \content -> content |> toString |> fromString |> Expect.equal (Ok content)
        ]



-- FUZZERS


contentFuzzer : Fuzzer Content
contentFuzzer =
    Fuzz.oneOf
        [ Fuzz.map text simpleStringFuzzer
        , Fuzz.map noteLink simpleStringFuzzer
        , Fuzz.map2 (\text_ href -> link { text = text_, href = href }) simpleStringFuzzer simpleStringFuzzer
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
