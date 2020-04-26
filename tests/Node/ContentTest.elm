module Node.ContentTest exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import Node.Content exposing (..)
import Test exposing (..)


contentTest : Test
contentTest =
    describe "Content"
        [ describe "fromList"
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
        , fuzz contentFuzzer "toString and fromString roundtrip successfully" <|
            \content -> content |> toString |> fromString |> Expect.equal (Ok content)
        ]



-- FUZZERS


contentFuzzer : Fuzzer Content
contentFuzzer =
    Fuzz.oneOf
        [ Fuzz.map text simpleStringFuzzer ]
        |> nonEmptyShortList
        |> Fuzz.map fromList


simpleStringFuzzer : Fuzzer String
simpleStringFuzzer =
    Fuzz.oneOf
        [ Fuzz.constant "a"
        , Fuzz.constant " "
        , Fuzz.constant "\n"
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
