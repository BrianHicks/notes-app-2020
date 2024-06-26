module SearchTest exposing (..)

import Expect
import Search exposing (..)
import Sort
import Sort.Dict as Dict
import Sort.Set as Set
import Test exposing (..)


type alias Doc =
    { id : Int
    , content : String
    }


emptyIndex : Index Int Doc
emptyIndex =
    init
        { ref = .id
        , sorter = Sort.increasing
        , toString = .content
        }


searchTest : Test
searchTest =
    describe "Search"
        [ test "searching for a term not present in the index gets no results" <|
            \_ ->
                emptyIndex
                    |> search "test"
                    |> Expect.equal []
        , test "indexing a node makes it searchable" <|
            \_ ->
                emptyIndex
                    |> index { id = 1, content = "test" }
                    |> search "test"
                    |> List.map .ref
                    |> Expect.equal [ 1 ]
        , test "searching returns only relevant matches" <|
            \_ ->
                emptyIndex
                    |> index { id = 1, content = "one" }
                    |> index { id = 2, content = "two" }
                    |> search "one"
                    |> List.map .ref
                    |> Expect.equal [ 1 ]
        , test "re-indexing a document removes old terms from the index" <|
            \_ ->
                emptyIndex
                    |> index { id = 1, content = "one" }
                    |> index { id = 1, content = "two" }
                    |> search "one"
                    |> List.map .ref
                    |> Expect.equal []
        , test "search terms must all match for a doc to be returned" <|
            \_ ->
                emptyIndex
                    |> index { id = 1, content = "one" }
                    |> index { id = 2, content = "two" }
                    |> index { id = 3, content = "one two" }
                    |> search "one two"
                    |> List.map .ref
                    |> Expect.equal [ 3 ]
        , test "search terms must match in order to be returned" <|
            \_ ->
                emptyIndex
                    |> index { id = 1, content = "one two" }
                    |> index { id = 2, content = "two one" }
                    |> search "one two"
                    |> List.map .ref
                    |> Expect.equal [ 1 ]
        ]
