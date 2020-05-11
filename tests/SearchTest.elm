module SearchTest exposing (..)

import Dict
import Expect
import Search exposing (..)
import Set
import Test exposing (..)


type alias Doc =
    { id : Int
    , content : String
    }


emptyIndex : Index Int Doc
emptyIndex =
    init
        { ref = .id
        , fields = [ .content ]
        }


searchTest : Test
searchTest =
    describe "Search"
        [ test "searching for a term not present in the index gets no results" <|
            \_ ->
                emptyIndex
                    |> search "test"
                    |> Expect.equal Dict.empty
        , test "indexing a node makes it searchable" <|
            \_ ->
                emptyIndex
                    |> index { id = 1, content = "test" }
                    |> search "test"
                    |> Expect.equal (Dict.singleton 1 (Set.fromList [ ( 0, 4 ) ]))
        , test "searching returns only relevant matches" <|
            \_ ->
                emptyIndex
                    |> index { id = 1, content = "one" }
                    |> index { id = 2, content = "two" }
                    |> search "one"
                    |> Expect.equal (Dict.singleton 1 (Set.fromList [ ( 0, 3 ) ]))
        , test "re-indexing a document removes old terms from the index" <|
            \_ ->
                emptyIndex
                    |> index { id = 1, content = "one" }
                    |> index { id = 1, content = "two" }
                    |> search "one"
                    |> Expect.equal Dict.empty
        ]
