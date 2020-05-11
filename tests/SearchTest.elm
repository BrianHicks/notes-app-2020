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
        [ test "indexing a node makes it searchable" <|
            \_ ->
                emptyIndex
                    |> index { id = 1, content = "test testing tested" }
                    |> search "test"
                    |> Expect.equal (Dict.singleton 1 (Set.fromList [ ( 0, 4 ), ( 5, 12 ), ( 13, 19 ) ]))
        ]
