module Search exposing (Index, init, search, index)

{-|

@docs Index, init, search, index

-}

import Parser exposing ((|.), (|=), Parser)
import Sort exposing (Sorter)
import Sort.Dict as Dict exposing (Dict)
import Sort.Set as Set exposing (Set)
import Stemmer


type Index ref doc
    = Index
        { ref : doc -> ref
        , sorter : Sorter ref
        , toString : doc -> String
        , reverse : Dict String (Dict ref (Set ( Int, Int )))
        }


init : { ref : doc -> ref, sorter : Sorter ref, toString : doc -> String } -> Index ref doc
init config =
    Index
        { ref = config.ref
        , sorter = config.sorter
        , toString = config.toString
        , reverse = Dict.empty Sort.alphabetical
        }



-- SEARCH


search : String -> Index ref doc -> Dict ref (Set ( Int, Int ))
search term ((Index idx) as outer) =
    case stems term of
        [] ->
            Dict.empty idx.sorter

        [ { stem } ] ->
            searchForStem stem outer

        firstStem :: rest ->
            List.foldl
                (\newStem soFar ->
                    Dict.merge
                        idx.sorter
                        (\_ _ result -> result)
                        (\key left right progress ->
                            Dict.insert key
                                (Set.union (Sort.custom Basics.compare) left right)
                                progress
                        )
                        (\_ _ result -> result)
                        soFar
                        (searchForStem newStem.stem outer)
                        (Dict.empty idx.sorter)
                )
                (searchForStem firstStem.stem outer)
                rest


searchForStem : String -> Index ref doc -> Dict ref (Set ( Int, Int ))
searchForStem stem (Index idx) =
    Dict.get stem idx.reverse
        |> Maybe.withDefault (Dict.empty idx.sorter)



-- INDEX


index : a -> Index comparable a -> Index comparable a
index doc (Index idx) =
    let
        (Index clean) =
            remove doc (Index idx)

        docStems =
            stems (idx.toString doc)
    in
    -- AAAAAAAAAAAAAAAAAAAHHHHHHHHH REFACTOR ME AND ADD TYPES
    Index
        { clean
            | reverse =
                List.foldl
                    (\{ stem, start, end } ->
                        Dict.update stem
                            (\maybeRefToSpans ->
                                case maybeRefToSpans of
                                    Just refToSpans ->
                                        refToSpans
                                            |> Dict.update (idx.ref doc)
                                                (\maybeSpans ->
                                                    case maybeSpans of
                                                        Just spans ->
                                                            Just (Set.insert ( start, end ) spans)

                                                        Nothing ->
                                                            Just
                                                                (Set.singleton
                                                                    (Sort.custom Basics.compare)
                                                                    ( start, end )
                                                                )
                                                )
                                            |> Just

                                    Nothing ->
                                        Just
                                            (Dict.singleton idx.sorter
                                                (idx.ref doc)
                                                (Set.singleton (Sort.custom Basics.compare) ( start, end ))
                                            )
                            )
                    )
                    clean.reverse
                    docStems
        }



-- REMOVE


remove : a -> Index comparable a -> Index comparable a
remove doc (Index idx) =
    let
        ref =
            idx.ref doc
    in
    Index
        { idx
            | reverse =
                Dict.map
                    (\term refs ->
                        Dict.remove ref refs
                    )
                    idx.reverse
        }



-- GETTING SPANS


type alias Stem =
    { stem : String
    , start : Int
    , end : Int
    }


stems : String -> List Stem
stems source =
    case Parser.run stemsParser source of
        Ok hooray ->
            hooray

        Err err ->
            -- for tests:
            -- Debug.todo (Debug.toString err)
            []


stemsParser : Parser (List Stem)
stemsParser =
    Parser.loop [] stemsLoop


whitespaceChars : Set Char
whitespaceChars =
    Set.fromList (Sort.custom Basics.compare) [ '\u{000D}', '\n', '\t', ' ' ]


stemsLoop : List Stem -> Parser (Parser.Step (List Stem) (List Stem))
stemsLoop soFar =
    Parser.oneOf
        [ Parser.succeed (\_ -> Parser.Done (List.reverse soFar))
            |= Parser.end
        , Parser.succeed (\_ -> Parser.Loop soFar)
            |= chompAtLeastOne (Set.memberOf whitespaceChars)
        , Parser.succeed
            (\start word end ->
                Parser.Loop
                    ({ stem = Stemmer.stem word
                     , start = start
                     , end = end
                     }
                        :: soFar
                    )
            )
            |= Parser.getOffset
            |= Parser.getChompedString (chompAtLeastOne (\c -> not (Set.memberOf whitespaceChars c)))
            |= Parser.getOffset
        ]


chompAtLeastOne : (Char -> Bool) -> Parser ()
chompAtLeastOne cond =
    Parser.chompIf cond |. Parser.chompWhile cond
