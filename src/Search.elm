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
        , forward : Dict ref (List Stem)
        , reverse : Dict String (Set ref)
        }


init : { ref : doc -> ref, sorter : Sorter ref, toString : doc -> String } -> Index ref doc
init config =
    Index
        { ref = config.ref
        , sorter = config.sorter
        , toString = config.toString
        , forward = Dict.empty config.sorter
        , reverse = Dict.empty Sort.alphabetical
        }



-- SEARCH


search : String -> Index ref doc -> Dict ref (Set ( Int, Int ))
search term ((Index idx) as outer) =
    let
        termStems =
            stems term
    in
    case termStems of
        [] ->
            Dict.empty idx.sorter

        [ { stem } ] ->
            Set.foldl
                (\item soFar -> Dict.insert item (Set.empty (Sort.custom Basics.compare)) soFar)
                (Dict.empty idx.sorter)
                (searchForStem stem outer)

        firstStem :: rest ->
            let
                subsequence =
                    -- currying! This returns a partially applied function
                    -- to use in `containsSubsequence` below.
                    List.map (\term_ doc -> term_.stem == doc.stem) termStems
            in
            rest
                |> List.foldl
                    (\newStem soFar -> Set.keepIf (Set.memberOf (searchForStem newStem.stem outer)) soFar)
                    (searchForStem firstStem.stem outer)
                |> Set.foldl
                    (\item soFar ->
                        case Maybe.map (containsSubsequence subsequence) (Dict.get item idx.forward) of
                            Just True ->
                                Dict.insert item (Set.empty (Sort.custom Basics.compare)) soFar

                            _ ->
                                soFar
                    )
                    (Dict.empty idx.sorter)


searchForStem : String -> Index ref doc -> Set ref
searchForStem stem (Index idx) =
    Dict.get stem idx.reverse
        |> Maybe.withDefault (Set.empty idx.sorter)


containsSubsequence : List (a -> Bool) -> List a -> Bool
containsSubsequence conds items =
    case ( conds, items ) of
        ( [], _ ) ->
            True

        ( _, [] ) ->
            False

        ( cond :: restConds, item :: restItems ) ->
            if cond item then
                containsSubsequence restConds restItems

            else
                containsSubsequence conds restItems



-- INDEX


index : a -> Index comparable a -> Index comparable a
index doc (Index idx) =
    let
        (Index clean) =
            remove doc (Index idx)

        docStems =
            stems (idx.toString doc)

        ref =
            idx.ref doc
    in
    Index
        { clean
            | forward = Dict.insert ref docStems clean.forward
            , reverse =
                List.foldl
                    (\{ stem, start, end } ->
                        Dict.update stem
                            (\maybeRefs ->
                                case maybeRefs of
                                    Just refs ->
                                        Just (Set.insert ref refs)

                                    Nothing ->
                                        Just (Set.singleton idx.sorter ref)
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
            | forward = Dict.remove ref idx.forward
            , reverse = Dict.map (\term refs -> Set.remove ref refs) idx.reverse
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
