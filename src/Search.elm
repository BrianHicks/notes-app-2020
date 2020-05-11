module Search exposing (Index, init, search, index)

{-|

@docs Index, init, search, index

-}

import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser)
import Set exposing (Set)
import Stemmer


type Index ref a
    = Index
        { ref : a -> ref
        , fields : List (a -> String)
        , reverse : Dict String (Dict ref (Set ( Int, Int )))
        }


init : { ref : a -> comparable, fields : List (a -> String) } -> Index comparable a
init config =
    Index
        { ref = config.ref
        , fields = config.fields
        , reverse = Dict.empty
        }



-- SEARCH


search : String -> Index ref a -> Dict ref (Set ( Int, Int ))
search term (Index idx) =
    case stems term of
        [] ->
            Dict.empty

        [ { stem } ] ->
            Dict.get stem idx.reverse
                |> Maybe.withDefault Dict.empty

        _ ->
            Debug.todo "eh"



-- INDEX


index : a -> Index comparable a -> Index comparable a
index doc (Index idx) =
    -- AAAAAAAAAAAAAAAAAAAHHHHHHHHH REFACTOR ME AND ADD TYPES
    Index
        { idx
            | reverse =
                List.foldl
                    (\extractor reverseOuter ->
                        doc
                            |> extractor
                            |> stems
                            |> List.foldl
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
                                                                        Just (Set.singleton ( start, end ))
                                                            )
                                                        |> Just

                                                Nothing ->
                                                    Just (Dict.singleton (idx.ref doc) (Set.singleton ( start, end )))
                                        )
                                )
                                reverseOuter
                    )
                    idx.reverse
                    idx.fields
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
    Set.fromList [ '\u{000D}', '\n', '\t', ' ' ]


stemsLoop : List Stem -> Parser (Parser.Step (List Stem) (List Stem))
stemsLoop soFar =
    Parser.oneOf
        [ Parser.succeed (\_ -> Parser.Done (List.reverse soFar))
            |= Parser.end
        , Parser.succeed (\_ -> Parser.Loop soFar)
            |= chompAtLeastOne (\c -> Set.member c whitespaceChars)
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
            |= Parser.getChompedString (chompAtLeastOne (\c -> not (Set.member c whitespaceChars)))
            |= Parser.getOffset
        ]


chompAtLeastOne : (Char -> Bool) -> Parser ()
chompAtLeastOne cond =
    Parser.chompIf cond |. Parser.chompWhile cond
