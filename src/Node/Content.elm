module Node.Content exposing
    ( Content, fromList, fromString, toList, toString, isEmpty
    , Node, text, noteLink, link
    )

{-|

@docs Content, fromList, fromString, toList, toString, isEmpty

@docs Node, text, noteLink, link

-}

import Parser exposing ((|.), (|=), Parser)


type Content
    = Content (List Node)


fromString : String -> Result (List Parser.DeadEnd) Content
fromString =
    Parser.run (Parser.map Content parser)


fromList : List Node -> Content
fromList nodes =
    nodes
        |> List.foldl
            (\next prev ->
                case ( next, prev ) of
                    ( Text nextText, (Text prevText) :: rest ) ->
                        Text (prevText ++ nextText) :: rest

                    _ ->
                        next :: prev
            )
            []
        |> Content


toString : Content -> String
toString (Content nodes) =
    nodes
        |> List.map nodeToString
        |> String.concat


toList : Content -> List Node
toList (Content guts) =
    guts


isEmpty : Content -> Bool
isEmpty (Content guts) =
    List.isEmpty guts



-- NODES


type Node
    = Text String
    | NoteLink String
    | Link { text : String, href : String }


text : String -> Node
text =
    Text


noteLink : String -> Node
noteLink =
    NoteLink


link : { text : String, href : String } -> Node
link =
    Link


nodeToString : Node -> String
nodeToString node =
    case node of
        Text text_ ->
            text_

        NoteLink name ->
            "[[" ++ name ++ "]]"

        Link guts ->
            "[" ++ guts.text ++ "](" ++ guts.href ++ ")"


parser : Parser (List Node)
parser =
    Parser.loop [] nodesParser


nodesParser : List Node -> Parser (Parser.Step (List Node) (List Node))
nodesParser soFar =
    Parser.oneOf
        [ Parser.succeed (\_ -> Parser.Done (List.reverse soFar))
            |= Parser.end
        , Parser.map (\node -> Parser.Loop (node :: soFar)) noteLinkParser
        , Parser.map (\node -> Parser.Loop (node :: soFar)) linkParser
        , Parser.succeed (\text_ -> Parser.Loop (Text text_ :: soFar))
            |= Parser.getChompedString (Parser.chompWhile (\c -> c /= '['))
        ]


noteLinkParser : Parser Node
noteLinkParser =
    Parser.andThen
        (\parsed ->
            case parsed of
                Ok contents ->
                    Parser.succeed (NoteLink contents)

                Err err ->
                    Parser.problem err
        )
        noteLinkContentsParser


noteLinkContentsParser : Parser (Result String String)
noteLinkContentsParser =
    Parser.succeed identity
        |. Parser.symbol "[["
        |= Parser.loop []
            (\soFar ->
                Parser.oneOf
                    [ Parser.succeed
                        (\sub ->
                            case sub of
                                Ok subContents ->
                                    Parser.Loop (("[[" ++ subContents ++ "]]") :: soFar)

                                Err err ->
                                    Parser.Done sub
                        )
                        |= Parser.lazy (\_ -> noteLinkContentsParser)
                    , Parser.succeed (\_ -> Parser.Done (Ok (String.concat (List.reverse soFar))))
                        |= Parser.symbol "]]"
                    , Parser.succeed (\_ -> Parser.Done (Err "newlines are not allowed in note links"))
                        |= Parser.token "\n"
                    , Parser.succeed (\text_ -> Parser.Loop (text_ :: soFar))
                        |= Parser.getChompedString (Parser.chompWhile (\c -> c /= '[' && c /= ']' && c /= '\n'))
                    ]
            )


linkParser : Parser Node
linkParser =
    Parser.succeed (\text_ href -> Link { text = text_, href = href })
        |. Parser.symbol "["
        |= Parser.getChompedString (Parser.chompWhile (\c -> c /= ']'))
        |. Parser.symbol "]("
        |= Parser.getChompedString (Parser.chompWhile (\c -> c /= ')'))
        |. Parser.symbol ")"
