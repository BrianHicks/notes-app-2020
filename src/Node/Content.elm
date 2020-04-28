module Node.Content exposing
    ( Content, empty, fromList, fromString, toList, toString, toHtml, isEmpty
    , Node, text, noteLink, link
    )

{-|

@docs Content, empty, fromList, fromString, toList, toString, toHtml, isEmpty

@docs Node, text, noteLink, link

-}

import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Parser.Advanced as Parser exposing ((|.), (|=), Token(..))


type Content
    = Content (List Node)


fromString : String -> Result (List String) Content
fromString =
    Parser.run (Parser.map Content parser)
        >> Result.mapError (List.map deadEndToString)


empty : Content
empty =
    Content []


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


toHtml : Content -> Html msg
toHtml (Content guts) =
    Html.div [] (List.map nodeToHtml guts)


isEmpty : Content -> Bool
isEmpty (Content guts) =
    List.isEmpty guts



-- NODES


type Node
    = Text String
    | NoteLink String -- TODO: List Node
    | Link
        { text : String -- TODO: List Node
        , href : String
        }


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


nodeToHtml : Node -> Html msg
nodeToHtml node =
    case node of
        Text text_ ->
            Html.text text_

        NoteLink name ->
            Html.a [] [ Html.text name ]

        Link guts ->
            Html.a [ Attrs.href guts.href ] [ Html.text guts.text ]



-- PARSER


type alias Parser a =
    Parser.Parser Context Problem a


type Context
    = ParsingText
    | ParsingNoteLink
    | ParsingLink


type Problem
    = ExpectingEndOfContent
    | ExpectingNewline
    | ExpectingNoNewline
    | -- text
      ExpectingText
      -- note link
    | ExpectingStartOfNoteLink
    | ExpectingNoteLinkText
    | ExpectingEndOfNoteLink
      -- link
    | ExpectingStartOfLink
    | ExpectingLinkText
    | ExpectingEndOfLinkText
    | ExpectingStartOfLinkHref
    | ExpectingLinkHref
    | ExpectingEndOfLinkHref


parser : Parser (List Node)
parser =
    Parser.loop [] nodesParser


nodesParser : List Node -> Parser (Parser.Step (List Node) (List Node))
nodesParser soFar =
    Parser.oneOf
        [ Parser.succeed (\_ -> Parser.Done (List.reverse soFar))
            |= Parser.end ExpectingEndOfContent
        , Parser.map (\node -> Parser.Loop (node :: soFar)) noteLinkParser
        , Parser.map (\node -> Parser.Loop (node :: soFar)) linkParser
        , Parser.map (\node -> Parser.Loop (node :: soFar)) textParser
        ]



-- text


textParser : Parser Node
textParser =
    Parser.succeed Text
        |= Parser.getChompedString (chompAtLeastOne (\c -> c /= '[') ExpectingText)
        |> Parser.inContext ParsingText



-- note links


noteLinkStart : Token Problem
noteLinkStart =
    Token "[[" ExpectingStartOfNoteLink


noteLinkEnd : Token Problem
noteLinkEnd =
    Token "]]" ExpectingEndOfNoteLink


noteLinkParser : Parser Node
noteLinkParser =
    noteLinkContentsParser
        |> Parser.andThen
            (\parsed ->
                case parsed of
                    Ok contents ->
                        Parser.succeed (NoteLink contents)

                    Err err ->
                        Parser.problem err
            )
        |> Parser.inContext ParsingNoteLink


noteLinkContentsParser : Parser (Result Problem String)
noteLinkContentsParser =
    Parser.succeed identity
        |. Parser.symbol noteLinkStart
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
                        |= Parser.symbol noteLinkEnd
                    , Parser.succeed (\_ -> Parser.Done (Err ExpectingNoNewline))
                        |= Parser.symbol newline
                    , Parser.succeed (\text_ -> Parser.Loop (text_ :: soFar))
                        |= Parser.getChompedString (chompAtLeastOne (\c -> c /= '[' && c /= ']' && c /= '\n') ExpectingNoteLinkText)
                    ]
            )



-- links


linkStart : Token Problem
linkStart =
    Token "[" ExpectingStartOfLink


linkTextClose : Token Problem
linkTextClose =
    Token "]" ExpectingEndOfLinkText


linkHrefOpen : Token Problem
linkHrefOpen =
    Token "(" ExpectingStartOfLinkHref


linkHrefClose : Token Problem
linkHrefClose =
    Token ")" ExpectingEndOfLinkHref


linkParser : Parser Node
linkParser =
    Parser.succeed (\text_ href -> Link { text = text_, href = href })
        |. Parser.symbol linkStart
        |= Parser.getChompedString (chompAtLeastOne (\c -> c /= ']') ExpectingLinkText)
        |. Parser.symbol linkTextClose
        |. Parser.symbol linkHrefOpen
        |= Parser.getChompedString (chompAtLeastOne (\c -> c /= ')') ExpectingLinkHref)
        |. Parser.symbol linkHrefClose
        |> Parser.inContext ParsingLink



-- dead ends


type alias DeadEnd =
    Parser.DeadEnd Context Problem


deadEndToString : DeadEnd -> String
deadEndToString { row, col, problem, contextStack } =
    let
        contextToString context_ =
            case context_ of
                ParsingText ->
                    "text"

                ParsingNoteLink ->
                    "a [[note link]]"

                ParsingLink ->
                    "a link"

        context =
            case List.map (.context >> contextToString) contextStack of
                [] ->
                    ""

                contextItems ->
                    "While parsing " ++ String.join " in a " contextItems ++ ", "

        expecting =
            case problem of
                ExpectingEndOfContent ->
                    "the end of the content string"

                ExpectingNewline ->
                    "a new line"

                ExpectingNoNewline ->
                    "no new line"

                -- text
                ExpectingText ->
                    "some text"

                -- note link
                ExpectingStartOfNoteLink ->
                    "the opening brackets of a [[note link]]"

                ExpectingNoteLinkText ->
                    "the text inside a [[note link]]"

                ExpectingEndOfNoteLink ->
                    "the closing brackets of a [[note link]]"

                -- link
                ExpectingStartOfLink ->
                    "the opening '[' of a [link](url)"

                ExpectingLinkText ->
                    "the 'link' part of a [link](url)"

                ExpectingEndOfLinkText ->
                    "the closing ']' of a [link](url)"

                ExpectingStartOfLinkHref ->
                    "the opening '(' of a [link](url)"

                ExpectingLinkHref ->
                    "the 'href' part of a [link](url)"

                ExpectingEndOfLinkHref ->
                    "the closing ')' of a [link](url)"
    in
    context ++ "I was expecting " ++ expecting



-- parser utilities


newline : Token Problem
newline =
    Token "\n" ExpectingNewline


chompAtLeastOne : (Char -> Bool) -> Problem -> Parser ()
chompAtLeastOne cond problem =
    Parser.chompIf cond problem |. Parser.chompWhile cond
