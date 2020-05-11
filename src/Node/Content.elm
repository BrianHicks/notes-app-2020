module Node.Content exposing
    ( Content, empty, fromList, fromString, toList, toString, toHtml, isEmpty, splitAt, append
    , Node, text, noteLink, link
    , encode, decoder
    )

{-|

@docs Content, empty, fromList, fromString, toList, toString, toHtml, isEmpty, splitAt, append

@docs Node, text, noteLink, link

@docs encode, decoder

-}

import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
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
        |> List.reverse
        |> Content


toString : Content -> String
toString (Content nodes) =
    nodes
        |> List.map nodeToString
        |> String.concat


toList : Content -> List Node
toList (Content guts) =
    guts


toHtml : Content -> List (Html msg)
toHtml (Content guts) =
    List.map nodeToHtml guts


isEmpty : Content -> Bool
isEmpty (Content guts) =
    List.isEmpty guts


splitAt : Int -> Content -> ( Content, Content )
splitAt howMuch (Content nodes) =
    let
        ( left, right ) =
            splitAtHelp howMuch [] nodes
    in
    ( Content left, Content right )


splitAtHelp : Int -> List Node -> List Node -> ( List Node, List Node )
splitAtHelp howMuch soFar nodes =
    if howMuch <= 0 then
        ( List.reverse soFar
        , nodes
        )

    else
        case nodes of
            [] ->
                ( List.reverse soFar
                , nodes
                )

            node :: rest ->
                let
                    currentLength =
                        nodeLength node
                in
                if howMuch > currentLength then
                    splitAtHelp (howMuch - currentLength) (node :: soFar) rest

                else if howMuch == currentLength then
                    ( List.reverse (node :: soFar)
                    , rest
                    )

                else
                    let
                        ( left, right ) =
                            splitNodeAt howMuch node
                    in
                    ( List.reverse (left :: soFar)
                    , right :: rest
                    )


append : Content -> Content -> Content
append (Content nodesA) (Content nodesB) =
    fromList (nodesA ++ nodesB)



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
            Html.a [ Attrs.href guts.href ]
                [ Html.span [] [ Html.text "[[" ]
                , Html.text guts.text
                , Html.span [] [ Html.text "]]" ]
                ]


nodeLength : Node -> Int
nodeLength node =
    case node of
        Text text_ ->
            String.length text_

        NoteLink text_ ->
            String.length text_

        Link link_ ->
            String.length link_.text


splitNodeAt : Int -> Node -> ( Node, Node )
splitNodeAt howMuch node =
    case node of
        Text text_ ->
            ( Text (String.left howMuch text_)
            , Text (String.right howMuch text_)
            )

        NoteLink text_ ->
            ( NoteLink (String.left howMuch text_)
            , NoteLink (String.right howMuch text_)
            )

        Link link_ ->
            ( Link { link_ | text = String.left howMuch link_.text }
            , Link { link_ | text = String.right howMuch link_.text }
            )



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



-- JSON


encode : Content -> Encode.Value
encode (Content nodes) =
    Encode.list encodeNode nodes


encodeNode : Node -> Encode.Value
encodeNode node =
    case node of
        Text text_ ->
            Encode.object
                [ ( "kind", Encode.string "text" )
                , ( "text", Encode.string text_ )
                ]

        NoteLink text_ ->
            Encode.object
                [ ( "kind", Encode.string "noteLink" )
                , ( "text", Encode.string text_ )
                ]

        Link guts ->
            Encode.object
                [ ( "kind", Encode.string "link" )
                , ( "text", Encode.string guts.text )
                , ( "href", Encode.string guts.href )
                ]


decoder : Decoder Content
decoder =
    Decode.map Content (Decode.list decodeNode)


decodeNode : Decoder Node
decodeNode =
    Decode.andThen
        (\kind ->
            case kind of
                "text" ->
                    Decode.map Text (Decode.field "text" Decode.string)

                "noteLink" ->
                    Decode.map NoteLink (Decode.field "text" Decode.string)

                "link" ->
                    Decode.map2 (\text_ href -> { text = text_, href = href })
                        (Decode.field "text" Decode.string)
                        (Decode.field "href" Decode.string)
                        |> Decode.map Link

                _ ->
                    Decode.fail ("I don't know how to decode a \"" ++ kind ++ "\"inside a content.")
        )
        (Decode.field "kind" Decode.string)
