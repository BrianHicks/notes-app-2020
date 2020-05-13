module Content exposing
    ( Content, empty, fromList, fromString, toList, toString, toHtml, isEmpty, splitAt, append
    , Snippet, text, noteLink, link
    , encode, decoder
    )

{-|

@docs Content, empty, fromList, fromString, toList, toString, toHtml, isEmpty, splitAt, append

@docs Snippet, text, noteLink, link

@docs encode, decoder

-}

import Accessibility.Styled as Html exposing (Attribute, Html)
import Css
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Parser.Advanced as Parser exposing ((|.), (|=), Token(..))
import Url
import Url.Builder
import Widgets.Colors as Colors


type Content
    = Content (List Snippet)


fromString : String -> Result (List String) Content
fromString =
    Parser.run (Parser.map Content parser)
        >> Result.mapError (List.map deadEndToString)


empty : Content
empty =
    Content []


fromList : List Snippet -> Content
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
        |> List.filter (not << nodeIsEmpty)
        |> Content


toString : Content -> String
toString (Content nodes) =
    nodes
        |> List.map nodeToString
        |> String.concat


toList : Content -> List Snippet
toList (Content guts) =
    guts


toHtml : msg -> List (Attribute Never) -> Content -> Html msg
toHtml onClick attrs ((Content guts) as outer) =
    Html.div (Attrs.css [ Css.position Css.relative ] :: attrs)
        [ Html.button
            [ Events.onClick onClick
            , Attrs.css
                [ Css.width (Css.pct 100)
                , Css.height (Css.pct 100)
                , Css.position Css.absolute
                , Css.margin Css.zero
                , Css.padding Css.zero
                , Css.border Css.zero
                , Css.opacity Css.zero
                , Css.top (Css.px 0)
                , Css.left (Css.px 0)

                -- interactive elements under this just need to set
                -- `position: relative` and a `z-index` higher than 0 to
                -- pop above and be clickable!
                , Css.zIndex (Css.int 0)
                ]
            ]
            [ Html.text (toString outer) ]
        , Html.div [] (List.map nodeToHtml guts)
        ]


isEmpty : Content -> Bool
isEmpty (Content guts) =
    List.isEmpty guts


splitAt : Int -> Content -> ( Content, Content )
splitAt splitPoint (Content nodes) =
    let
        ( left, right ) =
            splitListAt splitPoint nodes
    in
    ( Content left, Content right )


splitListAt : Int -> List Snippet -> ( List Snippet, List Snippet )
splitListAt splitPoint nodes =
    splitListAtHelp splitPoint [] nodes


splitListAtHelp : Int -> List Snippet -> List Snippet -> ( List Snippet, List Snippet )
splitListAtHelp splitPoint soFar nodes =
    if splitPoint <= 0 then
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
                if splitPoint > currentLength then
                    splitListAtHelp (splitPoint - currentLength) (node :: soFar) rest

                else if splitPoint == currentLength then
                    ( List.reverse (node :: soFar)
                    , rest
                    )

                else
                    let
                        ( left, right ) =
                            splitSnippetAt splitPoint node
                    in
                    ( List.reverse (left :: soFar)
                    , right :: rest
                    )


append : Content -> Content -> Content
append (Content nodesA) (Content nodesB) =
    fromList (nodesA ++ nodesB)



-- NODES


type Snippet
    = Text String
    | NoteLink (List Snippet)
    | Link
        { children : List Snippet
        , href : String
        }


text : String -> Snippet
text =
    Text


noteLink : List Snippet -> Snippet
noteLink children =
    NoteLink children


link : { children : List Snippet, href : String } -> Snippet
link =
    Link


nodeToString : Snippet -> String
nodeToString node =
    case node of
        Text text_ ->
            text_

        NoteLink children ->
            "[[" ++ String.concat (List.map nodeToString children) ++ "]]"

        Link guts ->
            "[" ++ String.concat (List.map nodeToString guts.children) ++ "](" ++ guts.href ++ ")"


nodeToHtml : Snippet -> Html msg
nodeToHtml node =
    let
        decoration color =
            Html.span [ Attrs.css [ Css.color (Colors.toCss color) ] ]
    in
    case node of
        Text text_ ->
            Html.text text_

        NoteLink children ->
            Html.a
                [ Attrs.css
                    [ Css.zIndex (Css.int 1)
                    , Css.position Css.relative
                    , Css.textDecoration Css.none
                    ]

                -- TODO: duplication of ideas! But I can't import Route right
                -- now without causing a cycle. Fix that and come back to this
                -- (and remove the Url imports above at that time.)
                , Attrs.href (Url.Builder.absolute [ "node", Url.percentEncode (toString (Content children)) ] [])
                ]
                [ decoration Colors.greyLight [ Html.text "[[" ]
                , decoration Colors.greenDark (List.map nodeToPlainHtml children)
                , decoration Colors.greyLight [ Html.text "]]" ]
                ]

        Link guts ->
            Html.a
                [ Attrs.href guts.href
                , Attrs.target "_blank"
                , Attrs.css
                    [ Css.color Css.inherit
                    , Css.textDecoration Css.none
                    , Css.zIndex (Css.int 1)
                    , Css.position Css.relative
                    ]
                ]
                [ decoration Colors.greyLight [ Html.text "[" ]
                , decoration Colors.greenDark (List.map nodeToPlainHtml guts.children)
                , decoration Colors.greyLight [ Html.text "](" ]
                , Html.span
                    [ Attrs.css
                        [ Css.color (Colors.toCss Colors.greenDark)
                        , Css.fontSize (Css.em 0.7)
                        ]
                    ]
                    [ Html.text "★" ]
                , decoration Colors.greyLight [ Html.text ")" ]
                ]


nodeToPlainHtml : Snippet -> Html msg
nodeToPlainHtml node =
    case node of
        Text text_ ->
            Html.text text_

        NoteLink children ->
            Html.span []
                [ Html.text "[["
                , Html.span [] (List.map nodeToPlainHtml children)
                , Html.text "]]"
                ]

        Link guts ->
            Html.span []
                [ Html.text "["
                , Html.span [] (List.map nodeToPlainHtml guts.children)
                , Html.text "](★)"
                ]


nodeLength : Snippet -> Int
nodeLength node =
    case node of
        Text text_ ->
            String.length text_

        NoteLink children ->
            List.sum (List.map nodeLength children)

        Link guts ->
            List.sum (List.map nodeLength guts.children)


splitSnippetAt : Int -> Snippet -> ( Snippet, Snippet )
splitSnippetAt splitPoint node =
    case node of
        Text text_ ->
            ( Text (String.left splitPoint text_)
            , Text (String.dropLeft splitPoint text_)
            )

        NoteLink contents ->
            let
                ( left, right ) =
                    splitListAt splitPoint contents
            in
            ( NoteLink left
            , NoteLink right
            )

        Link guts ->
            let
                ( left, right ) =
                    splitListAt splitPoint guts.children
            in
            ( Link { guts | children = left }
            , Link { guts | children = right }
            )


nodeIsEmpty : Snippet -> Bool
nodeIsEmpty node =
    case node of
        Text text_ ->
            String.isEmpty text_

        NoteLink [] ->
            True

        NoteLink children ->
            List.all nodeIsEmpty children

        Link guts ->
            List.isEmpty guts.children || List.all nodeIsEmpty guts.children



-- PARSER


type alias Parser a =
    Parser.Parser Context Problem a


type Context
    = ParsingText
    | ParsingNoteLink
    | ParsingLink


type Problem
    = -- text
      ExpectingText
      -- note link
    | ExpectingStartOfNoteLink
    | ExpectingEndOfNoteLink
      -- link
    | ExpectingStartOfLink
    | ExpectingEndOfLinkText
    | ExpectingStartOfLinkHref
    | ExpectingLinkHref
    | ExpectingEndOfLinkHref


parser : Parser (List Snippet)
parser =
    Parser.loop [] nodesParser


nodesParser : List Snippet -> Parser (Parser.Step (List Snippet) (List Snippet))
nodesParser soFar =
    Parser.oneOf
        [ Parser.map (\node -> Parser.Loop (node :: soFar)) noteLinkParser
        , Parser.map (\node -> Parser.Loop (node :: soFar)) linkParser
        , Parser.map (\node -> Parser.Loop (node :: soFar)) textParser
        , Parser.lazy (\_ -> Parser.succeed (Parser.Done (List.reverse soFar)))
        ]



-- text


textParser : Parser Snippet
textParser =
    Parser.succeed Text
        |= Parser.getChompedString (chompAtLeastOne (\c -> c /= '[' && c /= ']') ExpectingText)
        |> Parser.inContext ParsingText



-- note links


noteLinkStart : Token Problem
noteLinkStart =
    Token "[[" ExpectingStartOfNoteLink


noteLinkEnd : Token Problem
noteLinkEnd =
    Token "]]" ExpectingEndOfNoteLink


noteLinkParser : Parser Snippet
noteLinkParser =
    Parser.succeed NoteLink
        |. Parser.token noteLinkStart
        |= Parser.lazy (\_ -> parser)
        |. Parser.token noteLinkEnd
        |> Parser.inContext ParsingNoteLink



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


linkParser : Parser Snippet
linkParser =
    Parser.succeed (\children href -> Link { children = children, href = href })
        |. Parser.symbol linkStart
        |= Parser.lazy (\_ -> parser)
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
                    "While parsing " ++ String.join " in " contextItems ++ ", "

        expecting =
            case problem of
                -- text
                ExpectingText ->
                    "some text"

                -- note link
                ExpectingStartOfNoteLink ->
                    "the opening brackets of a [[note link]]"

                ExpectingEndOfNoteLink ->
                    "the closing brackets of a [[note link]]"

                -- link
                ExpectingStartOfLink ->
                    "the opening '[' of a [link](url)"

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


chompAtLeastOne : (Char -> Bool) -> Problem -> Parser ()
chompAtLeastOne cond problem =
    Parser.chompIf cond problem |. Parser.chompWhile cond



-- JSON


encode : Content -> Encode.Value
encode (Content nodes) =
    Encode.list encodeSnippet nodes


encodeSnippet : Snippet -> Encode.Value
encodeSnippet node =
    case node of
        Text text_ ->
            Encode.object
                [ ( "kind", Encode.string "text" )
                , ( "text", Encode.string text_ )
                ]

        NoteLink children ->
            Encode.object
                [ ( "kind", Encode.string "noteLink" )
                , ( "children", Encode.list encodeSnippet children )
                ]

        Link guts ->
            Encode.object
                [ ( "kind", Encode.string "link" )
                , ( "children", Encode.list encodeSnippet guts.children )
                , ( "href", Encode.string guts.href )
                ]


decoder : Decoder Content
decoder =
    Decode.map Content (Decode.list decodeSnippet)


decodeSnippet : Decoder Snippet
decodeSnippet =
    Decode.andThen
        (\kind ->
            case kind of
                "text" ->
                    Decode.map Text (Decode.field "text" Decode.string)

                "noteLink" ->
                    Decode.oneOf
                        [ Decode.map NoteLink (Decode.field "children" (Decode.list decodeSnippet))
                        , Decode.andThen
                            (\text_ ->
                                case fromString text_ of
                                    Ok (Content children) ->
                                        Decode.succeed (NoteLink children)

                                    Err err ->
                                        Decode.fail (String.join "\n" err)
                            )
                            (Decode.field "text" Decode.string)
                        ]

                "link" ->
                    Decode.map2 (\children href -> { children = children, href = href })
                        (Decode.oneOf
                            [ Decode.field "children" (Decode.list decodeSnippet)
                            , Decode.andThen
                                (\text_ ->
                                    case fromString text_ of
                                        Ok (Content children) ->
                                            Decode.succeed children

                                        Err err ->
                                            Decode.fail (String.join "\n" err)
                                )
                                (Decode.field "text" Decode.string)
                            ]
                        )
                        (Decode.field "href" Decode.string)
                        |> Decode.map Link

                _ ->
                    Decode.fail ("I don't know how to decode a \"" ++ kind ++ "\"inside a content.")
        )
        (Decode.field "kind" Decode.string)
