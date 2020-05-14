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
import Html.Styled as InaccessibleHtml
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events
import Html.Styled.Events.Extra exposing (onClickPreventDefaultForLinkWithHref)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Parser.Advanced as Parser exposing ((|.), (|=), Token(..))
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
fromList children =
    children
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
        |> List.filter (not << snippetIsEmpty)
        |> Content


toString : Content -> String
toString (Content children) =
    children
        |> List.map snippetToString
        |> String.concat


toList : Content -> List Snippet
toList (Content guts) =
    guts


toHtml :
    { activate : Maybe msg
    , navigate : Content -> msg
    , navigateUrl : Content -> String
    }
    -> List (Attribute Never)
    -> Content
    -> Html msg
toHtml { activate, navigate, navigateUrl } attrs ((Content guts) as outer) =
    Html.div (Attrs.css [ Css.position Css.relative ] :: attrs)
        [ case activate of
            Just msg ->
                Html.button
                    [ Events.onClick msg
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

            Nothing ->
                Html.text ""
        , Html.div []
            (List.map
                (snippetToHtml
                    { navigate = navigate
                    , navigateUrl = navigateUrl
                    }
                )
                guts
            )
        ]


isEmpty : Content -> Bool
isEmpty (Content guts) =
    List.isEmpty guts


splitAt : Int -> Content -> ( Content, Content )
splitAt splitPoint (Content snippets) =
    let
        ( left, right ) =
            splitListAt splitPoint snippets
    in
    ( Content left, Content right )


splitListAt : Int -> List Snippet -> ( List Snippet, List Snippet )
splitListAt splitPoint snippets =
    splitListAtHelp splitPoint [] snippets


splitListAtHelp : Int -> List Snippet -> List Snippet -> ( List Snippet, List Snippet )
splitListAtHelp splitPoint soFar snippets =
    if splitPoint <= 0 then
        ( List.reverse soFar
        , snippets
        )

    else
        case snippets of
            [] ->
                ( List.reverse soFar
                , snippets
                )

            snippet :: rest ->
                let
                    currentLength =
                        snippetLength snippet
                in
                if splitPoint > currentLength then
                    splitListAtHelp (splitPoint - currentLength) (snippet :: soFar) rest

                else if splitPoint == currentLength then
                    ( List.reverse (snippet :: soFar)
                    , rest
                    )

                else
                    let
                        ( left, right ) =
                            splitSnippetAt splitPoint snippet
                    in
                    ( List.reverse (left :: soFar)
                    , right :: rest
                    )


append : Content -> Content -> Content
append (Content childrenA) (Content childrenB) =
    fromList (childrenA ++ childrenB)



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


snippetToString : Snippet -> String
snippetToString snippet =
    case snippet of
        Text text_ ->
            text_

        NoteLink children ->
            "[[" ++ String.concat (List.map snippetToString children) ++ "]]"

        Link guts ->
            "[" ++ String.concat (List.map snippetToString guts.children) ++ "](" ++ guts.href ++ ")"


snippetToHtml :
    { navigate : Content -> msg
    , navigateUrl : Content -> String
    }
    -> Snippet
    -> Html msg
snippetToHtml { navigate, navigateUrl } snippet =
    let
        decoration color =
            Html.span [ Attrs.css [ Css.color (Colors.toCss color) ] ]
    in
    case snippet of
        Text text_ ->
            Html.text text_

        NoteLink children ->
            InaccessibleHtml.a
                [ Attrs.css
                    [ Css.zIndex (Css.int 1)
                    , Css.position Css.relative
                    , Css.textDecoration Css.none
                    , Css.cursor Css.pointer
                    ]
                , Attrs.href (navigateUrl (Content children))
                , onClickPreventDefaultForLinkWithHref (navigate (Content children))
                ]
                [ decoration Colors.greyLight [ Html.text "[[" ]
                , decoration Colors.greenDark (List.map snippetToPlainHtml children)
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
                , decoration Colors.greenDark (List.map snippetToPlainHtml guts.children)
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


snippetToPlainHtml : Snippet -> Html msg
snippetToPlainHtml snippet =
    case snippet of
        Text text_ ->
            Html.text text_

        NoteLink children ->
            Html.span []
                [ Html.text "[["
                , Html.span [] (List.map snippetToPlainHtml children)
                , Html.text "]]"
                ]

        Link guts ->
            Html.span []
                [ Html.text "["
                , Html.span [] (List.map snippetToPlainHtml guts.children)
                , Html.text "](★)"
                ]


snippetLength : Snippet -> Int
snippetLength snippet =
    case snippet of
        Text text_ ->
            String.length text_

        NoteLink children ->
            List.sum (List.map snippetLength children)

        Link guts ->
            List.sum (List.map snippetLength guts.children)


splitSnippetAt : Int -> Snippet -> ( Snippet, Snippet )
splitSnippetAt splitPoint snippet =
    case snippet of
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


snippetIsEmpty : Snippet -> Bool
snippetIsEmpty snippet =
    case snippet of
        Text text_ ->
            String.isEmpty text_

        NoteLink [] ->
            True

        NoteLink children ->
            List.all snippetIsEmpty children

        Link guts ->
            List.isEmpty guts.children || List.all snippetIsEmpty guts.children



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
    Parser.loop [] snippetsParser


snippetsParser : List Snippet -> Parser (Parser.Step (List Snippet) (List Snippet))
snippetsParser soFar =
    Parser.oneOf
        [ Parser.map (\snippet -> Parser.Loop (snippet :: soFar)) noteLinkParser
        , Parser.map (\snippet -> Parser.Loop (snippet :: soFar)) linkParser
        , Parser.map (\snippet -> Parser.Loop (snippet :: soFar)) textParser
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
encode (Content children) =
    Encode.list encodeSnippet children


encodeSnippet : Snippet -> Encode.Value
encodeSnippet snippet =
    case snippet of
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
