module Route exposing (Route(..), parse, toString)

import Content exposing (Content)
import Database.ID as ID exposing (ID)
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing ((</>), Parser, s, top)


type Route
    = NotFound
    | Root
    | NodeById ID
    | NodeByTitle Content
    | SyncSettings


toString : Route -> String
toString route =
    case route of
        Root ->
            Builder.absolute [] []

        NotFound ->
            Builder.absolute [ "404" ] []

        NodeById id_ ->
            Builder.absolute [ "node", ID.toString id_ ] []

        NodeByTitle title ->
            Builder.absolute [ "node", Url.percentEncode (Content.toString title) ] []

        SyncSettings ->
            Builder.absolute [ "settings", "sync" ] []


parse : Url -> Route
parse url =
    Maybe.withDefault NotFound (Parser.parse parser url)


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map Root top
        , Parser.map NodeById (s "node" </> id)
        , Parser.map NodeByTitle (s "node" </> content)
        , Parser.map SyncSettings (s "settings" </> s "sync")
        ]


id : Parser (ID -> a) a
id =
    Parser.custom "ID" (ID.fromString >> Result.toMaybe)


content : Parser (Content -> a) a
content =
    Parser.custom "CONTENT" (Url.percentDecode >> Maybe.andThen (Content.fromString >> Result.toMaybe))
