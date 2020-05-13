module Route exposing (..)

import Database
import Database.ID as ID exposing (ID)
import Node.Content as Content exposing (Content)
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing (..)


type Route
    = NotFound
    | Root
    | NodeById ID
    | NoteByName Content


toString : Route -> String
toString route =
    case route of
        Root ->
            Builder.absolute [] []

        NotFound ->
            Builder.absolute [ "404" ] []

        NodeById id_ ->
            Builder.absolute [ "node", ID.toString id_ ] []

        NoteByName name ->
            Builder.absolute [ "node", Url.percentEncode (Content.toString name) ] []


parse : Url -> Route
parse url =
    Maybe.withDefault NotFound (Parser.parse parser url)


parser =
    oneOf
        [ map Root top
        , map NodeById (s "node" </> id)
        , map NoteByName (s "node" </> content)
        ]


id : Parser (ID -> a) a
id =
    custom "ID" (ID.fromString >> Result.toMaybe)


content : Parser (Content -> a) a
content =
    custom "CONTENT" (Url.percentDecode >> Maybe.andThen (Content.fromString >> Result.toMaybe))
