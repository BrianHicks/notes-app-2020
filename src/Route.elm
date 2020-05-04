module Route exposing (..)

import Database
import Database.ID as ID
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing (..)


type Route
    = NotFound
    | Root
    | Node String


toString : Route -> String
toString route =
    case route of
        Root ->
            Builder.absolute [] []

        NotFound ->
            Builder.absolute [ "404" ] []

        Node id_ ->
            Builder.absolute [ "node", id_ ] []


parse : Url -> Route
parse url =
    Maybe.withDefault NotFound (Parser.parse parser url)


parser =
    oneOf
        [ map Root top
        , map Node (s "node" </> string)
        ]


id =
    custom "ID" (Result.toMaybe << ID.fromString)
