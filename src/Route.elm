module Route exposing (..)

import Database
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing ((</>), int, map, oneOf, s, top)


type Route
    = NotFound
    | Root
    | Node Database.ID


toString : Route -> String
toString route =
    case route of
        Root ->
            Builder.absolute [] []

        NotFound ->
            Builder.absolute [ "404" ] []

        Node id ->
            Builder.absolute [ "node", Database.idToString id ] []


parse : Url -> Route
parse url =
    Maybe.withDefault NotFound (Parser.parse parser url)


parser =
    oneOf
        [ map Root top
        , map Node (s "node" </> map Database.idFromInt int)
        ]
