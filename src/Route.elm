module Route exposing (..)

import Database
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing ((</>), int, map, oneOf, s, top)


type Route
    = NotFound
    | Root
    | Note Database.ID


toString : Route -> String
toString route =
    case route of
        Root ->
            Builder.absolute [] []

        NotFound ->
            Builder.absolute [ "404" ] []

        Note id ->
            Builder.absolute [ "notes", String.fromInt (Database.intFromId id) ] []


parse : Url -> Route
parse url =
    Maybe.withDefault NotFound (Parser.parse parser url)


parser =
    oneOf
        [ map Root top
        , map Note (s "notes" </> map Database.idFromInt int)
        ]
