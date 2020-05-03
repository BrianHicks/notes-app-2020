module Database.Log exposing (..)

import Database.LWW as LWW exposing (LWW)
import Database.Timestamp as Timestamp exposing (Timestamp)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Time exposing (Posix)


type alias Log =
    { log : List Entry
    , generator : Timestamp.Generator
    , state : State
    }


type alias State =
    Dict String Row


type alias Row =
    { content : Maybe (LWW String) }


emptyRow : Row
emptyRow =
    { content = Nothing }


type alias Entry =
    { timestamp : Timestamp
    , row : String
    , operation : Operation
    }


type Operation
    = SetContent String


init : Timestamp.NodeID -> Log
init nodeID =
    { log = []
    , state = Dict.empty
    , generator = Timestamp.generator nodeID
    }


insert : Posix -> String -> Operation -> Log -> Result Timestamp.Problem Log
insert now row operation log =
    Result.map
        (\( timestamp, generator ) ->
            let
                entry =
                    { timestamp = timestamp
                    , row = row
                    , operation = operation
                    }
            in
            { log = insertDescending (\a b -> Timestamp.compare a.timestamp b.timestamp) entry log.log
            , state = updateRow timestamp row operation log.state
            , generator = generator
            }
        )
        (Timestamp.sendAt now log.generator)


receive : Posix -> Entry -> Log -> Result Timestamp.Problem Log
receive now entry log =
    Result.map
        (\generator ->
            { log = insertDescending (\a b -> Timestamp.compare a.timestamp b.timestamp) entry log.log
            , state = updateRow entry.timestamp entry.row entry.operation log.state
            , generator = generator
            }
        )
        (Timestamp.receiveAt now log.generator entry.timestamp)


load : Entry -> Log -> Log
load entry log =
    let
        newLog =
            insertDescending (\a b -> Timestamp.compare a.timestamp b.timestamp) entry log.log
    in
    { log = newLog
    , state = updateRow entry.timestamp entry.row entry.operation log.state
    , generator =
        case newLog of
            newest :: _ ->
                Timestamp.generatorAt newest.timestamp (Timestamp.nodeID log.generator)

            [] ->
                Timestamp.generatorAt entry.timestamp (Timestamp.nodeID log.generator)
    }


updateRow : Timestamp -> String -> Operation -> State -> State
updateRow timestamp rowID operation state =
    Dict.update rowID
        (\maybeRow ->
            let
                row =
                    Maybe.withDefault emptyRow maybeRow
            in
            Just <|
                case operation of
                    SetContent content ->
                        let
                            new =
                                LWW.init timestamp content
                        in
                        { row
                            | content =
                                case row.content of
                                    Just existing ->
                                        Just (LWW.max new existing)

                                    Nothing ->
                                        Just new
                        }
        )
        state


insertDescending : (a -> a -> Order) -> a -> List a -> List a
insertDescending cmp item items =
    insertDescendingHelp cmp item items []


insertDescendingHelp : (a -> a -> Order) -> a -> List a -> List a -> List a
insertDescendingHelp cmp item items itemsRev =
    case items of
        [] ->
            List.reverse (item :: itemsRev)

        head :: tail ->
            case cmp item head of
                GT ->
                    List.reverse itemsRev ++ (item :: head :: tail)

                _ ->
                    insertDescendingHelp cmp item tail (head :: itemsRev)



-- JSON


encode : Entry -> Value
encode entry =
    Encode.object
        [ ( "dataset", Encode.string "nodes" )
        , ( "timestamp", Timestamp.encode entry.timestamp )
        , ( "row", Encode.string entry.row )
        , ( "operation"
          , case entry.operation of
                SetContent content ->
                    Encode.object
                        [ ( "operation", Encode.string "setContent" )
                        , ( "content", Encode.string content )
                        ]
          )
        ]


decoder : Decoder Entry
decoder =
    Decode.andThen
        (\dataset ->
            case dataset of
                "nodes" ->
                    Decode.map3 Entry
                        (Decode.field "timestamp" Timestamp.decoder)
                        (Decode.field "row" Decode.string)
                        (Decode.field "operation" operationDecoder)

                _ ->
                    Decode.fail ("I don't know how to decode entries in the " ++ dataset ++ " dataset.")
        )
        (Decode.field "dataset" Decode.string)


operationDecoder : Decoder Operation
operationDecoder =
    Decode.andThen
        (\name ->
            case name of
                "setContent" ->
                    Decode.map SetContent (Decode.field "content" Decode.string)

                _ ->
                    Decode.fail ("I don't know how to handle a " ++ name ++ " operation.")
        )
        (Decode.field "operation" Decode.string)
