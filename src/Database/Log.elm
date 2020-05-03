module Database.Log exposing
    ( Log, Row, State, init, get, filter, insert, edit, receive, load
    , Entry, Operation(..), decoder, encode
    )

{-|

@docs Log, Row, State, init, get, filter, insert, edit, receive, load

@docs Entry, Operation, decoder, encode

-}

import Database.LWW as LWW exposing (LWW)
import Database.Timestamp as Timestamp exposing (Timestamp)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Random
import Time exposing (Posix)
import UUID


type alias Log =
    { log : List Entry
    , generator : Timestamp.Generator
    , state : State
    , seed : Random.Seed
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


init : Random.Seed -> Timestamp.NodeID -> Log
init seed nodeID =
    { log = []
    , state = Dict.empty
    , generator = Timestamp.generator nodeID
    , seed = seed
    }


get : String -> Log -> Maybe Row
get id log =
    Dict.get id log.state


filter : (String -> Row -> Bool) -> Log -> Dict String Row
filter pred log =
    Dict.filter pred log.state


insert : Posix -> String -> Log -> Result Timestamp.Problem ( String, Log, Entry )
insert now content log =
    Result.map
        (\( timestamp, generator ) ->
            let
                ( id, nextSeed ) =
                    Random.step (Random.map UUID.toString UUID.generator) log.seed

                entry =
                    { timestamp = timestamp
                    , row = id
                    , operation = SetContent content
                    }
            in
            ( id
            , { log = insertDescending (\a b -> Timestamp.compare a.timestamp b.timestamp) entry log.log
              , state = updateRow entry log.state
              , generator = generator
              , seed = nextSeed
              }
            , entry
            )
        )
        (Timestamp.sendAt now log.generator)


edit : Posix -> String -> String -> Log -> Result Timestamp.Problem ( Log, Entry )
edit now id content log =
    Result.map
        (\( timestamp, generator ) ->
            let
                entry =
                    { timestamp = timestamp
                    , row = id
                    , operation = SetContent content
                    }
            in
            ( { log = insertDescending (\a b -> Timestamp.compare a.timestamp b.timestamp) entry log.log
              , state = updateRow entry log.state
              , generator = generator
              , seed = log.seed
              }
            , entry
            )
        )
        (Timestamp.sendAt now log.generator)


receive : Posix -> Entry -> Log -> Result Timestamp.Problem Log
receive now entry log =
    Result.map
        (\generator ->
            { log = insertDescending (\a b -> Timestamp.compare a.timestamp b.timestamp) entry log.log
            , state = updateRow entry log.state
            , generator = generator
            , seed = log.seed
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
    , state = updateRow entry log.state
    , generator =
        case newLog of
            newest :: _ ->
                Timestamp.generatorAt newest.timestamp (Timestamp.nodeID log.generator)

            [] ->
                Timestamp.generatorAt entry.timestamp (Timestamp.nodeID log.generator)
    , seed = log.seed
    }


updateRow : Entry -> State -> State
updateRow entry state =
    Dict.update entry.row
        (\maybeRow ->
            let
                row =
                    Maybe.withDefault emptyRow maybeRow
            in
            Just <|
                case entry.operation of
                    SetContent content ->
                        let
                            new =
                                LWW.init entry.timestamp content
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
        [ -- couchdb/pouchdb need an ID for sorting. This is the right one to
          -- do it with!
          ( "_id", Timestamp.encode entry.timestamp )
        , ( "dataset", Encode.string "nodes" )
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
                        (Decode.field "_id" Timestamp.decoder)
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
