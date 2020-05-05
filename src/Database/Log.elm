module Database.Log exposing
    ( Log, Row, init, get, toDict, newNode, edit, receive, load
    , Entry, Operation(..), decoder, encode
    )

{-|

@docs Log, Row, init, get, toDict, newNode, edit, receive, load

@docs Entry, Operation, decoder, encode

-}

import Database.ID as ID exposing (ID)
import Database.LWW as LWW exposing (LWW)
import Database.Timestamp as Timestamp exposing (Timestamp)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Random
import Sort.Dict as Dict exposing (Dict)
import Time exposing (Posix)
import UUID


type Log
    = Log
        { generator : Timestamp.Generator
        , state : Dict ID Row
        , seed : Random.Seed
        }


type alias Row =
    { content : Maybe (LWW String) }


emptyRow : Row
emptyRow =
    { content = Nothing }


type alias Entry =
    { timestamp : Timestamp
    , id : ID
    , operation : Operation
    }


type Operation
    = SetContent String


init : Random.Seed -> Timestamp.NodeID -> Log
init seed nodeID =
    Log
        { state = Dict.empty ID.sorter
        , generator = Timestamp.generator nodeID
        , seed = seed
        }


get : ID -> Log -> Maybe Row
get id (Log log) =
    Dict.get id log.state


toDict : Log -> Dict ID Row
toDict (Log log) =
    log.state


newNode : Posix -> String -> Log -> Result Timestamp.Problem ( ID, Log, Entry )
newNode now content (Log log) =
    let
        ( id, nextSeed ) =
            Random.step ID.generator log.seed
    in
    send now id (SetContent content) (Log { log | seed = nextSeed })
        |> Result.map (\( newLog, entry ) -> ( id, newLog, entry ))


edit : Posix -> ID -> String -> Log -> Result Timestamp.Problem ( Log, Maybe Entry )
edit now id content log =
    case log |> get id |> Maybe.andThen .content |> Maybe.map LWW.value |> Maybe.map ((==) content) of
        Just False ->
            log
                |> send now id (SetContent content)
                |> Result.map (Tuple.mapSecond Just)

        _ ->
            Ok ( log, Nothing )


send : Posix -> ID -> Operation -> Log -> Result Timestamp.Problem ( Log, Entry )
send now id operation (Log log) =
    Result.map
        (\( timestamp, generator ) ->
            let
                entry =
                    { timestamp = timestamp
                    , id = id
                    , operation = operation
                    }
            in
            ( Log
                { log
                    | state = updateRow entry log.state
                    , generator = generator
                }
            , entry
            )
        )
        (Timestamp.sendAt now log.generator)


receive : Posix -> Entry -> Log -> Result Timestamp.Problem Log
receive now entry (Log log) =
    Result.map
        (\generator ->
            Log
                { state = updateRow entry log.state
                , generator = generator
                , seed = log.seed
                }
        )
        (Timestamp.receiveAt now log.generator entry.timestamp)


load : Entry -> Log -> Log
load entry (Log log) =
    Log
        { state = updateRow entry log.state
        , generator =
            -- TODO: compare if it's newer or older here
            Timestamp.generatorAt entry.timestamp (Timestamp.nodeID log.generator)
        , seed = log.seed
        }


updateRow : Entry -> Dict ID Row -> Dict ID Row
updateRow entry state =
    Dict.update entry.id
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



-- JSON


encode : Entry -> Value
encode entry =
    Encode.object
        [ -- couchdb/pouchdb need an ID for sorting. This is the right one to
          -- do it with!
          ( "_id", Timestamp.encode entry.timestamp )
        , ( "dataset", Encode.string "nodes" )
        , ( "id", ID.encode entry.id )
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
                        (Decode.field "id" ID.decoder)
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
