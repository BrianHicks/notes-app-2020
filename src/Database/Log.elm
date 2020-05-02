module Database.Log exposing (..)

import Database.Timestamp as Timestamp exposing (Timestamp)
import Dict exposing (Dict)
import Json.Decode exposing (Value)
import Time exposing (Posix)


type alias Log =
    { log : List Entry
    , state : State
    , generator : Timestamp.Generator
    }


type alias Entry =
    { timestamp : Timestamp
    , key : String
    , column : String
    , operation : Operation
    }


type alias State =
    Dict String (Dict String ( Timestamp, Value ))


type Operation
    = Set Value


init : Timestamp.NodeID -> Log
init nodeID =
    { log = []
    , state = Dict.empty
    , generator = Timestamp.generator nodeID
    }


insert : { now : Posix, key : String, column : String, operation : Operation } -> Log -> Result Timestamp.Problem Log
insert { now, key, column, operation } log =
    Result.map
        (\( timestamp, generator ) ->
            let
                entry =
                    { timestamp = timestamp
                    , key = key
                    , column = column
                    , operation = operation
                    }
            in
            { log = entry :: log.log
            , state = updateState entry log.state
            , generator = generator
            }
        )
        (Timestamp.sendAt now log.generator)


{-| AAAAAAAAAAAAAAAAAAAHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH
-}
updateState : Entry -> State -> State
updateState { timestamp, key, column, operation } state =
    Dict.update key
        (\maybeRow ->
            case maybeRow of
                Just row ->
                    row
                        |> Dict.update column
                            (\maybeCell ->
                                case maybeCell of
                                    Just ( existingTimestamp, existingValue ) ->
                                        case operation of
                                            Set value ->
                                                if Timestamp.compare existingTimestamp timestamp == LT then
                                                    Just ( timestamp, value )

                                                else
                                                    Just ( existingTimestamp, existingValue )

                                    Nothing ->
                                        case operation of
                                            Set value ->
                                                Just ( timestamp, value )
                            )
                        |> Just

                Nothing ->
                    case operation of
                        Set value ->
                            Just (Dict.singleton column ( timestamp, value ))
        )
        state
