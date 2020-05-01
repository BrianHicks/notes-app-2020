module Database.Log exposing (..)

import Database.Timestamp as Timestamp
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


insert : { now : Posix, key : String, column : String, operation : Operation } -> Log -> Log
insert { now, key, column, operation } log =
    let
        ( timestamp, generator ) =
            Timestamp.sendAt now log.generator

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


{-| AAAAAAAAAAAAAAAAAAAHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH
-}
updateState : Entry -> State -> State
updateState { timestamp, key, column, operation } state =
    Dict.update
        (\maybeRow ->
            case maybeRow of
                Just row ->
                    Dict.update
                        (\maybeCell ->
                            case maybeCell of
                                Just ( existingTimestamp, existingValue ) ->
                                    case operation of
                                        Set value ->
                                            if compare existingTimestamp timestamp == LT then
                                                Just ( timestamp, value )

                                            else
                                                Just ( existingTimestamp, existingValue )

                                Nothing ->
                                    case operation of
                                        Set value ->
                                            Just ( timestamp, value )
                        )
                        column
                        row

                Nothing ->
                    case operation of
                        Set value ->
                            Just (Dict.singleton column ( timestamp, value ))
        )
        key
        state
