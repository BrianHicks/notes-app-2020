module Database.Log exposing (..)

import Database.Timestamp as Timestamp exposing (Timestamp)
import Dict exposing (Dict)
import Time exposing (Posix)


type alias Log =
    { log : List Entry
    , generator : Timestamp.Generator
    , state : State
    }


type alias State =
    Dict String Row


type alias Row =
    { content : Maybe String }


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


insert : { now : Posix, row : String, operation : Operation } -> Log -> Result Timestamp.Problem Log
insert { now, row, operation } log =
    Result.map
        (\( timestamp, generator ) ->
            let
                entry =
                    { timestamp = timestamp
                    , row = row
                    , operation = operation
                    }
            in
            { log = entry :: log.log
            , state = updateRow row operation log.state
            , generator = generator
            }
        )
        (Timestamp.sendAt now log.generator)


updateRow : String -> Operation -> State -> State
updateRow rowID operation state =
    Dict.update rowID
        (\maybeRow ->
            let
                row =
                    Maybe.withDefault emptyRow maybeRow
            in
            Just <|
                case operation of
                    SetContent content ->
                        { row | content = Just content }
        )
        state
