module Database.Log exposing (..)

import Database.LWW as LWW exposing (LWW)
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
            { log = entry :: log.log
            , state = updateRow timestamp row operation log.state
            , generator = generator
            }
        )
        (Timestamp.sendAt now log.generator)


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
                                        let
                                            _ =
                                                Debug.log "n,e" ( new, existing )
                                        in
                                        Just (LWW.max new existing)

                                    Nothing ->
                                        Just new
                        }
        )
        state
