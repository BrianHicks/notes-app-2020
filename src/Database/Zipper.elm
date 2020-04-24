module Database.Zipper exposing (Zipper, startAt)

import Database exposing (Database)
import Node exposing (Node)


type Zipper
    = Zipper
        { id : Database.ID
        , node : Node
        , parent : Maybe Database.ID
        , children : List Database.ID
        }


startAt : Database.ID -> Database -> Maybe Zipper
startAt id database =
    Maybe.map Zipper (Database.get id database)
