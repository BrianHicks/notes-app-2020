module Database.Zipper exposing
    ( Zipper, startAt
    , id, node
    )

{-|

@docs Zipper, startAt
@docs id, node

-}

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
startAt id_ database =
    Maybe.map Zipper (Database.get id_ database)



-- GETTING STUFF


id : Zipper -> Database.ID
id (Zipper guts) =
    guts.id


node : Zipper -> Node
node (Zipper guts) =
    guts.node
