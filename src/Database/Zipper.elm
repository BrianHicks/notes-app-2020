module Database.Zipper exposing
    ( Zipper, startAt, Problem(..)
    , id, node
    , update
    )

{-|

@docs Zipper, startAt, Problem
@docs id, node
@docs update

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


startAt : Database.ID -> Database -> Result Problem Zipper
startAt id_ database =
    Database.get id_ database
        |> Result.fromMaybe (NotFound id_)
        |> Result.map Zipper


type Problem
    = NotFound Database.ID



-- GETTING STUFF


id : Zipper -> Database.ID
id (Zipper guts) =
    guts.id


node : Zipper -> Node
node (Zipper guts) =
    guts.node



-- MAKING MODIFICATIONS


update : (Node -> Node) -> Database -> Zipper -> Result Problem ( Zipper, Database )
update updater database zipper =
    let
        updated =
            Database.update (id zipper) updater database
    in
    case Database.get (id zipper) updated of
        Just guts ->
            Ok ( Zipper guts, updated )

        Nothing ->
            Err (NotFound (id zipper))
