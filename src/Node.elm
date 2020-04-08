module Node exposing (ID(..), Node, fromString, nextID)


type ID
    = ID Int


nextID : ID -> ID
nextID (ID i) =
    ID (i + 1)


type alias Node =
    { id : Maybe ID -- may not have been set yet
    , content : String
    }


fromString : String -> Node
fromString =
    Node Nothing
