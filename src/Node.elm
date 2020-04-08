module Node exposing (Node, fromString)


type alias Node =
    { id : Maybe Int -- may not have been set yet
    , content : String
    }


fromString : String -> Node
fromString =
    Node Nothing
