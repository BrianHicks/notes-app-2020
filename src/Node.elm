module Node exposing (ID(..), Metadata(..), Node, fromString, nextID, withMetadata)


type ID
    = ID Int


nextID : ID -> ID
nextID (ID i) =
    ID (i + 1)


type alias Node =
    { id : Maybe ID -- may not have been set yet
    , metadata : Maybe Metadata
    , content : String
    }


fromString : String -> Node
fromString =
    Node Nothing Nothing


type Metadata
    = Note


withMetadata : Metadata -> Node -> Node
withMetadata metadata node =
    { node | metadata = Just metadata }
