module Node exposing (Metadata(..), Node, fromString, withMetadata)


type alias Node =
    { metadata : Maybe Metadata
    , content : String
    }


fromString : String -> Node
fromString =
    Node Nothing


type Metadata
    = Note


withMetadata : Metadata -> Node -> Node
withMetadata metadata node =
    { node | metadata = Just metadata }
