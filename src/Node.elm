module Node exposing (Metadata(..), Node, asNote, content, fromString, isNote)


type Node
    = Node
        { metadata : Maybe Metadata
        , content : String
        }


fromString : String -> Node
fromString content_ =
    Node
        { metadata = Nothing
        , content = content_
        }


content : Node -> String
content (Node node) =
    node.content


type Metadata
    = Note


asNote : Node -> Node
asNote =
    withMetadata Note


isNote : Node -> Bool
isNote (Node node) =
    node.metadata == Just Note


withMetadata : Metadata -> Node -> Node
withMetadata metadata (Node node) =
    Node { node | metadata = Just metadata }
