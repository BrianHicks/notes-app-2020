module Node exposing
    ( Node, content
    , note, isNote
    )

{-|

@docs Node, content

@docs note, isNote

-}


type Node
    = Node
        { metadata : Maybe Metadata
        , content : String
        }


note : String -> Node
note content_ =
    Node
        { metadata = Just Note
        , content = content_
        }


content : Node -> String
content (Node node) =
    node.content


type Metadata
    = Note


isNote : Node -> Bool
isNote (Node node) =
    node.metadata == Just Note


withMetadata : Metadata -> Node -> Node
withMetadata metadata (Node node) =
    Node { node | metadata = Just metadata }
