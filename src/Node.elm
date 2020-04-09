module Node exposing
    ( Node, content
    , note, isNote
    , node
    )

{-|

@docs Node, content

@docs note, isNote

@docs node

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


node : String -> Node
node content_ =
    Node
        { metadata = Nothing
        , content = content_
        }


content : Node -> String
content (Node guts) =
    guts.content


type Metadata
    = Note


isNote : Node -> Bool
isNote (Node guts) =
    guts.metadata == Just Note
