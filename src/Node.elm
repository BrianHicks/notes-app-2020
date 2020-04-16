module Node exposing
    ( Node, content, setContent, isEmpty
    , note, isNote
    , node
    )

{-|

@docs Node, content, setContent, isEmpty

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


isEmpty : Node -> Bool
isEmpty (Node guts) =
    String.isEmpty guts.content


setContent : String -> Node -> Node
setContent content_ (Node guts) =
    Node { guts | content = content_ }


type Metadata
    = Note


isNote : Node -> Bool
isNote (Node guts) =
    guts.metadata == Just Note
