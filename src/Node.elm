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

import Node.Content as Content exposing (Content)


type Node
    = Node
        { metadata : Maybe Metadata
        , content : Content
        }


note : Content -> Node
note content_ =
    Node
        { metadata = Just Note
        , content = content_
        }


node : Content -> Node
node content_ =
    Node
        { metadata = Nothing
        , content = content_
        }


content : Node -> Content
content (Node guts) =
    guts.content


isEmpty : Node -> Bool
isEmpty (Node guts) =
    Content.isEmpty guts.content


setContent : Content -> Node -> Node
setContent content_ (Node guts) =
    Node { guts | content = content_ }


type Metadata
    = Note


isNote : Node -> Bool
isNote (Node guts) =
    guts.metadata == Just Note
