module Node.Content exposing
    ( Content, fromList, fromString, toList, toString
    , Node, text
    )

{-|

@docs Content, fromList, fromString, toList, toString

@docs Node, text

-}


type Content
    = Content (List Node)


fromString : String -> Result () Content
fromString string =
    Ok (Content [ Text string ])


fromList : List Node -> Content
fromList =
    Content


toString : Content -> String
toString (Content nodes) =
    nodes
        |> List.map nodeToString
        |> String.concat


toList : Content -> List Node
toList (Content guts) =
    guts



-- NODES


type Node
    = Text String


text : String -> Node
text =
    Text


nodeToString : Node -> String
nodeToString node =
    case node of
        Text text_ ->
            text_
