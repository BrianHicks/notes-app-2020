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
    if string == "" then
        Ok (Content [])

    else
        Ok (Content [ Text string ])


fromList : List Node -> Content
fromList nodes =
    nodes
        |> List.foldl
            (\next prev ->
                case ( next, prev ) of
                    ( Text nextText, (Text prevText) :: rest ) ->
                        Text (prevText ++ nextText) :: rest

                    _ ->
                        next :: prev
            )
            []
        |> Content


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
