module Node exposing
    ( Node, content, setContent, isEmpty
    , note, isNote
    , node
    , encode, decoder
    )

{-|

@docs Node, content, setContent, isEmpty

@docs note, isNote

@docs node

@docs encode, decoder

-}

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
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


encode : Node -> Encode.Value
encode (Node guts) =
    Encode.object
        [ ( "metadata"
          , case guts.metadata of
                Just meta ->
                    encodeMetadata meta

                Nothing ->
                    Encode.null
          )
        , ( "content", Content.encode guts.content )
        ]


encodeMetadata : Metadata -> Encode.Value
encodeMetadata meta =
    case meta of
        Note ->
            Encode.string "note"


decoder : Decoder Node
decoder =
    Decode.map2
        (\metadata content_ ->
            Node { metadata = metadata, content = content_ }
        )
        (Decode.field "metadata" (Decode.nullable metadataDecoder))
        (Decode.field "content" Content.decoder)


metadataDecoder : Decoder Metadata
metadataDecoder =
    Decode.andThen
        (\kind ->
            case kind of
                "note" ->
                    Decode.succeed Note

                _ ->
                    Decode.fail ("I don't know about " ++ kind ++ " metadata.")
        )
        Decode.string
