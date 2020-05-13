module Node exposing
    ( Node, content, setContent, isEmpty
    , title, isTitle
    , node
    , encode, decoder
    )

{-|

@docs Node, content, setContent, isEmpty

@docs title, isTitle

@docs node

@docs encode, decoder

-}

import Content exposing (Content)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Node
    = Node
        { metadata : Maybe Metadata
        , content : Content
        }


title : Content -> Node
title content_ =
    Node
        { metadata = Just Title
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
    = Title


isTitle : Node -> Bool
isTitle (Node guts) =
    guts.metadata == Just Title


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
        Title ->
            Encode.string "title"


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
                -- Title used to be called Note
                "note" ->
                    Decode.succeed Title

                "title" ->
                    Decode.succeed Title

                _ ->
                    Decode.fail ("I don't know about " ++ kind ++ " metadata.")
        )
        Decode.string
