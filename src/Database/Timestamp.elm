module Database.Timestamp exposing
    ( Generator, generator, Problem(..), problemToString, sendAt, receiveAt
    , NodeID, nodeIdFromInt
    , Timestamp, init, compare, toString, fromString, ParsingProblem(..), parsingProblemToString, encode, decoder
    )

{-| An implementation of [Hybrid Logical Clocks][hlc]

[hlc]: https://cse.buffalo.edu/tech-reports/2014-04.pdf


# Generator

@docs Generator, generator, Problem, problemToString, sendAt, receiveAt


## Node ID

@docs NodeID, nodeIdFromInt


## Timestamp

@docs Timestamp, init, compare, toString, fromString, ParsingProblem, parsingProblemToString, encode, decoder

-}

import Hex
import Iso8601
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Time exposing (Posix)



-- GENERATOR


type alias Options =
    { maxDriftMillis : Int }


defaultOptions : Options
defaultOptions =
    { maxDriftMillis = 60000 }


type Generator
    = Generator
        { options : Options
        , millis : Int
        , counter : Int
        , node : NodeID
        }


{-| Create a timestamp generator given a number to use for this device's
ID. This number should be under 16^16, but practically it doesn't matter
since the biggest `Number` value JS can deal with is 2^53-1.
-}
generator : NodeID -> Generator
generator node =
    Generator
        { options = defaultOptions
        , millis = 0
        , counter = 0
        , node = node
        }


type Problem
    = TooMuchClockDrift { got : Int, limit : Int }
    | CounterTooHigh { got : Int, limit : Int }
    | DuplicateNode NodeID


problemToString : Problem -> String
problemToString problem =
    case problem of
        TooMuchClockDrift { got, limit } ->
            "Too much clock drift. I saw a drift of "
                ++ String.fromInt got
                ++ "ms, but the maximum drift is "
                ++ String.fromInt limit
                ++ "ms"

        CounterTooHigh { got, limit } ->
            "Counter over limit. I got "
                ++ String.fromInt got
                ++ " but the maximum size is "
                ++ String.fromInt limit

        DuplicateNode (NodeID got) ->
            "I got a duplicate node ID: " ++ String.fromInt got


sendAt : Posix -> Generator -> Result Problem ( Timestamp, Generator )
sendAt phys (Generator local) =
    let
        physMillis =
            Time.posixToMillis phys

        millisNew =
            max local.millis physMillis

        counterNew =
            if local.millis == millisNew then
                local.counter + 1

            else
                0
    in
    if millisNew - physMillis > local.options.maxDriftMillis then
        Err
            (TooMuchClockDrift
                { got = millisNew - physMillis
                , limit = local.options.maxDriftMillis
                }
            )

    else if counterNew > maxCounterSize then
        Err
            (CounterTooHigh
                { got = counterNew
                , limit = maxCounterSize
                }
            )

    else
        Ok
            ( Timestamp
                { millis = millisNew
                , counter = counterNew
                , node = local.node
                }
            , Generator
                { local
                    | millis = millisNew
                    , counter = counterNew
                    , node = local.node
                }
            )


receiveAt : Posix -> Generator -> Timestamp -> Result Problem Generator
receiveAt phys (Generator local) (Timestamp remote) =
    let
        physMillis =
            Time.posixToMillis phys
    in
    if local.node == remote.node then
        Err (DuplicateNode remote.node)

    else if remote.millis - physMillis > local.options.maxDriftMillis then
        Err
            (TooMuchClockDrift
                { got = remote.millis - physMillis
                , limit = local.options.maxDriftMillis
                }
            )

    else
        let
            millisNew =
                max (max local.millis physMillis) remote.millis

            counterNew =
                if millisNew == local.millis && millisNew == remote.millis then
                    max local.counter remote.counter + 1

                else if millisNew == local.millis then
                    local.counter + 1

                else if millisNew == remote.millis then
                    remote.counter + 1

                else
                    0
        in
        if millisNew - physMillis > local.options.maxDriftMillis then
            Err
                (TooMuchClockDrift
                    { got = millisNew - physMillis
                    , limit = local.options.maxDriftMillis
                    }
                )

        else if counterNew > maxCounterSize then
            Err
                (CounterTooHigh
                    { got = counterNew
                    , limit = maxCounterSize
                    }
                )

        else
            (Ok << Generator)
                { local
                    | millis = millisNew
                    , counter = counterNew
                    , node = local.node
                }



-- NODE ID


type NodeID
    = NodeID Int


nodeIdFromInt : Int -> NodeID
nodeIdFromInt =
    NodeID << abs


nodeIdToInt : NodeID -> Int
nodeIdToInt (NodeID id) =
    id



-- TIMESTAMP


type Timestamp
    = Timestamp
        { millis : Int
        , counter : Int
        , node : NodeID
        }


init : { millis : Int, counter : Int, node : NodeID } -> Result Problem Timestamp
init { millis, counter, node } =
    if counter > maxCounterSize then
        Err
            (CounterTooHigh
                { got = counter
                , limit = maxCounterSize
                }
            )

    else
        (Ok << Timestamp)
            { millis = millis
            , counter = counter
            , node = node
            }


compare : Timestamp -> Timestamp -> Basics.Order
compare (Timestamp left) (Timestamp right) =
    Basics.compare
        ( left.millis, left.counter, nodeIdToInt left.node )
        ( right.millis, right.counter, nodeIdToInt right.node )


toString : Timestamp -> String
toString (Timestamp timestamp) =
    String.join "-"
        [ timestamp.millis
            |> Time.millisToPosix
            |> Iso8601.fromTime
        , Hex.toString timestamp.counter
            |> String.padLeft maxCounterHexes '0'
            |> String.right maxCounterHexes
        , nodeIdToInt timestamp.node
            |> Hex.toString
            |> String.padLeft maxNodeHexes '0'
            |> String.right maxNodeHexes
        ]


{-| TODO: this would be better as a real parser!
-}
fromString : String -> Result ParsingProblem Timestamp
fromString string =
    case String.split "-" string of
        [ yearPart, monthPart, time, counterPart, nodePart ] ->
            let
                millisResult =
                    (yearPart ++ "-" ++ monthPart ++ "-" ++ time)
                        |> Iso8601.toTime
                        |> Result.mapError (\_ -> BadTime)
                        |> Result.map Time.posixToMillis

                counterResult =
                    counterPart
                        |> Hex.fromString
                        |> Result.mapError (BadHex "counter")

                nodeResult =
                    nodePart
                        |> Hex.fromString
                        |> Result.mapError (BadHex "node")
                        |> Result.map nodeIdFromInt
            in
            case ( millisResult, counterResult, nodeResult ) of
                ( Ok millis, Ok counter, Ok node ) ->
                    { millis = millis
                    , counter = counter
                    , node = node
                    }
                        |> init
                        |> Result.mapError SemanticProblem

                ( Err err, _, _ ) ->
                    Err err

                ( _, Err err, _ ) ->
                    Err err

                ( _, _, Err err ) ->
                    Err err

        otherSize ->
            Err (WrongSegmentCount (List.length otherSize))


type ParsingProblem
    = WrongSegmentCount Int
    | BadTime
    | BadHex String String
    | SemanticProblem Problem


parsingProblemToString : ParsingProblem -> String
parsingProblemToString problem =
    case problem of
        WrongSegmentCount howMany ->
            "Wrong segment count. I got "
                ++ String.fromInt howMany
                ++ " fields separated by `-`, but expected exactly 3."

        BadTime ->
            "Bad ISO8601 time format"

        BadHex field message ->
            "Bad hex value in "
                ++ field
                ++ ": "
                ++ message

        SemanticProblem semanticProblem ->
            problemToString semanticProblem



-- JSON


decoder : Decoder Timestamp
decoder =
    Decode.andThen
        (\raw ->
            case fromString raw of
                Ok timestamp ->
                    Decode.succeed timestamp

                Err err ->
                    Decode.fail (parsingProblemToString err)
        )
        Decode.string


encode : Timestamp -> Value
encode =
    Encode.string << toString



-- CONSTANTS


maxCounterHexes : Int
maxCounterHexes =
    4


maxCounterSize : Int
maxCounterSize =
    16 ^ maxCounterHexes


maxNodeHexes : Int
maxNodeHexes =
    16


maxNodeSize : Int
maxNodeSize =
    16 ^ maxNodeHexes
