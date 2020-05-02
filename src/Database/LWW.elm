module Database.LWW exposing (LWW, init, max, value)

{-| Last Write Wins
-}

import Database.Timestamp as Timestamp exposing (Timestamp)


type LWW a
    = LWW Timestamp a


init : Timestamp -> a -> LWW a
init =
    LWW


value : LWW a -> a
value (LWW _ a) =
    a


max : LWW a -> LWW a -> LWW a
max (LWW timestampA a) (LWW timestampB b) =
    case Timestamp.compare timestampA timestampB of
        LT ->
            LWW timestampB b

        GT ->
            LWW timestampA a

        EQ ->
            -- TODO: what to do if a and b aren't equal?
            LWW timestampB b
