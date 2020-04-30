module Database.LWW exposing (LWW, init, map, value)

{-| Last Write Wins
-}


type LWW a
    = LWW {- Timestamp -} a


init : a -> LWW a
init =
    LWW


value : LWW a -> a
value (LWW a) =
    a


map : (a -> b) -> LWW a -> LWW b
map fn (LWW a) =
    LWW (fn a)
