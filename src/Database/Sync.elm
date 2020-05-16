module Database.Sync exposing (Sync, isValid)


type alias Sync =
    { host : String
    , database : String
    , username : String
    , password : String
    }


isValid : Sync -> Bool
isValid { host, database, username, password } =
    (host /= "")
        && (database /= "")
        && (username /= "")
        && (password /= "")
