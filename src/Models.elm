module Models exposing (ShortUrl)


type alias ShortUrl =
    { id : String
    , originalUrl : String
    , anonymous : Bool
    }
