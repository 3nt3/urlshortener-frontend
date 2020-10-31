module Api.UrlShortener.Url exposing (..)

import Api
import Api.Api exposing (apiAddress)
import Http
import Json.Decode as Json
import Json.Encode as Encode
import Models exposing (ShortUrl)


urlEncoder : String -> Encode.Value
urlEncoder url =
    Encode.object
        [ ( "original_url", Encode.string url ) ]


urlDecoder : Json.Decoder ShortUrl
urlDecoder =
    Json.map3 ShortUrl
        (Json.field "id" Json.string)
        (Json.field "original_url" Json.string)
        (Json.field "anonymous" Json.bool)


shortenUrl : String -> { onResponse : Api.Data ShortUrl -> msg } -> Cmd msg
shortenUrl originalUrl options =
    Http.post
        { url = apiAddress ++ "/url"
        , body = Http.jsonBody (urlEncoder originalUrl)
        , expect = Api.expectJson options.onResponse urlDecoder
        }


getUrl : String -> { onResponse : Api.Data ShortUrl -> msg } -> Cmd msg
getUrl id options =
    Http.get
        { url = apiAddress ++ "/url/" ++ id ++ "?response=json"
        , expect = Api.expectJson options.onResponse urlDecoder
        }
