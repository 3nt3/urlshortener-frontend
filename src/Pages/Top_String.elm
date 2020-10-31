module Pages.Top_String exposing (..)

import Api exposing (Data(..), HttpError(..))
import Api.UrlShortener.Url exposing (getUrl)
import Browser.Navigation
import Element exposing (..)
import Element.Font as Font exposing (color)
import Models exposing (ShortUrl)
import Spa.Document as Document exposing (Document)
import Spa.Page as Page exposing (Page)
import Spa.Url exposing (Url)
import Url


type alias Params =
    { top : String }


type alias Model =
    { url : Url Params
    , urlData : Api.Data ShortUrl
    }


type Msg
    = GotData (Api.Data ShortUrl)


page : Page Params Model Msg
page =
    Page.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }


init : Url Params -> ( Model, Cmd Msg )
init url =
    ( { url = url, urlData = NotAsked }, getUrl url.params.top { onResponse = GotData } )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotData data ->
            case data of
                Success url ->
                    ( { model | urlData = data }, Browser.Navigation.load url.originalUrl )

                _ ->
                    ( { model | urlData = data }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Document Msg
view model =
    { title = "Redirecting..."
    , body =
        [ el [ width fill, height fill ]
            (column
                [ centerX, centerY, spacing 20 ]
                [ el
                    [ centerX
                    , Font.size 60
                    , Font.bold
                    , Font.center
                    ]
                    (text
                        (case model.urlData of
                            Loading ->
                                "Loading..."

                            NotAsked ->
                                ""

                            Failure e ->
                                case e of
                                    BadStatus status error ->
                                        if status == 404 then
                                            "404 Not Found"

                                        else if status == 500 then
                                            "500 Internal Server Error"

                                        else
                                            "Unknown Error"

                                    NetworkError ->
                                        "Network error"

                                    _ ->
                                        "Unknow error"

                            Success _ ->
                                "Redirecting..."
                        )
                    )
                , row [ centerX ] [ el [] (text "if you are not automatically redirected, use this link: "), viewLink model ]
                ]
            )
        ]
    }


viewLink : Model -> Element Msg
viewLink model =
    el [ centerX ]
        (case model.urlData of
            Loading ->
                text "Loading..."

            NotAsked ->
                text ""

            Failure e ->
                case e of
                    BadStatus status errors ->
                        column []
                            (List.map viewError errors)

                    _ ->
                        el [ color (rgb255 231 76 60) ] (text "error")

            Success url ->
                link [ Font.color (rgb255 52 152 219) ] { label = text url.originalUrl, url = url.originalUrl }
        )


viewError : String -> Element Msg
viewError error =
    el [ color (rgb255 231 76 60) ] (text error)
