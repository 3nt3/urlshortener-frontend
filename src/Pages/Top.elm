module Pages.Top exposing (Model, Msg, Params, page)

import Api exposing (Data(..), HttpError(..))
import Api.Api exposing (apiAddress)
import Api.UrlShortener.Url exposing (shortenUrl)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Models exposing (ShortUrl)
import Shared exposing (init, subscriptions)
import Spa.Document exposing (Document)
import Spa.Page as Page exposing (Page)
import Spa.Url exposing (Url)
import Url


type alias Params =
    ()


type alias Model =
    { url : Url Params
    , urlInput : String
    , urlData : Api.Data ShortUrl
    }


type Msg
    = UrlInput String
    | Shorten
    | GotData (Api.Data ShortUrl)


page : Page Params Model Msg
page =
    Page.application
        { view = view
        , init = init
        , update = update
        , save = save
        , load = load
        , subscriptions = subscriptions
        }


init : Shared.Model -> Url Params -> ( Model, Cmd Msg )
init _ url =
    ( { url = url, urlInput = "", urlData = Api.NotAsked }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlInput text ->
            ( { model | urlInput = text }, Cmd.none )

        Shorten ->
            ( { model | urlData = Api.Loading }, shortenUrl model.urlInput { onResponse = GotData } )

        GotData data ->
            ( { model | urlData = data }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


load : Shared.Model -> Model -> ( Model, Cmd msg )
load shared model =
    ( model, Cmd.none )


save : Model -> Shared.Model -> Shared.Model
save model shared =
    shared


view : Model -> Document Msg
view model =
    { title = "Homepage"
    , body =
        [ column
            [ width fill
            , height fill
            ]
            [ column
                [ centerY
                , centerX
                , spacing 20
                ]
                [ el
                    [ Font.size 50
                    , Font.family
                        [ Font.typeface "Source Sans Pro"
                        , Font.serif
                        ]
                    , centerX
                    ]
                    (text "3nt3.xyz URL shortener v0.1")
                , viewForm model
                , viewLink model
                ]
            ]
        ]
    }


inputStyle : List (Attribute msg)
inputStyle =
    [ height (px 50)
    , Border.rounded 4
    , Border.solid
    , Border.width 1
    , Border.color (rgb255 127 140 141)
    ]


buttonStyle : List (Attribute msg)
buttonStyle =
    inputStyle
        ++ [ Font.bold
           , padding 10
           , Font.color (rgb 1 1 1)
           , Border.width 0
           ]


onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )


viewForm : Model -> Element Msg
viewForm model =
    row
        [ centerX
        , spacing 10
        ]
        [ Input.text
            (inputStyle
                ++ (if String.isEmpty model.urlInput then
                        []

                    else
                        [ onEnter Shorten ]
                   )
            )
            { label = Input.labelHidden "URL"
            , placeholder = Just (Input.placeholder [] (text ""))
            , onChange = UrlInput
            , text = model.urlInput
            }
        , Input.button
            ([ Background.color
                (if String.isEmpty model.urlInput then
                    rgb255 127 140 141

                 else
                    rgb255 46 204 113
                )
             ]
                ++ buttonStyle
            )
            { onPress =
                if String.isEmpty model.urlInput then
                    Nothing

                else
                    Just Shorten
            , label =
                case model.urlData of
                    Loading ->
                        text "Loading..."

                    NotAsked ->
                        text "shorten!"

                    Failure _ ->
                        text "error."

                    Success _ ->
                        text "shorten!"
            }
        ]


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
                            (List.map text errors)

                    _ ->
                        text "error"

            Success url ->
                link [ Font.color (rgb255 52 152 219) ] { label = text (Url.toString model.url.rawUrl ++ url.id), url = Url.toString model.url.rawUrl ++ url.id }
        )
