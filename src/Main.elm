module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import Database exposing (Database)
import Html.Styled as Html exposing (Html)
import Url exposing (Url)


type alias Model =
    { database : Database }


type Msg
    = UrlRequest Browser.UrlRequest
    | UrlChange Url


init : () -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { database = Database.init }
    , Cmd.none
    )


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = \msg model -> ( model, Cmd.none )
        , subscriptions = \model -> Sub.none
        , onUrlRequest = UrlRequest
        , onUrlChange = UrlChange
        , view =
            \model ->
                { title = "Notes"
                , body = [ Html.toUnstyled (Html.text "TODO") ]
                }
        }
