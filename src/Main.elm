module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import Database exposing (Database)
import Html.Styled as Html exposing (Html)
import Url exposing (Url)


type alias Model =
    { database : Database
    , url : Url
    , key : Navigation.Key
    }


type Msg
    = ClickedLink Browser.UrlRequest
    | UrlChanged Url


init : () -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { database = Database.init
      , url = url
      , key = key
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink (Browser.Internal url) ->
            ( model
            , Navigation.pushUrl model.key (Url.toString url)
            )

        ClickedLink (Browser.External url) ->
            ( model
            , Navigation.load url
            )

        UrlChanged url ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = "Notes"
    , body = [ Html.toUnstyled (Html.text (Debug.toString model)) ]
    }


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = UrlChanged
        , view = view
        }
