module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Navigation as Navigation
import Css
import Database.LWW as LWW
import Database.Log as Log exposing (Log)
import Database.Timestamp as Timestamp
import Dict
import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Attributes as Attrs exposing (css)
import Html.Styled.Events as Events
import Html.Styled.Keyed as Keyed
import Json.Decode as Decode exposing (Decoder)
import Maybe.Extra
import Node exposing (Node)
import Node.Content as Content
import Random
import Route exposing (Route)
import Selection exposing (Selection)
import Sort
import Task
import Time exposing (Posix)
import UUID exposing (UUID)
import Url exposing (Url)


type alias Model key =
    { database : Log
    , url : Url
    , key : key
    , route : Route

    -- view state
    , editing :
        Maybe
            { id : String
            , input : String
            , errors : List String
            }
    , selection : Maybe Selection
    }


type Msg
    = ClickedLink Browser.UrlRequest
    | UrlChanged Url


type Effect
    = NoEffect
    | Batch (List Effect)
    | LoadUrl String
    | PushUrl Route
    | GetTimeAnd (Posix -> Msg)


init : () -> Url -> key -> ( Model key, Effect )
init flags url key =
    ( { database =
            Log.init
                -- TODO: current time or something
                (Random.initialSeed 0)
                -- TODO: set persistently
                (Timestamp.nodeIdFromInt 0)
      , url = url
      , key = key
      , route = Route.parse url

      -- view state
      , editing = Nothing
      , selection = Nothing
      }
    , NoEffect
    )


update : Msg -> Model key -> ( Model key, Effect )
update msg model =
    case msg of
        ClickedLink (Browser.Internal url) ->
            ( model, PushUrl (Route.parse url) )

        ClickedLink (Browser.External url) ->
            ( model, LoadUrl url )

        UrlChanged url ->
            ( { model | route = Route.parse url }
            , NoEffect
            )


perform : Model Navigation.Key -> Effect -> Cmd Msg
perform model effect =
    case effect of
        NoEffect ->
            Cmd.none

        Batch effects ->
            Cmd.batch (List.map (perform model) effects)

        LoadUrl url ->
            Navigation.load url

        PushUrl route ->
            if model.route == route then
                Cmd.none

            else
                Navigation.pushUrl model.key (Route.toString route)

        GetTimeAnd next ->
            Task.perform next Time.now


subscriptions : Model key -> Sub Msg
subscriptions model =
    Sub.none


view : Model key -> Browser.Document Msg
view model =
    { title = "Notes"
    , body = [ Html.toUnstyled (viewApplication model) ]
    }


viewApplication : Model key -> Html Msg
viewApplication model =
    Html.main_ []
        [ model.database.state
            |> Dict.foldr
                (\id { content } acc ->
                    Html.li
                        [ Attrs.attribute "role" "button"

                        -- , Events.onClick (UserSelectedNode id)
                        -- TODO: trigger on space and enter
                        -- TODO: put this in the tabbing order
                        ]
                        [ case content of
                            Just inner ->
                                Html.text (LWW.value inner)

                            Nothing ->
                                Html.text "no content! Fine but unusual. Maybe missing a log entry?"
                        ]
                        :: acc
                )
                []
            |> Html.ul []
            |> List.singleton
            |> Html.nav []
        , case model.route of
            Route.NotFound ->
                Html.text "Not found!"

            Route.Root ->
                Html.text "Select or create a note!"

            Route.Node id ->
                Html.text "NODE"
        , Html.text (Debug.toString model.selection)
        ]


main : Program () (Model Navigation.Key) Msg
main =
    Browser.application
        { init =
            \flags url key ->
                let
                    ( model, effect ) =
                        init flags url key
                in
                ( model, perform model effect )
        , update =
            \model msg ->
                let
                    ( newModel, effect ) =
                        update model msg
                in
                ( newModel
                , perform newModel effect
                )
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = UrlChanged
        , view = view
        }
