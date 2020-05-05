port module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Navigation as Navigation
import Css
import Database.ID as ID exposing (ID)
import Database.LWW as LWW
import Database.Log as Log exposing (Log)
import Database.Timestamp as Timestamp
import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Attributes as Attrs exposing (css)
import Html.Styled.Events as Events
import Html.Styled.Keyed as Keyed
import Json.Decode as Decode exposing (Decoder, Value)
import Maybe.Extra
import Node exposing (Node)
import Node.Content as Content
import Process
import Random
import Route exposing (Route)
import Selection exposing (Selection)
import Sort
import Sort.Dict as Dict
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
            { id : ID
            , input : String
            , errors : List String
            , saveAfter : Maybe Posix
            }
    , selection : Maybe Selection
    }


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


type Msg
    = ClickedLink Browser.UrlRequest
    | UrlChanged Url
    | UserClickedNewNote
    | UserClickedNewNoteAt Posix
    | UserEditedNode String
    | UserEditedNodeAt String Posix
    | DelayTriggeredSave Posix


type Effect
    = NoEffect
    | Batch (List Effect)
    | LoadUrl String
    | PushUrl Route
    | GetTimeFor (Posix -> Msg)
    | SaveAfter Float
    | PersistLogEntry Log.Entry


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

        UserClickedNewNote ->
            ( model
            , GetTimeFor UserClickedNewNoteAt
            )

        UserClickedNewNoteAt time ->
            case Log.newNode time "" model.database of
                Ok ( id, database, toPersist ) ->
                    let
                        saveAfter =
                            Time.millisToPosix (Time.posixToMillis time + 1000)
                    in
                    ( { model
                        | database = database
                        , editing =
                            Just
                                { id = id
                                , input = ""
                                , errors = []
                                , saveAfter = Just saveAfter
                                }
                      }
                    , Batch
                        [ PushUrl (Route.Node id)
                        , SaveAfter 1000
                        , PersistLogEntry toPersist
                        ]
                    )

                Err err ->
                    Debug.todo (Debug.toString err)

        UserEditedNode input ->
            ( model
            , GetTimeFor (UserEditedNodeAt input)
            )

        UserEditedNodeAt input time ->
            case model.editing of
                Just editing ->
                    let
                        saveAfter =
                            Time.millisToPosix (Time.posixToMillis time + 1000)
                    in
                    ( { model | editing = Just { editing | input = input, saveAfter = Just saveAfter } }
                    , SaveAfter 1000
                    )

                Nothing ->
                    ( model
                    , NoEffect
                    )

        DelayTriggeredSave now ->
            let
                shouldSave =
                    -- if we're editing
                    model.editing
                        -- and saveAfter is set
                        |> Maybe.andThen .saveAfter
                        -- and it's in the past
                        |> Maybe.map (\saveAfter -> Time.posixToMillis saveAfter <= Time.posixToMillis now)
            in
            case Maybe.map2 Tuple.pair model.editing shouldSave of
                Just ( editing, True ) ->
                    case Log.edit now editing.id editing.input model.database of
                        Ok ( database, toPersist ) ->
                            ( { model
                                | editing = Just { editing | saveAfter = Nothing }
                                , database = database
                              }
                            , PersistLogEntry toPersist
                            )

                        Err err ->
                            Debug.todo (Debug.toString err)

                _ ->
                    ( model
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

        GetTimeFor next ->
            Task.perform next Time.now

        SaveAfter millis ->
            Process.sleep millis
                |> Task.andThen (\_ -> Time.now)
                |> Task.perform DelayTriggeredSave

        PersistLogEntry entry ->
            persistLogEntry (Log.encode entry)


port persistLogEntry : Value -> Cmd msg


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
        [ Html.button [ Events.onClick UserClickedNewNote ] [ Html.text "New Note" ]
        , model.database
            |> Log.toDict
            |> Dict.foldr
                (\id { content } acc ->
                    Html.li []
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
                viewNode id model
        ]


viewNode : ID -> Model key -> Html Msg
viewNode id model =
    case Log.get id model.database of
        Nothing ->
            Html.text "Node ID not found!"

        Just { content } ->
            if Maybe.map .id model.editing == Just id then
                Html.textarea
                    [ Attrs.value (model.editing |> Maybe.map .input |> Maybe.withDefault "")
                    , Attrs.attribute "aria-label" "Content"
                    , Attrs.id "content"
                    , Events.onInput UserEditedNode
                    ]
                    []

            else
                case Maybe.map LWW.value content of
                    Just something ->
                        Html.text something

                    Nothing ->
                        Html.text "Content is unset. This is legal but unusual. Missing a log entry, maybe?"


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
