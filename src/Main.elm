port module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Navigation as Navigation
import Css
import Database exposing (Database)
import Database.ID as ID exposing (ID)
import Database.LWW as LWW
import Database.Timestamp as Timestamp
import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Attributes as Attrs exposing (css)
import Html.Styled.Events as Events
import Html.Styled.Keyed as Keyed
import Json.Decode as Decode exposing (Decoder, Value)
import Maybe.Extra
import Node exposing (Node)
import Node.Content as Content exposing (Content)
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
    { database : Database
    , url : Url
    , key : key
    , route : Route

    -- view state
    , editing :
        Maybe
            { id : ID
            , input : Result ( String, List String ) Content
            }
    , selection : Maybe Selection
    }


init : Value -> Url -> key -> ( Model key, Effect )
init flags url key =
    let
        { seed } =
            case Decode.decodeValue flagsDecoder flags of
                Ok stuff ->
                    stuff

                Err err ->
                    Debug.todo (Debug.toString err)
    in
    ( { database = Database.empty seed -- TODO: load data
      , url = url
      , key = key
      , route = Route.parse url

      -- view state
      , editing = Nothing
      , selection = Nothing
      }
    , NoEffect
    )


flagsDecoder : Decoder { seed : Random.Seed }
flagsDecoder =
    Decode.map (\seed -> { seed = seed })
        (Decode.field "seed" (Decode.map Random.initialSeed Decode.int))


type Msg
    = ClickedLink Browser.UrlRequest
    | UrlChanged Url
    | UserClickedNewNote
    | UserEditedNode String


type Effect
    = NoEffect
    | Batch (List Effect)
    | LoadUrl String
    | PushUrl Route


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
            let
                ( id, database ) =
                    Database.insert (Node.note Content.empty) model.database
            in
            ( { model
                | database = database
                , editing =
                    Just
                        { id = id
                        , input = Ok Content.empty
                        }
              }
            , PushUrl (Route.Node id)
            )

        UserEditedNode input ->
            case model.editing of
                Nothing ->
                    ( model
                    , NoEffect
                    )

                Just editing ->
                    case Content.fromString input of
                        Ok content ->
                            ( { model
                                | editing = Just { editing | input = Ok content }
                                , database = Database.update editing.id (Node.setContent content) model.database
                              }
                            , NoEffect
                            )

                        Err problems ->
                            ( { model | editing = Just { editing | input = Err ( input, problems ) } }
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


port persistLogEvent : Value -> Cmd msg


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
            |> Database.filter Node.isNote
            |> List.map (\{ node } -> Html.li [] [ Content.toHtml (Node.content node) ])
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
    case Database.get id model.database of
        Nothing ->
            Html.text "Node not found!"

        Just { node } ->
            if Maybe.map .id model.editing == Just id then
                Html.textarea
                    [ Attrs.value (Content.toString (Node.content node))
                    , Attrs.attribute "aria-label" "Content"
                    , Attrs.id "content"
                    , Events.onInput UserEditedNode
                    ]
                    []

            else
                Content.toHtml (Node.content node)


main : Program Value (Model Navigation.Key) Msg
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
