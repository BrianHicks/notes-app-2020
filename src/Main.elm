port module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Navigation as Navigation
import Css
import Database exposing (Database)
import Database.ID as ID exposing (ID)
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
        { seed, rows } =
            case Decode.decodeValue flagsDecoder flags of
                Ok stuff ->
                    stuff

                Err err ->
                    Debug.todo (Debug.toString err)
    in
    ( { database = Database.load seed rows
      , url = url
      , key = key
      , route = Route.parse url

      -- view state
      , editing = Nothing
      , selection = Nothing
      }
    , NoEffect
    )


flagsDecoder : Decoder { seed : Random.Seed, rows : List Database.Row }
flagsDecoder =
    Decode.map2 (\seed rows -> { seed = seed, rows = rows })
        (Decode.field "seed" (Decode.map Random.initialSeed Decode.int))
        (Decode.field "rows" (Decode.list (Decode.field "doc" Database.decoder)))


type Msg
    = ClickedLink Browser.UrlRequest
    | UrlChanged Url
    | PouchDBPutSuccessfully Value
    | TimerTriggeredSave
    | UserClickedNewNote
    | UserEditedNode String


type Effect
    = NoEffect
    | Batch (List Effect)
    | LoadUrl String
    | PushUrl Route
    | Put Value


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

        PouchDBPutSuccessfully value ->
            let
                decoder =
                    Decode.map2 Tuple.pair
                        (Decode.field "id" ID.decoder)
                        (Decode.field "rev" Decode.string)
            in
            case Decode.decodeValue decoder value of
                Ok ( id, rev ) ->
                    ( { model | database = Database.updateRevision id rev model.database }
                    , NoEffect
                    )

                Err err ->
                    Debug.todo (Debug.toString err)

        TimerTriggeredSave ->
            let
                ( toPersist, database ) =
                    Database.toPersist model.database
            in
            ( { model | database = database }
            , Batch (List.map (Put << Database.encode) toPersist)
            )

        UserClickedNewNote ->
            let
                ( row, database ) =
                    Database.insert (Node.note Content.empty) model.database
            in
            ( { model
                | database = database
                , editing =
                    Just
                        { id = row.id
                        , input = Ok Content.empty
                        }
              }
            , Batch
                [ PushUrl (Route.Node row.id)
                , NoEffect
                ]
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

        Put value ->
            put value


port put : Value -> Cmd msg


port putSuccessfully : (Value -> msg) -> Sub msg


subscriptions : Model key -> Sub Msg
subscriptions model =
    Sub.batch
        [ putSuccessfully PouchDBPutSuccessfully
        , Time.every 1000 (\_ -> TimerTriggeredSave)
        ]


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
