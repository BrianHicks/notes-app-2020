module Main exposing (..)

import Browser
import Browser.Navigation as Navigation
import Css
import Database exposing (Database)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs exposing (css)
import Html.Styled.Events as Events
import Node exposing (Node)
import Route exposing (Route)
import Sort
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
    , editing : Maybe Database.ID
    }


type Msg
    = ClickedLink Browser.UrlRequest
    | UrlChanged Url
    | ClickedNewNote
    | UserEditedNode String


type Effect
    = NoEffect
    | LoadUrl String
    | PushUrl Route


init : () -> Url -> key -> ( Model key, Effect )
init flags url key =
    ( { database = Database.empty
      , url = url
      , key = key
      , route = Route.parse url

      -- view state
      , editing = Nothing
      }
    , NoEffect
    )


update : Msg -> Model key -> ( Model key, Effect )
update msg model =
    case msg of
        ClickedLink (Browser.Internal url) ->
            ( model
            , PushUrl (Route.parse url)
            )

        ClickedLink (Browser.External url) ->
            ( model
            , LoadUrl url
            )

        UrlChanged url ->
            ( { model | route = Route.parse url }
            , NoEffect
            )

        ClickedNewNote ->
            let
                ( id, database ) =
                    Database.insert (Node.note "") model.database
            in
            ( { model
                | database = database
                , editing = Just id
              }
            , PushUrl (Route.Note id)
            )

        UserEditedNode content ->
            case model.editing of
                Just id ->
                    ( { model | database = Database.update id (Node.setContent content) model.database }
                    , NoEffect
                    )

                Nothing ->
                    ( model, NoEffect )


perform : ( Model Navigation.Key, Effect ) -> ( Model Navigation.Key, Cmd Msg )
perform ( model, effect ) =
    case effect of
        NoEffect ->
            ( model, Cmd.none )

        LoadUrl url ->
            ( model, Navigation.load url )

        PushUrl url ->
            ( model, Navigation.pushUrl model.key (Route.toString url) )


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
        [ Html.button [ Events.onClick ClickedNewNote ] [ Html.text "New Note" ]
        , model.database
            |> Database.filter Node.isNote
            |> List.map (\{ node } -> Html.li [] [ Html.text (Node.content node) ])
            |> Html.ul []
        , case model.route of
            Route.NotFound ->
                Html.text "Not found!"

            Route.Root ->
                Html.text "Select or create a note!"

            Route.Note id ->
                viewNote model id
        , Html.text (Debug.toString model.route)
        ]


viewNote :
    Model key
    -> Database.ID
    -> Html Msg
viewNote model id =
    case Database.get id model.database of
        Nothing ->
            Html.text "Note not found."

        Just note ->
            if model.editing == Just id then
                Html.input
                    [ Attrs.attribute "aria-label" "Title"
                    , Attrs.id "title"
                    , Attrs.value (Node.content note.node)
                    , Events.onInput UserEditedNode
                    ]
                    []

            else
                Html.text (Node.content note.node)


main : Program () (Model Navigation.Key) Msg
main =
    Browser.application
        { init = \flags url key -> init flags url key |> perform
        , update = \model msg -> update model msg |> perform
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = UrlChanged
        , view = view
        }
