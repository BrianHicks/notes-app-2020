module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Navigation as Navigation
import Css
import Database exposing (Database)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs exposing (css)
import Html.Styled.Events as Events
import Json.Decode as Decode
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
    | UserFinishedEditingNode
    | Focused (Result Dom.Error ())
    | UserSelectedNode Database.ID
    | UserHitEnterOnNode Database.ID


type Effect
    = NoEffect
    | Batch (List Effect)
    | LoadUrl String
    | PushUrl Route
    | FocusOnContent


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
            ( model, PushUrl (Route.parse url) )

        ClickedLink (Browser.External url) ->
            ( model, LoadUrl url )

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
            , Batch
                [ PushUrl (Route.Node id)
                , FocusOnContent
                ]
            )

        UserEditedNode content ->
            case model.editing of
                Just id ->
                    ( { model | database = Database.update id (Node.setContent content) model.database }
                    , NoEffect
                    )

                Nothing ->
                    ( model, NoEffect )

        UserFinishedEditingNode ->
            ( { model | editing = Nothing }, NoEffect )

        Focused _ ->
            -- TODO: report this?
            ( model, NoEffect )

        UserSelectedNode id ->
            ( model
            , PushUrl (Route.Node id)
            )

        UserHitEnterOnNode id ->
            let
                ( newId, database ) =
                    Database.insert (Node.node "") model.database
            in
            case Database.get id model.database |> Maybe.map (Node.isNote << .node) of
                Just True ->
                    ( { model
                        | database = Database.moveInto id newId database
                        , editing = Just newId
                      }
                    , FocusOnContent
                    )

                Just False ->
                    ( { model
                        | database = Database.moveAfter id newId database
                        , editing = Just newId
                      }
                    , FocusOnContent
                    )

                Nothing ->
                    ( model, NoEffect )


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

        FocusOnContent ->
            Task.attempt Focused (Dom.focus "content")


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
            |> List.map
                (\{ id, node } ->
                    Html.li
                        [ Attrs.attribute "role" "button"
                        , Events.onClick (UserSelectedNode id)

                        -- TODO: trigger on space and enter
                        -- TODO: put this in the tabbing order
                        ]
                        [ Html.text (Node.content node) ]
                )
            |> Html.ul []
        , case model.route of
            Route.NotFound ->
                Html.text "Not found!"

            Route.Root ->
                Html.text "Select or create a note!"

            Route.Node id ->
                viewNode model id
        , Html.text (Debug.toString model.route)
        ]


viewNode :
    Model key
    -> Database.ID
    -> Html Msg
viewNode model id =
    case Database.get id model.database of
        Nothing ->
            Html.text "Note not found."

        Just { node, children } ->
            let
                tag =
                    if Node.isNote node then
                        Html.section

                    else
                        Html.li
            in
            tag []
                [ if model.editing == Just id then
                    Html.input
                        [ Attrs.attribute "aria-label" "Content"
                        , Attrs.id "content"
                        , Attrs.value (Node.content node)
                        , Events.onInput UserEditedNode
                        , Events.onBlur UserFinishedEditingNode
                        , Events.on "keydown"
                            (Decode.andThen
                                (\key ->
                                    case key of
                                        -- return
                                        -- TODO: add a next sibling node from this
                                        13 ->
                                            Decode.succeed (UserHitEnterOnNode id)

                                        -- escape
                                        27 ->
                                            Decode.succeed UserFinishedEditingNode

                                        _ ->
                                            Decode.fail ""
                                )
                                Events.keyCode
                            )
                        ]
                        []

                  else if Node.isNote node then
                    Html.h1 [] [ Html.text (Node.content node) ]

                  else
                    Html.text (Node.content node)
                , children
                    |> List.map (viewNode model)
                    |> Html.ul []
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
