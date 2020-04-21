module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Navigation as Navigation
import Css
import Database exposing (Database)
import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Attributes as Attrs exposing (css)
import Html.Styled.Events as Events
import Json.Decode as Decode exposing (Decoder)
import Maybe.Extra
import Node exposing (Node)
import Route exposing (Route)
import Selection exposing (Selection)
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
    , selection : Maybe Selection
    }


type Msg
    = ClickedLink Browser.UrlRequest
    | UrlChanged Url
    | ClickedNewNote
    | UserWantsToEditNode Database.ID
    | UserEditedNode String
    | UserFinishedEditingNode
    | Focused (Result Dom.Error ())
    | UserSelectedNode Database.ID
    | UserChangedSelection Selection
    | UserHitEnterOnNode Database.ID
    | UserHitTabToIndent Database.ID
    | UserHitShiftTabToDedent Database.ID
    | UserWantsToDeleteNode Database.ID
    | UserWantsToMoveNodeUp Database.ID
    | UserWantsToMoveNodeDown Database.ID
    | UserWantsToNavigateUp
    | UserWantsToNavigateDown


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

        UserWantsToEditNode id ->
            case Database.get id model.database of
                Just _ ->
                    ( { model | editing = Just id }, FocusOnContent )

                Nothing ->
                    ( model, NoEffect )

        UserEditedNode content ->
            case model.editing of
                Just id ->
                    ( { model | database = Database.update id (Node.setContent content) model.database }
                    , NoEffect
                    )

                Nothing ->
                    ( model, NoEffect )

        UserFinishedEditingNode ->
            ( { model
                | editing = Nothing
                , selection = Nothing
              }
            , NoEffect
            )

        Focused _ ->
            -- TODO: report errors that happen?
            ( model, NoEffect )

        UserSelectedNode id ->
            ( model
            , PushUrl (Route.Node id)
            )

        UserChangedSelection selection ->
            ( { model | selection = Just selection }
            , NoEffect
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

        UserHitTabToIndent id ->
            case
                model.database
                    |> Database.previousSibling id
                    |> Maybe.andThen (\siblingId -> Database.get siblingId model.database)
            of
                Just previousSibling ->
                    ( { model
                        | database =
                            -- TODO: this looks like some logic that
                            -- should live inside Database.elm. It's making
                            -- a decision where to move something based on
                            -- the field values when the intent could be
                            -- clearer as "moveToLastChild" or similar.
                            case previousSibling.children |> List.reverse |> List.head of
                                Nothing ->
                                    Database.moveInto previousSibling.id id model.database

                                Just lastChild ->
                                    Database.moveAfter lastChild id model.database
                      }
                    , FocusOnContent
                    )

                Nothing ->
                    ( model, NoEffect )

        UserHitShiftTabToDedent id ->
            case Database.get id model.database |> Maybe.andThen .parent of
                Just parent ->
                    ( { model | database = Database.moveAfter parent id model.database }
                    , FocusOnContent
                    )

                Nothing ->
                    ( model, NoEffect )

        UserWantsToDeleteNode id ->
            -- TODO: if we were focused on editing this comment, change focus
            -- to the previous sibling.
            ( { model | database = Database.delete id model.database }
            , NoEffect
            )

        UserWantsToMoveNodeUp id ->
            case
                Maybe.Extra.orListLazy
                    [ \_ -> Database.previousSibling id model.database
                    , \_ -> Database.get id model.database |> Maybe.andThen .parent
                    ]
            of
                Just target ->
                    ( { model | database = Database.moveBefore target id model.database }
                    , FocusOnContent
                    )

                Nothing ->
                    ( model
                    , NoEffect
                    )

        UserWantsToNavigateUp ->
            case
                Maybe.Extra.orListLazy
                    [ \_ -> model.editing |> Maybe.andThen (\id -> Database.previousSibling id model.database)
                    , \_ -> model.editing |> Maybe.andThen (\id -> Database.get id model.database) |> Maybe.andThen .parent
                    ]
            of
                Just target ->
                    ( { model | editing = Just target }
                    , FocusOnContent
                    )

                Nothing ->
                    ( model
                    , NoEffect
                    )

        UserWantsToMoveNodeDown id ->
            case
                Maybe.Extra.orListLazy
                    [ \_ -> Database.nextSibling id model.database
                    , \_ ->
                        Database.get id model.database
                            |> Maybe.andThen .parent
                            |> Maybe.andThen (\parentId -> Database.nextSibling parentId model.database)
                    ]
            of
                Just target ->
                    ( { model | database = Database.moveAfter target id model.database }
                    , FocusOnContent
                    )

                Nothing ->
                    ( model
                    , NoEffect
                    )

        UserWantsToNavigateDown ->
            case
                Maybe.Extra.orListLazy
                    [ \_ -> model.editing |> Maybe.andThen (\id -> Database.nextSibling id model.database)
                    , \_ ->
                        model.editing
                            |> Maybe.andThen (\id -> Database.get id model.database)
                            |> Maybe.andThen .parent
                            |> Maybe.andThen (\id -> Database.nextSibling id model.database)
                    ]
            of
                Just target ->
                    ( { model | editing = Just target }
                    , FocusOnContent
                    )

                Nothing ->
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
            |> List.singleton
            |> Html.nav []
        , case model.route of
            Route.NotFound ->
                Html.text "Not found!"

            Route.Root ->
                Html.text "Select or create a note!"

            Route.Node id ->
                viewNode model id
        , Html.text (Debug.toString model.selection)
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
                    Html.textarea
                        [ Attrs.attribute "aria-label" "Content"
                        , Attrs.attribute "is" "note-input"
                        , Attrs.id "content"
                        , Events.onInput UserEditedNode
                        , Events.onBlur UserFinishedEditingNode
                        , nodeInputKeydownHotkeys id node
                        , nodeInputKeyupHotkeys model.selection
                        , nodeInputSelectionChange
                        ]
                        [ Html.text (Node.content node) ]

                  else if Node.isNote node then
                    Html.button
                        [ Events.onClick (UserWantsToEditNode id) ]
                        [ Html.h1 [] [ Html.text (Node.content node) ] ]

                  else
                    Html.button
                        [ Events.onClick (UserWantsToEditNode id) ]
                        [ Html.text (Node.content node) ]
                , if List.isEmpty children then
                    Html.text ""

                  else
                    children
                        |> List.map (viewNode model)
                        |> Html.ul []
                ]


nodeInputKeydownHotkeys : Database.ID -> Node -> Attribute Msg
nodeInputKeydownHotkeys id node =
    Decode.map3
        (\key shift alt ->
            { key = key
            , shift = shift
            , alt = alt
            }
        )
        (Decode.field "key" Decode.string)
        (Decode.field "shiftKey" Decode.bool)
        (Decode.field "altKey" Decode.bool)
        |> Decode.andThen
            (\{ key, shift, alt } ->
                case key of
                    "Tab" ->
                        Decode.succeed
                            { message =
                                if shift then
                                    UserHitShiftTabToDedent id

                                else
                                    UserHitTabToIndent id
                            , stopPropagation = True
                            , preventDefault = True
                            }

                    "Enter" ->
                        Decode.succeed
                            { message = UserHitEnterOnNode id
                            , stopPropagation = False
                            , preventDefault = False
                            }

                    "Escape" ->
                        Decode.succeed
                            { message = UserFinishedEditingNode
                            , stopPropagation = False
                            , preventDefault = False
                            }

                    "Backspace" ->
                        if Node.isEmpty node then
                            Decode.succeed
                                { message = UserWantsToDeleteNode id
                                , stopPropagation = False
                                , preventDefault = False
                                }

                        else
                            Decode.fail "ignoring backspace on non-empty node"

                    "Up" ->
                        if alt then
                            Decode.succeed
                                { message = UserWantsToMoveNodeUp id
                                , stopPropagation = True
                                , preventDefault = True
                                }

                        else
                            Decode.fail "ignoring up without alt key"

                    "Down" ->
                        if alt then
                            Decode.succeed
                                { message = UserWantsToMoveNodeDown id
                                , stopPropagation = True
                                , preventDefault = True
                                }

                        else
                            Decode.fail "ignoring down without alt key"

                    _ ->
                        Decode.fail "unhandled key"
            )
        |> Events.custom "keydown"


nodeInputKeyupHotkeys : Maybe Selection -> Attribute Msg
nodeInputKeyupHotkeys maybeSelection =
    Events.on "keyup" <|
        case maybeSelection of
            Nothing ->
                Decode.fail "no selection"

            Just selection ->
                Decode.map2 Tuple.pair
                    (Decode.field "key" Decode.string)
                    (Decode.field "shiftKey" Decode.bool)
                    |> Decode.andThen
                        (\( key, shiftKey ) ->
                            if shiftKey then
                                Decode.fail "don't mess with while selecting"

                            else if key == "ArrowUp" && Selection.atStart selection then
                                Decode.succeed UserWantsToNavigateUp

                            else if key == "ArrowDown" && Selection.atEnd selection then
                                Decode.succeed UserWantsToNavigateDown

                            else
                                Decode.fail "nothing to do"
                        )


nodeInputSelectionChange : Attribute Msg
nodeInputSelectionChange =
    Selection.decoder
        |> Decode.field "detail"
        |> Decode.map UserChangedSelection
        |> Events.on "note-input-selectionchange"


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
