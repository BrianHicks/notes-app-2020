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
            , selection : { start : Int, end : Int }
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
    | FocusedOnEditor
    | UserClickedNewNote
    | UserEditedNode String
    | UserFinishedEditing
    | UserHitEnterOnNode
    | UserSelectedNoteInList ID
    | UserWantsToEditNode ID
    | UserWantsToIndentNode
    | UserWantsToDedentNode
    | UserHitBackspaceOnEmptyNode
    | UserWantsToMoveNodeUp
    | UserWantsToMoveNodeDown
    | UserChangedSelection { start : Int, end : Int }


type Effect
    = NoEffect
    | Batch (List Effect)
    | LoadUrl String
    | PushUrl Route
    | ReplaceUrl Route
    | Put Value
    | FocusOnEditor


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

        FocusedOnEditor ->
            ( model, NoEffect )

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
                        , selection = { start = 0, end = 0 }
                        }
              }
            , Batch
                [ PushUrl (Route.Node row.id)
                , FocusOnEditor
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

        UserFinishedEditing ->
            -- TODO: this will discard content if it's in the errored
            -- state. Fix?
            ( { model | editing = Nothing }
            , NoEffect
            )

        UserHitEnterOnNode ->
            case Maybe.andThen (\editing -> Database.get editing.id model.database) model.editing of
                Just row ->
                    let
                        ( newNode, inserted ) =
                            Database.insert (Node.node Content.empty) model.database

                        database =
                            if Node.isNote row.node then
                                Database.moveInto row.id newNode.id inserted

                            else
                                Database.moveAfter row.id newNode.id inserted
                    in
                    ( { model
                        | editing =
                            Just
                                { id = newNode.id
                                , input = Ok (Node.content newNode.node)
                                , selection = { start = 0, end = 0 }
                                }
                        , database = database
                      }
                    , FocusOnEditor
                    )

                Nothing ->
                    ( model
                    , NoEffect
                    )

        UserSelectedNoteInList id ->
            ( model
            , PushUrl (Route.Node id)
            )

        UserWantsToEditNode id ->
            case Database.get id model.database of
                Just row ->
                    ( { model
                        | editing =
                            Just
                                { id = id
                                , input = Ok (Node.content row.node)
                                , selection = { start = 0, end = 0 }
                                }
                      }
                    , FocusOnEditor
                    )

                Nothing ->
                    ( model
                    , NoEffect
                    )

        UserWantsToIndentNode ->
            let
                current =
                    Maybe.map .id model.editing

                newParent =
                    current
                        |> Maybe.andThen (\id -> Database.previousSibling id model.database)
                        |> Maybe.andThen (\id -> Database.get id model.database)
            in
            case Maybe.map2 Tuple.pair current newParent of
                Just ( id, parent ) ->
                    ( { model
                        | database =
                            case List.reverse parent.children of
                                lastChild :: _ ->
                                    Database.moveAfter lastChild id model.database

                                [] ->
                                    Database.moveInto parent.id id model.database
                      }
                    , FocusOnEditor
                    )

                Nothing ->
                    ( model
                    , NoEffect
                    )

        UserWantsToDedentNode ->
            let
                current =
                    Maybe.map .id model.editing

                newParent =
                    current
                        |> Maybe.andThen (\id -> Database.get id model.database)
                        |> Maybe.andThen .parent
                        |> Maybe.andThen (\id -> Database.get id model.database)
                        |> Maybe.andThen
                            (\parent ->
                                if Node.isNote parent.node then
                                    Nothing

                                else
                                    Just parent
                            )
            in
            case Maybe.map2 Tuple.pair current newParent of
                Just ( id, parent ) ->
                    ( { model | database = Database.moveAfter parent.id id model.database }
                    , FocusOnEditor
                    )

                Nothing ->
                    ( model, NoEffect )

        UserHitBackspaceOnEmptyNode ->
            let
                maybeRow =
                    model.editing
                        |> Maybe.map .id
                        |> Maybe.andThen (\id -> Database.get id model.database)
            in
            case maybeRow of
                Just row ->
                    ( { model
                        | database = Database.delete row.id model.database
                        , editing = Nothing
                      }
                    , if Node.isNote row.node then
                        ReplaceUrl Route.Root

                      else
                        NoEffect
                    )

                Nothing ->
                    ( model, NoEffect )

        UserWantsToMoveNodeUp ->
            let
                maybeTarget =
                    Maybe.Extra.orListLazy
                        [ \_ ->
                            model.editing
                                |> Maybe.andThen (\{ id } -> Database.previousSibling id model.database)
                        , \_ ->
                            model.editing
                                |> Maybe.andThen (\{ id } -> Database.get id model.database)
                                |> Maybe.andThen .parent
                        ]
            in
            case Maybe.map2 Tuple.pair model.editing maybeTarget of
                Just ( { id }, target ) ->
                    ( { model | database = Database.moveBefore target id model.database }
                    , FocusOnEditor
                    )

                Nothing ->
                    ( model, NoEffect )

        UserWantsToMoveNodeDown ->
            let
                maybeTarget =
                    Maybe.Extra.orListLazy
                        [ \_ ->
                            model.editing
                                |> Maybe.andThen (\{ id } -> Database.nextSibling id model.database)
                        , \_ ->
                            model.editing
                                |> Maybe.andThen (\{ id } -> Database.get id model.database)
                                |> Maybe.andThen .parent
                                |> Maybe.andThen (\id -> Database.nextSibling id model.database)
                        ]
            in
            case Maybe.map2 Tuple.pair model.editing maybeTarget of
                Just ( { id }, target ) ->
                    ( { model | database = Database.moveAfter target id model.database }
                    , FocusOnEditor
                    )

                Nothing ->
                    ( model, NoEffect )

        UserChangedSelection selection ->
            ( { model
                | editing =
                    Maybe.map
                        (\editing -> { editing | selection = selection })
                        model.editing
              }
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

        ReplaceUrl route ->
            if model.route == route then
                Cmd.none

            else
                Navigation.replaceUrl model.key (Route.toString route)

        Put value ->
            put value

        FocusOnEditor ->
            Task.attempt
                (\_ -> FocusedOnEditor)
                (Dom.focus "editor")


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
            |> List.map
                (\{ id, node } ->
                    Html.li []
                        [ Html.button
                            [ Events.onClick (UserSelectedNoteInList id) ]
                            (Content.toHtml (Node.content node))
                        ]
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
                viewNode id model
        ]


viewNode : ID -> Model key -> Html Msg
viewNode id model =
    case Database.get id model.database of
        Nothing ->
            Html.text "Node not found!"

        Just row ->
            let
                tag =
                    if Node.isNote row.node then
                        "section"

                    else
                        "li"
            in
            Html.node tag
                []
                [ if Maybe.map .id model.editing == Just id then
                    Html.form []
                        [ Html.textarea
                            [ case Maybe.map .input model.editing of
                                Just (Ok content) ->
                                    Attrs.value (Content.toString content)

                                Just (Err ( input, _ )) ->
                                    Attrs.value input

                                -- should not actually be possible; oh well.
                                Nothing ->
                                    Attrs.value ""
                            , Attrs.attribute "aria-label" "Content"
                            , Attrs.id "editor"
                            , Attrs.attribute "is" "node-input"
                            , Events.onInput UserEditedNode
                            , Events.onBlur UserFinishedEditing
                            , editorKeybindings row
                            , onSelectionChange UserChangedSelection
                            ]
                            []
                        , case Maybe.map .input model.editing of
                            Just (Err ( _, problems )) ->
                                Html.ul [] (List.map (\problem -> Html.li [] [ Html.text problem ]) problems)

                            _ ->
                                Html.text ""
                        ]

                  else
                    Html.button
                        [ Events.onClick (UserWantsToEditNode row.id) ]
                        (Content.toHtml (Node.content row.node))
                , if List.isEmpty row.children then
                    Html.text ""

                  else
                    row.children
                        |> List.map (\child -> viewNode child model)
                        |> Html.ul []
                ]


editorKeybindings : Database.Row -> Attribute Msg
editorKeybindings row =
    Decode.map3
        (\key shiftKey altKey ->
            { key = key
            , shiftKey = shiftKey
            , altKey = altKey
            }
        )
        (Decode.field "key" Decode.string)
        (Decode.field "shiftKey" Decode.bool)
        (Decode.field "altKey" Decode.bool)
        |> Decode.andThen
            (\{ key, shiftKey, altKey } ->
                case key of
                    "Escape" ->
                        Decode.succeed
                            { message = UserFinishedEditing
                            , stopPropagation = False
                            , preventDefault = False
                            }

                    "Enter" ->
                        Decode.succeed
                            { message = UserHitEnterOnNode
                            , stopPropagation = True
                            , preventDefault = True
                            }

                    "Tab" ->
                        Decode.succeed
                            { message =
                                if shiftKey then
                                    UserWantsToDedentNode

                                else
                                    UserWantsToIndentNode
                            , stopPropagation = True
                            , preventDefault = True
                            }

                    "Backspace" ->
                        -- TODO: remove demeter chain!
                        if Content.isEmpty (Node.content row.node) && List.isEmpty row.children then
                            Decode.succeed
                                { message = UserHitBackspaceOnEmptyNode
                                , stopPropagation = False
                                , preventDefault = False
                                }

                        else
                            Decode.fail "Not removing non-empty node"

                    "ArrowUp" ->
                        if altKey then
                            Decode.succeed
                                { message = UserWantsToMoveNodeUp
                                , stopPropagation = True
                                , preventDefault = True
                                }

                        else
                            Decode.fail "ignoring ArrowUp without modifier"

                    "ArrowDown" ->
                        if altKey then
                            Decode.succeed
                                { message = UserWantsToMoveNodeDown
                                , stopPropagation = True
                                , preventDefault = True
                                }

                        else
                            Decode.fail "Ignoring ArrowUp without modifier"

                    _ ->
                        Decode.fail ("Unhandled key: " ++ key)
            )
        |> Events.custom "keydown"


onSelectionChange : ({ start : Int, end : Int } -> msg) -> Attribute msg
onSelectionChange msg =
    Decode.map2
        (\start end ->
            msg
                { start = start
                , end = end
                }
        )
        (Decode.field "start" Decode.int)
        (Decode.field "end" Decode.int)
        |> Decode.field "detail"
        |> Events.on "note-input-selectionchange"


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
