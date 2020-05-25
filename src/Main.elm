port module Main exposing (Effect(..), Model, Msg(..), init, main, update, view)

import Accessibility.Styled as Html exposing (Attribute, Html)
import Accessibility.Styled.Style exposing (invisible)
import Browser
import Browser.Dom as Dom
import Browser.Navigation as Navigation
import Content exposing (Content)
import Css
import Css.Global
import Css.Reset
import Database exposing (Database)
import Database.ID as ID exposing (ID)
import Database.Sync as Sync exposing (Sync)
import Html.Styled as Inaccessible
import Html.Styled.Attributes as Attrs exposing (css)
import Html.Styled.Events as Events
import Html.Styled.Events.Extra exposing (onClickPreventDefaultForLinkWithHref)
import Json.Decode as Decode exposing (Decoder, Value)
import Maybe.Extra
import Node exposing (Node)
import Random
import Route exposing (Route)
import Selection exposing (Selection)
import Settings exposing (Settings)
import Task
import Time exposing (Posix)
import Url exposing (Url)
import Widgets.Button as Button
import Widgets.Colors as Colors
import Widgets.Icons as Icons
import Widgets.Text as Text
import Widgets.TimeDifference as TimeDifference


type alias Model key =
    { database : Database
    , url : Url
    , key : key
    , route : Route
    , currentTime : Posix
    , settings : Settings

    -- view state
    , editing : Maybe Editing
    , selection : Maybe Selection
    , draftSync : Maybe Sync
    }


type alias Editing =
    { id : ID
    , input : Result ( String, List String ) Content
    , selection : { start : Int, end : Int }
    }


init : Value -> Url -> key -> ( Model key, Effect )
init flags url key =
    let
        { seed, documents, now, settings } =
            case Decode.decodeValue flagsDecoder flags of
                Ok stuff ->
                    stuff

                Err err ->
                    Debug.todo (Debug.toString err)

        ( model, routingEffects ) =
            update
                (UrlChanged url)
                { database = Database.load seed documents
                , url = url
                , key = key
                , route = Route.Root
                , currentTime = now
                , settings = settings

                -- view state
                , editing = Nothing
                , selection = Nothing
                , draftSync = Nothing
                }
    in
    ( model
    , Batch
        [ routingEffects
        , Batch (List.map SyncOnce settings.syncs)
        ]
    )


flagsDecoder :
    Decoder
        { now : Posix
        , seed : Random.Seed
        , documents : List Database.Document
        , settings : Settings
        }
flagsDecoder =
    Decode.map3
        (\millis documents settings ->
            { now = Time.millisToPosix millis
            , seed = Random.initialSeed millis
            , documents = documents
            , settings = settings
            }
        )
        (Decode.field "now" Decode.int)
        (Decode.field "documents" (Decode.list (Decode.field "doc" Database.decoder)))
        (Decode.field "settings"
            (Decode.nullable Settings.decoder
                |> Decode.map (Maybe.withDefault Settings.init)
            )
        )


type Msg
    = ClickedLink Browser.UrlRequest
    | UrlChanged Url
    | GotCurrentTime Posix
    | PouchDBPutSuccessfully Value
    | TimerTriggeredSave
    | TimerTriggeredSyncAll
    | TimerTriggeredCompaction
    | FocusedOnEditor
    | UserClickedNewNote
    | UserEditedNode String
    | UserFinishedEditing
    | UserHitEnterOnNode
    | UserSelectedNoteInList ID
    | UserWantsToEditNode ID
    | UserWantsToIndentNode
    | UserWantsToDedentNode
    | UserHitBackspaceAtBeginningOfNode
    | UserWantsToMoveNodeUp
    | UserWantsToMoveNodeDown
    | UserChangedSelection { start : Int, end : Int }
    | UserWantsToCreateNewSync
      -- workarounds for elm-program-test issues
    | UserWantsToOpenNoteWithTitle Content
    | UserWantsToNavigate Route
      -- draft sync form
    | UserTypedInDraftSyncDatabaseField String
    | UserTypedInDraftSyncHostField String
    | UserTypedInDraftSyncPasswordField String
    | UserTypedInDraftSyncUsernameField String
    | UserSubmittedSyncForm
    | UserWantsToDeleteSync Sync


type Effect
    = NoEffect
    | Batch (List Effect)
    | LoadUrl String
    | PushUrl Route
    | ReplaceUrl Route
    | Put Value
    | FocusOnEditor
    | SyncOnce Sync
    | Compact


type UpdatedRevision
    = ForSettings
    | ForRow ID


update : Msg -> Model key -> ( Model key, Effect )
update msg model =
    case msg of
        ClickedLink (Browser.Internal url) ->
            ( model, PushUrl (Route.parse url) )

        ClickedLink (Browser.External url) ->
            ( model, LoadUrl url )

        UrlChanged url ->
            case Route.parse url of
                Route.NodeByTitle content ->
                    let
                        ( id, database ) =
                            findOrCreateNoteWithTitle model.currentTime content model.database
                    in
                    ( { model | database = database }
                    , ReplaceUrl (Route.NodeById id)
                    )

                route ->
                    ( { model | route = route }
                    , NoEffect
                    )

        UserWantsToNavigate route ->
            ( model
            , PushUrl route
            )

        GotCurrentTime now ->
            ( { model | currentTime = now }
            , NoEffect
            )

        PouchDBPutSuccessfully value ->
            let
                decoder =
                    Decode.map2 Tuple.pair
                        (Decode.field "id"
                            (Decode.oneOf
                                [ Decode.map ForRow ID.decoder
                                , Decode.map (\_ -> ForSettings) Settings.idDecoder
                                ]
                            )
                        )
                        (Decode.field "rev" Decode.string)
            in
            case Decode.decodeValue decoder value of
                Ok ( ForRow id, rev ) ->
                    ( { model | database = Database.updateRevision id rev model.database }
                    , NoEffect
                    )

                Ok ( ForSettings, rev ) ->
                    ( { model | settings = Settings.updateRevision rev model.settings }
                    , NoEffect
                    )

                Err err ->
                    Debug.todo (Debug.toString err)

        TimerTriggeredSave ->
            let
                ( rows, database ) =
                    Database.toPersist model.database
            in
            ( { model | database = database }
            , Batch (List.map (Put << Database.encode) rows)
            )

        TimerTriggeredSyncAll ->
            ( model
            , Batch (List.map SyncOnce model.settings.syncs)
            )

        TimerTriggeredCompaction ->
            ( model, Compact )

        FocusedOnEditor ->
            ( model, NoEffect )

        UserClickedNewNote ->
            let
                ( row, database ) =
                    Database.insert model.currentTime (Node.title Content.empty) model.database
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
                [ PushUrl (Route.NodeById row.id)
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
                                , database = Database.update model.currentTime editing.id (Node.setContent content) model.database
                              }
                            , NoEffect
                            )

                        Err problems ->
                            ( { model | editing = Just { editing | input = Err ( input, problems ) } }
                            , NoEffect
                            )

        UserFinishedEditing ->
            case Maybe.map .input model.editing of
                Just (Ok _) ->
                    ( { model | editing = Nothing }
                    , NoEffect
                    )

                _ ->
                    ( model
                    , NoEffect
                    )

        UserHitEnterOnNode ->
            case
                Maybe.map2 Tuple.pair
                    model.editing
                    (Maybe.andThen (\editing -> Database.get editing.id model.database) model.editing)
            of
                Just ( editing, row ) ->
                    let
                        ( leftContent, rightContent ) =
                            -- demeter chain?
                            Content.splitAt editing.selection.start (Node.content row.node)

                        ( newNode, inserted ) =
                            model.database
                                |> Database.update model.currentTime row.id (Node.setContent leftContent)
                                |> Database.insert model.currentTime (Node.node rightContent)

                        database =
                            if Node.isTitle row.node then
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
            , PushUrl (Route.NodeById id)
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
                                if Node.isTitle parent.node then
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

        UserHitBackspaceAtBeginningOfNode ->
            let
                maybeRow =
                    Maybe.andThen (\{ id } -> Database.get id model.database) model.editing

                maybeTarget =
                    Maybe.Extra.orListLazy
                        [ \_ -> Maybe.andThen (\{ id } -> Database.previousSibling id model.database) maybeRow
                        , \_ -> Maybe.andThen .parent maybeRow
                        ]
                        |> Maybe.andThen (\id -> Database.get id model.database)
            in
            case Maybe.map2 Tuple.pair maybeRow maybeTarget of
                Just ( row, target ) ->
                    let
                        updatedContent =
                            Content.append (Node.content target.node) (Node.content row.node)
                    in
                    ( { model
                        | database =
                            model.database
                                |> Database.update model.currentTime target.id (Node.setContent updatedContent)
                                |> Database.delete row.id
                        , editing =
                            Just
                                { id = target.id
                                , input = Ok updatedContent
                                , selection = { start = 0, end = 0 }
                                }
                      }
                    , FocusOnEditor
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

        UserWantsToOpenNoteWithTitle content ->
            let
                ( id, database ) =
                    findOrCreateNoteWithTitle model.currentTime content model.database
            in
            ( { model | database = database }
            , PushUrl (Route.NodeById id)
            )

        UserWantsToCreateNewSync ->
            ( { model | draftSync = Just (Sync "" "" "" "") }
            , NoEffect
            )

        UserTypedInDraftSyncDatabaseField database ->
            ( { model | draftSync = Maybe.map (\draftSync -> { draftSync | database = database }) model.draftSync }
            , NoEffect
            )

        UserTypedInDraftSyncHostField host ->
            ( { model | draftSync = Maybe.map (\draftSync -> { draftSync | host = host }) model.draftSync }
            , NoEffect
            )

        UserTypedInDraftSyncPasswordField password ->
            ( { model | draftSync = Maybe.map (\draftSync -> { draftSync | password = password }) model.draftSync }
            , NoEffect
            )

        UserTypedInDraftSyncUsernameField username ->
            ( { model | draftSync = Maybe.map (\draftSync -> { draftSync | username = username }) model.draftSync }
            , NoEffect
            )

        UserSubmittedSyncForm ->
            case model.draftSync of
                Just draftSync ->
                    if Sync.isValid draftSync then
                        let
                            newSettings =
                                Settings.insertSync draftSync model.settings
                        in
                        ( { model | draftSync = Nothing, settings = newSettings }
                        , Batch
                            [ Put (Settings.encode newSettings)
                            , SyncOnce draftSync
                            ]
                        )

                    else
                        ( model, NoEffect )

                Nothing ->
                    ( model, NoEffect )

        UserWantsToDeleteSync sync ->
            let
                newSettings =
                    Settings.removeSync sync model.settings
            in
            ( { model | settings = newSettings }
            , Put (Settings.encode newSettings)
            )


{-| TODO: should this go in Database?
-}
findOrCreateNoteWithTitle : Posix -> Content -> Database -> ( ID, Database )
findOrCreateNoteWithTitle now content database =
    case Database.filter (\node -> Node.content node == content) database of
        title :: _ ->
            ( title.id
            , database
            )

        [] ->
            Database.insert now (Node.title content) database
                |> Tuple.mapFirst .id


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

        SyncOnce sync ->
            case Sync.toUrl sync of
                Just url ->
                    syncOnce url

                Nothing ->
                    Cmd.none

        Compact ->
            compact ()


port put : Value -> Cmd msg


port syncOnce : String -> Cmd msg


port compact : () -> Cmd msg


port putSuccessfully : (Value -> msg) -> Sub msg


subscriptions : Model key -> Sub Msg
subscriptions model =
    Sub.batch
        [ putSuccessfully PouchDBPutSuccessfully
        , Time.every 1000 (\_ -> TimerTriggeredSave)
        , Time.every 60000 (\_ -> TimerTriggeredSyncAll)
        , Time.every 120000 (\_ -> TimerTriggeredCompaction)

        -- Using a 1-second resolution is more than enough for most UI
        -- concerns. If it needs to be more precise, crank this down! But
        -- remember aware that it'll be a lot of updates and not precise ticks.
        , Time.every 1000 GotCurrentTime
        ]


view : Model key -> Browser.Document Msg
view model =
    { title = "Notes"
    , body =
        List.map Html.toUnstyled
            [ Css.Reset.meyerV2
            , Css.Reset.borderBoxV201408
            , Css.Global.global
                [ Css.Global.body
                    [ Css.backgroundColor (Colors.toCss Colors.whiteLightest)
                    , Css.color (Colors.toCss Colors.blackDark)
                    ]
                ]
            , viewApplication model
            ]
    }


viewApplication : Model key -> Html Msg
viewApplication model =
    Html.main_
        [ css
            [ Css.property "display" "grid"
            , Css.property "grid-template-columns" "350px 1fr"
            , Css.property "grid-template-rows" "auto 1fr auto"
            , Css.property "grid-template-areas" "\"header .\" \"list note\" \"sync note\""
            , Css.property "grid-column-gap" "10px"
            , Css.height (Css.vh 100)
            , Css.width (Css.pct 100)
            ]
        ]
        [ viewHeader [ css [ Css.property "grid-area" "header" ] ]
        , viewNav [ css [ Css.property "grid-area" "list" ] ]
            (case model.route of
                Route.NodeById id ->
                    Just id

                _ ->
                    Nothing
            )
            model.currentTime
            (Database.filter Node.isTitle model.database)
        , viewSync [ css [ Css.property "grid-area" "sync" ] ]
        , Html.div [ css [ Css.property "grid-area" "note" ] ]
            [ case model.route of
                Route.NotFound ->
                    Html.text "Not found!"

                Route.Root ->
                    Html.p
                        [ Attrs.css [ Text.text ] ]
                        [ Html.text "Select or create a note!" ]

                Route.NodeById id ->
                    viewRow id model

                Route.NodeByTitle _ ->
                    Html.text "You shouldn't ever see this page!"

                Route.SyncSettings ->
                    viewSyncSettingsPage model
            ]
        ]


viewHeader : List (Attribute Never) -> Html Msg
viewHeader attrs =
    Html.header
        (Attrs.css
            [ Css.borderBottom3 (Css.px 1) Css.solid (Colors.toCss Colors.greyLight)
            , Css.borderRight3 (Css.px 1) Css.solid (Colors.toCss Colors.greyLight)
            ]
            :: attrs
        )
        [ Button.button (Button.OnClick UserClickedNewNote)
            [ Button.transparent
            , Button.css
                [ Css.padding (Css.px 12)
                , Css.cursor Css.pointer
                ]
            ]
            [ Icons.chick
                { height = 30
                , shell = Colors.greyDark
                , chick = Colors.yellowDark
                }
            , Html.span invisible [ Html.text "New Note" ]
            ]
        ]


viewNav : List (Attribute Never) -> Maybe ID -> Posix -> List Database.Row -> Html Msg
viewNav attrs activeId now rows =
    Html.nav
        (Attrs.css
            [ Css.height (Css.pct 100)
            , Css.borderRight3 (Css.px 1) Css.solid (Colors.toCss Colors.greyLight)
            , Css.overflowY Css.scroll
            , Css.overflowX Css.hidden
            ]
            :: attrs
        )
        [ rows
            |> List.sortBy (\row -> -(Time.posixToMillis row.updated))
            |> List.map (\row -> Html.li [] [ viewNavLink activeId now row ])
            |> Html.ul []
        ]


viewSync : List (Attribute Never) -> Html Msg
viewSync attrs =
    Html.div attrs
        [ Inaccessible.a
            [ Attrs.href (Route.toString Route.SyncSettings)
            , onClickPreventDefaultForLinkWithHref (UserWantsToNavigate Route.SyncSettings)
            ]
            [ Html.text "Sync" ]
        ]


viewSyncSettingsPage : Model key -> Html Msg
viewSyncSettingsPage model =
    -- TODO: pull this out to a widget and remove the duplication
    Html.section
        [ Attrs.css
            [ Css.maxWidth (Css.em 50)
            , Css.padding2 Css.zero (Css.px 27)
            , Css.margin2 Css.zero Css.auto
            ]
        ]
        [ Html.h1 [ Attrs.css [ Text.h1 ] ] [ Html.text "Sync Settings" ]
        , case model.draftSync of
            Just draftSync ->
                -- TODO: extract this all to a widget
                let
                    field onInput label value =
                        Html.labelBefore
                            [ Attrs.css
                                [ Text.text
                                , Css.display Css.tableRow
                                ]
                            ]
                            (Html.span [ Attrs.css [ Css.display Css.tableCell ] ] [ Html.text label ])
                            (Html.inputText label
                                [ Attrs.value value
                                , Events.onInput onInput
                                , Attrs.css [ Css.display Css.tableCell ]
                                ]
                            )
                in
                -- using onSubmit as a hook here since hitting enter needs to work
                Inaccessible.form
                    [ Events.onSubmit UserSubmittedSyncForm
                    , Attrs.css
                        [ Css.display Css.table
                        , Css.margin2 (Css.px 27) Css.zero
                        ]
                    ]
                    [ field UserTypedInDraftSyncHostField "Host" draftSync.host
                    , field UserTypedInDraftSyncDatabaseField "Database" draftSync.database
                    , field UserTypedInDraftSyncUsernameField "Username" draftSync.username
                    , field UserTypedInDraftSyncPasswordField "Password" draftSync.password
                    , Button.button
                        Button.Submit
                        [ Button.enabled (Sync.isValid draftSync) ]
                        [ Html.text "Start Syncing" ]
                    ]

            Nothing ->
                Button.button (Button.OnClick UserWantsToCreateNewSync)
                    [ Button.css [ Css.margin2 (Css.px 27) Css.zero ] ]
                    [ Html.text "Sync with a new CouchDB Server" ]
        , let
            th =
                Attrs.css
                    [ Text.text
                    , Css.fontWeight Css.bold
                    , Css.textAlign Css.left
                    ]

            cell =
                Attrs.css
                    [ Text.text
                    , Css.padding (Css.px 5)
                    ]
          in
          Html.table
            [ Attrs.css [ Css.width (Css.pct 100) ] ]
            [ Html.thead []
                [ Html.tr []
                    [ Html.th [ th ] [ Html.text "Host" ]
                    , Html.th [ th ] [ Html.text "Database" ]
                    , Html.th [ th ] [ Html.text "Username" ]
                    , Html.th [ th ] [ Html.text "Password" ]
                    , Html.th [] []
                    ]
                ]
            , Html.tbody []
                (List.map
                    (\sync ->
                        Html.tr []
                            [ Html.td [ cell ] [ Html.text sync.host ]
                            , Html.td [ cell ] [ Html.text sync.database ]
                            , Html.td [ cell ] [ Html.text sync.username ]
                            , Html.td [ cell ]
                                [ if sync.password /= "" then
                                    Html.text "*****"

                                  else
                                    Html.text ""
                                ]
                            , Html.td []
                                [ Button.button
                                    (Button.OnClick (UserWantsToDeleteSync sync))
                                    [ Button.delete ]
                                    [ Html.text "Delete" ]
                                ]
                            ]
                    )
                    model.settings.syncs
                )
            ]
        ]


viewNavLink : Maybe ID -> Posix -> Database.Row -> Html Msg
viewNavLink activeId now { id, node, updated } =
    Button.button
        (Button.OnClick (UserSelectedNoteInList id))
        [ Button.transparent
        , Button.css [ Css.width (Css.pct 100) ]
        ]
        [ -- Safari doesn't let you make a button a grid container. Boo.
          Html.div
            [ Attrs.css
                [ Css.padding (Css.px 12)
                , Css.width (Css.pct 100)

                -- it's always text!
                , Text.text

                -- content layout
                , Css.property "display" "grid"
                , Css.property "grid-template-columns" "15% 1fr"
                , Css.property "grid-template-rows" "auto"
                , Css.property "grid-template-areas" "\"date title\""
                , Css.property "grid-column-gap" "10px"

                -- surround by borders
                , Css.borderBottom3 (Css.px 1) Css.solid (Colors.toCss Colors.greyLight)
                , Css.borderLeft3 (Css.px 8) Css.solid Css.transparent
                , Css.property "transition" "all 0.25s"

                -- you can click on this!
                , Css.cursor Css.pointer

                -- highlight the active node
                , -- TODO: make an isActive helper that checks if a child is active too
                  if activeId == Just id then
                    Css.batch
                        [ Css.borderLeftColor (Colors.toCss Colors.greenLight)
                        , Css.backgroundColor (Colors.toCss Colors.whiteLight)
                        ]

                  else
                    Css.batch []
                ]
            ]
            [ Html.div
                [ css [ Css.property "grid-area" "date" ] ]
                [ TimeDifference.compact updated now ]
            , Content.toHtml
                { activate = Nothing
                , navigate = UserWantsToOpenNoteWithTitle
                , navigateUrl = Route.toString << Route.NodeByTitle
                }
                [ css
                    [ Css.property "grid-area" "title"
                    , Css.textAlign Css.left
                    , Css.height (Css.calc Text.textLineHeight Css.plus Text.textLineHeight)
                    , Css.property "display" "-webkit-box"
                    , Css.property "-webkit-line-clamp" "2"
                    , Css.property "-webkit-box-orient" "vertical"
                    , Css.overflow Css.hidden
                    ]
                ]
                (Node.content node)
            ]
        ]


viewRow : ID -> Model key -> Html Msg
viewRow id model =
    case Database.get id model.database of
        Nothing ->
            Html.text "Node not found!"

        Just row ->
            let
                tag =
                    if Node.isTitle row.node then
                        -- TODO: pull this out to a widget and remove the duplication
                        Html.section
                            [ Attrs.css
                                [ Css.maxWidth (Css.em 50)
                                , Css.padding2 Css.zero (Css.px 27)
                                , Css.margin2 Css.zero Css.auto
                                ]
                            ]

                    else
                        -- TODO: extract this list item to a widget or something
                        Html.li
                            [ Attrs.css
                                [ Css.pseudoElement "marker" [ Css.color (Colors.toCss Colors.greenDark) ]
                                , Css.marginBottom (Css.px 5.333)
                                , Css.position Css.relative
                                ]
                            ]
            in
            tag
                [ case model.editing of
                    Just editing ->
                        if id == editing.id then
                            Html.form []
                                [ Html.textarea
                                    [ Attrs.css
                                        [ if Node.isTitle row.node then
                                            Text.h1

                                          else
                                            Text.text
                                        , Css.width (Css.pct 100)
                                        , Css.border Css.zero
                                        , Css.backgroundColor Css.transparent
                                        , Css.resize Css.none
                                        , Css.height Css.zero
                                        ]
                                    , case Maybe.map .input model.editing of
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
                                    , editorKeybindings editing row
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
                            viewNode row.id row.node

                    Nothing ->
                        viewNode row.id row.node
                , if List.isEmpty row.children then
                    Html.text ""

                  else
                    row.children
                        |> List.map (\child -> viewRow child model)
                        -- TODO: extract this list to a widget or something
                        |> Html.ul
                            [ Attrs.css
                                [ Css.paddingLeft (Css.px 27)
                                , Css.listStylePosition Css.outside
                                , Css.listStyleType Css.disc
                                ]
                            ]
                , if Node.isTitle row.node then
                    case Database.backlinksTo row.id model.database of
                        [] ->
                            Html.text ""

                        backlinks ->
                            Html.aside
                                [ Attrs.css
                                    [ Css.padding (Css.px 18)
                                    , Css.marginTop (Css.px 40.5)
                                    , Css.backgroundColor (Colors.toCss Colors.whiteLight)
                                    ]
                                ]
                                [ Html.h2
                                    [ Attrs.css [ Text.h2, Css.color (Colors.toCss Colors.blackLight) ] ]
                                    [ Html.text "Incoming Links" ]

                                -- TODO: extract this list to a widget or something
                                , Html.ul
                                    [ Attrs.css
                                        [ Css.paddingLeft (Css.px 27)
                                        , Css.listStylePosition Css.outside
                                        , Css.listStyleType Css.disc
                                        ]
                                    ]
                                    (List.map
                                        (\linkingRow ->
                                            -- TODO: extract this list item to a widget or something
                                            Html.li
                                                [ Attrs.css [ Css.pseudoElement "marker" [ Css.color (Colors.toCss Colors.greenDark) ] ] ]
                                                [ Content.toHtml
                                                    { activate = Nothing
                                                    , navigate = UserWantsToOpenNoteWithTitle
                                                    , navigateUrl = Route.toString << Route.NodeByTitle
                                                    }
                                                    [ Attrs.css [ Text.text ] ]
                                                    (Node.content linkingRow.node)
                                                ]
                                        )
                                        backlinks
                                    )
                                ]

                  else
                    Html.div
                        [ Attrs.css
                            [ Css.display Css.block
                            , Css.width (Css.px 2)
                            , Css.height (Css.calc (Css.pct 100) Css.minus Text.textLineHeight)
                            , Css.position Css.absolute
                            , Css.left (Css.px -15)
                            , Css.top Text.textLineHeight
                            , Css.backgroundColor (Colors.toCss Colors.whiteDark)
                            ]
                        ]
                        []
                ]


viewNode : ID -> Node -> Html Msg
viewNode id node =
    Content.toHtml
        { activate = Just (UserWantsToEditNode id)
        , navigate = UserWantsToOpenNoteWithTitle
        , navigateUrl = Route.toString << Route.NodeByTitle
        }
        [ Attrs.css
            [ if Node.isTitle node then
                Text.h1

              else
                Text.text
            ]
        ]
        (Node.content node)


editorKeybindings : Editing -> Database.Row -> Attribute Msg
editorKeybindings editing row =
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
                        if editing.selection.start == 0 && editing.selection.end == 0 && List.isEmpty row.children then
                            Decode.succeed
                                { message = UserHitBackspaceAtBeginningOfNode
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
