port module Main exposing (..)

import Accessibility.Styled as Html exposing (Attribute, Html)
import Accessibility.Styled.Style exposing (invisible)
import Browser
import Browser.Dom as Dom
import Browser.Navigation as Navigation
import Css
import Css.Global
import Css.Reset
import Database exposing (Database)
import Database.ID as ID exposing (ID)
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
import Widgets.Button as Button
import Widgets.Colors as Colors
import Widgets.Icons as Icons
import Widgets.Text as Text


type alias Model key =
    { database : Database
    , url : Url
    , key : key
    , route : Route

    -- view state
    , editing : Maybe Editing
    , selection : Maybe Selection
    }


type alias Editing =
    { id : ID
    , input : Result ( String, List String ) Content
    , selection : { start : Int, end : Int }
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
    | UserHitBackspaceAtBeginningOfNode
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
                                |> Database.update row.id (Node.setContent leftContent)
                                |> Database.insert (Node.node rightContent)

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
                                |> Database.update target.id (Node.setContent updatedContent)
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
            , Css.property "grid-template-rows" "auto 1fr"
            , Css.property "grid-template-areas" "\"header .\" \"list note\" "
            , Css.property "grid-column-gap" "10px"
            , Css.height (Css.vh 100)
            , Css.width (Css.pct 100)
            ]
        ]
        [ viewHeader [ css [ Css.property "grid-area" "header" ] ]
        , viewNav [ css [ Css.property "grid-area" "list" ] ]
            (case model.route of
                Route.Node id ->
                    Just id

                _ ->
                    Nothing
            )
            (Database.filter Node.isNote model.database)
        , Html.div [ css [ Css.property "grid-area" "note" ] ]
            [ case model.route of
                Route.NotFound ->
                    Html.text "Not found!"

                Route.Root ->
                    Html.text "Select or create a note!"

                Route.Node id ->
                    viewRow id model
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
        [ Button.button UserClickedNewNote
            [ Button.transparent
            , Button.css [ Css.padding (Css.px 10) ]
            ]
            [ Icons.chick
                { height = 30
                , shell = Colors.greyDark
                , chick = Colors.yellowDark
                }
            , Html.span invisible [ Html.text "New Note" ]
            ]
        ]


viewNav : List (Attribute Never) -> Maybe ID -> List Database.Row -> Html Msg
viewNav attrs activeId rows =
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
            |> List.map (\row -> Html.li [] [ viewNavLink activeId row ])
            |> Html.ul []
        ]


viewNavLink : Maybe ID -> Database.Row -> Html Msg
viewNavLink activeId { id, node } =
    Content.toHtml (UserSelectedNoteInList id)
        [ Attrs.css
            [ Css.padding (Css.px 10)
            , Css.width (Css.pct 100)

            -- it's always text!
            , Text.text
            , Css.Global.descendants
                [ Css.Global.everything
                    [ Css.textOverflow Css.ellipsis
                    , Css.overflow Css.hidden
                    , Css.whiteSpace Css.noWrap
                    ]
                ]

            -- left align, but center vertically
            , Css.displayFlex
            , Css.flexDirection Css.row
            , Css.justifyContent Css.left
            , Css.alignItems Css.center
            , Css.overflow Css.hidden

            -- surround by borders
            , Css.borderBottom3 (Css.px 1) Css.solid (Colors.toCss Colors.greyLight)
            , Css.borderLeft3 (Css.px 5) Css.solid Css.transparent
            , Css.property "transition" "all 0.25s"

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
        (Node.content node)


viewRow : ID -> Model key -> Html Msg
viewRow id model =
    case Database.get id model.database of
        Nothing ->
            Html.text "Node not found!"

        Just row ->
            let
                tag =
                    if Node.isNote row.node then
                        Html.section
                            [ Attrs.css
                                [ Css.maxWidth (Css.em 40)
                                , Css.margin2 Css.zero Css.auto
                                ]
                            ]

                    else
                        Html.li
                            [ Attrs.css [ Css.pseudoElement "marker" [ Css.color (Colors.toCss Colors.greenDark) ] ] ]
            in
            tag
                [ case model.editing of
                    Just editing ->
                        if id == editing.id then
                            Html.form []
                                [ Html.textarea
                                    [ Attrs.css
                                        [ if Node.isNote row.node then
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
                        |> Html.ul
                            [ Attrs.css
                                [ Css.paddingLeft (Css.px 30)
                                , Css.listStylePosition Css.outside
                                , Css.listStyleType Css.disc
                                ]
                            ]
                ]


viewNode : ID -> Node -> Html Msg
viewNode id node =
    Content.toHtml (UserWantsToEditNode id)
        [ Attrs.css
            [ if Node.isNote node then
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
