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
    | ClickedNewNote
    | ClickedNewNoteAt Posix
    | UserWantsToEditNode String
    | UserEditedNode String
    | UserFinishedEditingNode
    | UserFinishedEditingNodeAt String String Posix
    | Focused (Result Dom.Error ())



-- | UserSelectedNode Database.ID
-- | UserChangedSelection Selection
-- | UserHitEnterOnNode Database.ID
-- | UserHitTabToIndent Database.ID
-- | UserHitShiftTabToDedent Database.ID
-- | UserWantsToDeleteNode Database.ID
-- | UserWantsToMoveNodeUp Database.ID
-- | UserWantsToMoveNodeDown Database.ID
-- | UserWantsToNavigateUp
-- | UserWantsToNavigateDown


type Effect
    = NoEffect
    | Batch (List Effect)
    | LoadUrl String
    | PushUrl Route
    | FocusOnContent
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

        ClickedNewNote ->
            ( model
            , GetTimeAnd ClickedNewNoteAt
            )

        ClickedNewNoteAt currentTime ->
            case Log.insert currentTime "" model.database of
                Ok ( id, database, entryToSave ) ->
                    ( { model
                        | database = database
                        , editing =
                            Just
                                { id = id
                                , input = ""
                                , errors = []
                                }
                      }
                    , Batch
                        [ PushUrl (Route.Node id)
                        , FocusOnContent

                        -- TODO: send entryToSave out the saving port
                        ]
                    )

                Err err ->
                    Debug.todo (Debug.toString err)

        UserWantsToEditNode id ->
            case Log.get id model.database of
                Just { content } ->
                    ( { model
                        | editing =
                            Just
                                { id = id
                                , input = content |> Maybe.map LWW.value |> Maybe.withDefault ""
                                , errors = []
                                }
                      }
                    , FocusOnContent
                    )

                Nothing ->
                    ( model, NoEffect )

        UserEditedNode content ->
            case model.editing of
                Just editing ->
                    ( { model
                        | editing =
                            Just
                                { editing
                                    | input = content
                                    , errors =
                                        case Content.fromString content of
                                            Ok _ ->
                                                []

                                            Err problems ->
                                                problems
                                }
                      }
                    , NoEffect
                    )

                Nothing ->
                    ( model, NoEffect )

        UserFinishedEditingNode ->
            case model.editing of
                Just { id, input, errors } ->
                    if not (List.isEmpty errors) then
                        Debug.todo "nonempty errors! Revert? Discard?"

                    else
                        ( { model | editing = Nothing, selection = Nothing }
                        , GetTimeAnd (UserFinishedEditingNodeAt id input)
                        )

                Nothing ->
                    ( model
                    , NoEffect
                    )

        UserFinishedEditingNodeAt id input time ->
            case Log.edit time id input model.database of
                Ok ( database, toInsert ) ->
                    ( { model | database = database }
                    , -- TODO: persist entryToSave
                      NoEffect
                    )

                Err err ->
                    Debug.todo (Debug.toString err)

        Focused _ ->
            -- TODO: report errors that happen?
            ( model, NoEffect )



-- UserSelectedNode id ->
--     ( model
--     , PushUrl (Route.Node id)
--     )
-- UserChangedSelection selection ->
--     ( { model | selection = Just selection }
--     , NoEffect
--     )
-- UserHitEnterOnNode id ->
--     let
--         ( newId, database ) =
--             Database.insert (Node.node Content.empty) model.database
--     in
--     case Database.get id model.database |> Maybe.map (Node.isNote << .node) of
--         Just True ->
--             ( { model
--                 | database = Database.moveInto id newId database
--                 , editing = Just newId
--               }
--             , FocusOnContent
--             )
--         Just False ->
--             ( { model
--                 | database = Database.moveAfter id newId database
--                 , editing = Just newId
--               }
--             , FocusOnContent
--             )
--         Nothing ->
--             ( model, NoEffect )
-- UserHitTabToIndent id ->
--     case
--         model.database
--             |> Database.previousSibling id
--             |> Maybe.andThen (\siblingId -> Database.get siblingId model.database)
--     of
--         Just previousSibling ->
--             ( { model
--                 | database =
--                     -- TODO: this looks like some logic that
--                     -- should live inside Database.elm. It's making
--                     -- a decision where to move something based on
--                     -- the field values when the intent could be
--                     -- clearer as "moveToLastChild" or similar.
--                     case previousSibling.children |> List.reverse |> List.head of
--                         Nothing ->
--                             Database.moveInto previousSibling.id id model.database
--                         Just lastChild ->
--                             Database.moveAfter lastChild id model.database
--               }
--             , FocusOnContent
--             )
--         Nothing ->
--             ( model, NoEffect )
-- UserHitShiftTabToDedent id ->
--     case Database.get id model.database |> Maybe.andThen .parent of
--         Just parent ->
--             ( { model | database = Database.moveAfter parent id model.database }
--             , FocusOnContent
--             )
--         Nothing ->
--             ( model, NoEffect )
-- UserWantsToDeleteNode id ->
--     -- TODO: if we were focused on editing this comment, change focus
--     -- to the previous sibling.
--     ( { model | database = Database.delete id model.database }
--     , NoEffect
--     )
-- UserWantsToMoveNodeUp id ->
--     case
--         Maybe.Extra.orListLazy
--             [ \_ -> Database.previousSibling id model.database
--             , \_ -> Database.get id model.database |> Maybe.andThen .parent
--             ]
--     of
--         Just target ->
--             ( { model | database = Database.moveBefore target id model.database }
--             , FocusOnContent
--             )
--         Nothing ->
--             ( model
--             , NoEffect
--             )
-- UserWantsToNavigateUp ->
--     case
--         Maybe.Extra.orListLazy
--             [ \_ -> model.editing |> Maybe.andThen (\id -> Database.previousSibling id model.database)
--             , \_ -> model.editing |> Maybe.andThen (\id -> Database.get id model.database) |> Maybe.andThen .parent
--             ]
--     of
--         Just target ->
--             ( { model | editing = Just target }
--             , FocusOnContent
--             )
--         Nothing ->
--             ( model
--             , NoEffect
--             )
-- UserWantsToMoveNodeDown id ->
--     case Database.nextNode id model.database of
--         Just target ->
--             ( { model | database = Database.moveAfter target id model.database }
--             , FocusOnContent
--             )
--         Nothing ->
--             ( model
--             , NoEffect
--             )
-- UserWantsToNavigateDown ->
--     case
--         Maybe.Extra.orListLazy
--             [ \_ -> model.editing |> Maybe.andThen (\id -> Database.nextSibling id model.database)
--             , \_ ->
--                 model.editing
--                     |> Maybe.andThen (\id -> Database.get id model.database)
--                     |> Maybe.andThen .parent
--                     |> Maybe.andThen (\id -> Database.nextSibling id model.database)
--             ]
--     of
--         Just target ->
--             ( { model | editing = Just target }
--             , FocusOnContent
--             )
--         Nothing ->
--             ( model
--             , NoEffect
--             )


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
        [ Html.button [ Events.onClick ClickedNewNote ] [ Html.text "New Note" ]
        , model.database.state
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
