module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import Css
import Database exposing (Database)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs exposing (css)
import Html.Styled.Events as Events
import Node exposing (Node)
import Random
import Sort
import Task
import Time exposing (Posix)
import UUID exposing (UUID)
import Url exposing (Url)


type alias Model =
    { database : Database
    , url : Url
    , key : Navigation.Key
    , seed : Random.Seed

    -- view state
    , selected : Maybe Database.ID
    , editing : Maybe Database.ID
    }


type Msg
    = ClickedLink Browser.UrlRequest
    | UrlChanged Url
    | ClickedNewNote
    | NewNote UUID Posix
    | NewRandomSeed Random.Seed
    | ClickedToSelectNote Database.ID
    | ClickedToEditNode Database.ID
    | StoppedEditingNode
      -- TODO: better names for this distinction. EditedNodeContents is for editing, EditNodeContents is for getting the timestamp
    | EditedNodeContents String
    | UpdatedNote Database.ID Posix


init : () -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { database = Database.empty
      , url = url
      , key = key
      , seed = Random.initialSeed 0 -- TODO: current time?

      -- view state
      , selected = Nothing
      , editing = Nothing
      }
    , Random.generate NewRandomSeed Random.independentSeed
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink (Browser.Internal url) ->
            ( model
            , Navigation.pushUrl model.key (Url.toString url)
            )

        ClickedLink (Browser.External url) ->
            ( model
            , Navigation.load url
            )

        NewRandomSeed seed ->
            ( { model | seed = seed }
            , Cmd.none
            )

        UrlChanged url ->
            ( model, Cmd.none )

        ClickedNewNote ->
            let
                ( uuid, newSeed ) =
                    Random.step UUID.generator model.seed
            in
            ( { model | seed = newSeed }
            , Task.perform (NewNote uuid) Time.now
            )

        NewNote uuid time ->
            ( { model
                | database =
                    Database.insert
                        { id = Database.ID uuid
                        , metadata = Just Database.Note
                        , content = ""
                        , children = []
                        , updated = time
                        }
                        model.database
                , selected = Just (Database.ID uuid)
                , editing = Just (Database.ID uuid)
              }
            , Cmd.none
            )

        ClickedToSelectNote id ->
            -- TODO: validate this is a valid ID
            ( { model | selected = Just id }
            , Cmd.none
            )

        ClickedToEditNode id ->
            -- TODO: validate this is a valid ID
            ( { model | editing = Just id }
            , Cmd.none
            )

        StoppedEditingNode ->
            ( { model | editing = Nothing }
            , Cmd.none
            )

        EditedNodeContents newContent ->
            case model.editing of
                Just id ->
                    ( { model | database = Database.update id (\node -> { node | content = newContent }) model.database }
                    , Task.perform (UpdatedNote id) Time.now
                    )

                Nothing ->
                    ( model
                    , Cmd.none
                    )

        UpdatedNote id newTimestamp ->
            ( { model | database = Database.update id (\node -> { node | updated = newTimestamp }) model.database }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = "Notes"
    , body =
        [ Html.toUnstyled <|
            Html.main_
                [ css
                    [ Css.property "display" "grid"
                    , Css.property "grid-template-areas" "\"header content\" \"notes content\" \"debug debug\""
                    , Css.property "grid-template-rows" "100px 1fr 100px"
                    , Css.property "grid-template-columns" "300px 1fr"
                    , Css.height (Css.vh 100)
                    ]
                ]
                [ Html.p
                    [ css [ Css.property "grid-area" "debug" ] ]
                    [ Html.text (Debug.toString model) ]
                , Html.section
                    [ css
                        [ Css.property "grid-area" "notes"
                        , Css.backgroundColor (Css.hex "EEEEEE")
                        ]
                    ]
                    [ Html.button
                        [ Events.onClick ClickedNewNote ]
                        [ Html.text "New Note" ]
                    , Database.notes model.database
                        |> Sort.list
                            (Sort.increasing
                                |> Sort.by (\{ updated } -> Time.posixToMillis updated)
                                |> Sort.reverse
                            )
                        |> List.map
                            (\node ->
                                Html.li
                                    -- TODO: make this accessible. Button?
                                    [ Events.onClick (ClickedToSelectNote node.id) ]
                                    [ Html.text node.content ]
                            )
                        |> Html.ul []
                    ]
                , Html.section
                    [ css [ Css.property "grid-area" "content" ] ]
                    [ case Maybe.andThen (\id -> Database.get id model.database) model.selected of
                        Just node ->
                            viewTree model.editing node model.database

                        Nothing ->
                            Html.text "Select a note on the left-hand side to get started."
                    ]
                ]
        ]
    }


{-| LATER: this is the function to add lazy calls in!
-}
viewTree : Maybe Database.ID -> Node -> Database -> Html Msg
viewTree editing node database =
    Html.div []
        [ if editing == Just node.id then
            Html.form [ Events.onSubmit StoppedEditingNode ]
                [ Html.input
                    [ Attrs.value node.content
                    , Events.onInput EditedNodeContents
                    ]
                    []
                ]

          else
            -- TODO: how to make this accessible? A link?
            Html.span [ Events.onClick (ClickedToEditNode node.id) ] [ Html.text node.content ]
        ]


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = UrlChanged
        , view = view
        }
