module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import Database exposing (Database)
import Html.Styled as Html exposing (Html)
import Html.Styled.Events as Events
import Node
import Random
import Task
import Time exposing (Posix)
import UUID exposing (UUID)
import Url exposing (Url)


type alias Model =
    { database : Database
    , url : Url
    , key : Navigation.Key
    , seed : Random.Seed
    }


type Msg
    = ClickedLink Browser.UrlRequest
    | UrlChanged Url
    | ClickedNewNote
    | NewNote UUID Posix
    | NewRandomSeed Random.Seed


init : () -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { database = Database.init
      , url = url
      , key = key
      , seed = Random.initialSeed 0 -- TODO: current time?
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
                        { id = Node.ID uuid
                        , metadata = Just Node.Note
                        , content = ""
                        , children = []
                        , updated = time
                        }
                        model.database
              }
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
            Html.main_ []
                [ Html.p [] [ Html.text (Debug.toString model) ]
                , Html.button
                    [ Events.onClick ClickedNewNote ]
                    [ Html.text "New Note" ]
                , Database.notes model.database
                    |> List.map (\node -> Html.li [] [ Html.text (Debug.toString node) ])
                    |> Html.ul []
                ]
        ]
    }


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
