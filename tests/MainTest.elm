module MainTest exposing (..)

import Database
import Expect
import Main exposing (..)
import ProgramTest exposing (ProgramTest, SimulatedEffect, clickButton, done, ensureBrowserUrl, ensureViewHas, fillIn)
import Route
import SimulatedEffect.Cmd as SCmd
import SimulatedEffect.Navigation as Navigation
import Test exposing (..)
import Test.Html.Selector as Selector


type alias NotesTest =
    ProgramTest (Model ()) Msg Effect


start : NotesTest
start =
    ProgramTest.createApplication
        { init = init
        , onUrlChange = UrlChanged
        , onUrlRequest = ClickedLink
        , update = update
        , view = view
        }
        |> ProgramTest.withBaseUrl "https://localhost/"
        |> ProgramTest.withSimulatedEffects testPerform
        |> ProgramTest.start ()


testPerform : Effect -> SimulatedEffect Msg
testPerform effect =
    case effect of
        NoEffect ->
            SCmd.none

        Batch effects ->
            SCmd.batch (List.map testPerform effects)

        LoadUrl url ->
            SCmd.none

        PushUrl url ->
            Navigation.pushUrl (Route.toString url)


programTest : Test
programTest =
    describe "notes"
        [ test "it should be possible to add a note and see it in the sidebar after adding" <|
            \_ ->
                start
                    |> addNote (Database.idFromInt 0) "What's up?"
                    |> done
        ]


addNote : Database.ID -> String -> NotesTest -> NotesTest
addNote id text =
    clickButton "New Note"
        >> ensureBrowserUrl (Expect.equal ("https://localhost/notes/" ++ Database.idToString id))
        >> fillIn "title" "Title" text
        >> ensureViewHas [ Selector.text text ]
