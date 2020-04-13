module MainTest exposing (..)

import Expect
import Main exposing (..)
import ProgramTest exposing (ProgramTest, clickButton, done, ensureBrowserUrl, expectViewHas, fillIn)
import Route
import SimulatedEffect.Cmd as SCmd
import SimulatedEffect.Navigation as Navigation
import Test exposing (..)
import Test.Html.Selector as Selector


start : ProgramTest (Model ()) Msg Effect
start =
    ProgramTest.createApplication
        { init = init
        , onUrlChange = UrlChanged
        , onUrlRequest = ClickedLink
        , update = update
        , view = view
        }
        |> ProgramTest.withBaseUrl "https://localhost/"
        |> ProgramTest.withSimulatedEffects
            (\effect ->
                case effect of
                    NoEffect ->
                        SCmd.none

                    LoadUrl url ->
                        SCmd.none

                    PushUrl url ->
                        Navigation.pushUrl (Route.toString url)
            )
        |> ProgramTest.start ()


programTest : Test
programTest =
    describe "notes"
        [ test "it should be possible to add a note and see it in the sidebar after adding" <|
            \_ ->
                start
                    |> clickButton "New Note"
                    |> ensureBrowserUrl (Expect.equal "https://localhost/notes/0")
                    |> fillIn "node-0" "Title" "What's up?"
                    |> expectViewHas [ Selector.text "What's up?" ]
        ]
