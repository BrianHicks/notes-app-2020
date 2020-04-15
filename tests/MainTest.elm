module MainTest exposing (..)

import Database
import Expect
import Json.Encode as Encode
import Main exposing (..)
import ProgramTest exposing (ProgramTest, SimulatedEffect, clickButton, done, ensureBrowserUrl, expectViewHas, expectViewHasNot, fillIn, simulateDomEvent)
import Route
import SimulatedEffect.Cmd as SCmd
import SimulatedEffect.Navigation as Navigation
import Test exposing (..)
import Test.Html.Query as Query
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

        FocusOnContent ->
            SCmd.none


programTest : Test
programTest =
    describe "notes"
        [ test "it should be possible to add a note and see it in the sidebar after adding" <|
            \_ ->
                start
                    |> addNote (Database.idFromInt 0) "What's up?"
                    |> expectViewHas [ Selector.text "What's up?" ]
        , test "after editing, blurring finalizes the note" <|
            \_ ->
                start
                    |> addNote (Database.idFromInt 0) "What's up?"
                    |> simulateDomEvent (Query.find [ Selector.id "content" ]) ( "blur", Encode.object [] )
                    |> expectViewHasNot [ Selector.id "content" ]
        , test "after editing, hitting escape finalizes the note" <|
            \_ ->
                start
                    |> addNote (Database.idFromInt 0) "What's up?"
                    |> hitShortcutKey [] Esc
                    |> expectViewHasNot [ Selector.id "content" ]
        , test "after editing a note, hitting enter creates a new child note" <|
            \_ ->
                start
                    |> addNote (Database.idFromInt 0) "What's up?"
                    |> hitShortcutKey [] Enter
                    |> fillIn "content" "Content" "Not much, you?"
                    |> hitShortcutKey [] Esc
                    |> expectViewHas [ Selector.text "Not much, you?" ]
        , test "after adding two notes, you should be able to click to select either" <|
            \_ ->
                start
                    |> addNote (Database.idFromInt 0) "What's up?"
                    |> addNote (Database.idFromInt 1) "Not much."
                    |> clickButton "What's up?"
                    -- TODO: expectation that the thing gets selected
                    |> done
        , test "when adding a note, tab indents further" <|
            \_ ->
                start
                    |> addNote (Database.idFromInt 0) "Note"
                    |> hitShortcutKey [] Enter
                    |> fillIn "content" "Content" "Parent"
                    |> hitShortcutKey [] Enter
                    |> hitShortcutKey [] Tab
                    |> fillIn "content" "Content" "Child"
                    |> hitShortcutKey [] Esc
                    |> expectViewHas
                        [ Selector.text "Parent"
                        , Selector.containing [ Selector.text "Child" ]
                        ]
        ]


addNote : Database.ID -> String -> NotesTest -> NotesTest
addNote id text =
    clickButton "New Note"
        >> ensureBrowserUrl (Expect.equal ("https://localhost/node/" ++ Database.idToString id))
        >> fillIn "content" "Content" text


type Key
    = Enter
    | Esc
    | Tab


type Modifier
    = Shift


hitShortcutKey : List Modifier -> Key -> NotesTest -> NotesTest
hitShortcutKey modifiers key =
    simulateDomEvent (Query.find [ Selector.id "content" ]) (keyDown modifiers key)


keyDown : List Modifier -> Key -> ( String, Encode.Value )
keyDown modifiers key =
    let
        code =
            case key of
                Enter ->
                    13

                Esc ->
                    27

                Tab ->
                    9
    in
    ( "keydown"
    , Encode.object
        [ ( "keyCode", Encode.int code )
        , ( "shiftKey", Encode.bool (List.member Shift modifiers) )
        ]
    )
