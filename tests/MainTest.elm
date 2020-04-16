module MainTest exposing (..)

import Database
import Expect exposing (Expectation)
import Json.Encode as Encode
import Main exposing (..)
import ProgramTest exposing (ProgramTest, SimulatedEffect, clickButton, done, ensureBrowserUrl, expectView, expectViewHas, expectViewHasNot, fillIn, simulateDomEvent)
import Route
import SimulatedEffect.Cmd as SCmd
import SimulatedEffect.Navigation as Navigation
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (Selector)


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
                    |> expectSidebar (Query.find [ Selector.tag "li" ] >> Query.has [ Selector.text "What's up?" ])
        , test "after editing, blurring finalizes the note" <|
            \_ ->
                start
                    |> addNote (Database.idFromInt 0) "What's up?"
                    |> simulateDomEvent (Query.find [ input ]) ( "blur", Encode.object [] )
                    |> expectViewDoesntHaveInput
        , test "after editing, hitting escape finalizes the note" <|
            \_ ->
                start
                    |> addNote (Database.idFromInt 0) "What's up?"
                    |> hitShortcutKey [] Esc
                    |> expectViewDoesntHaveInput
        , test "after editing a note, hitting enter creates a new child note" <|
            \_ ->
                start
                    |> addNote (Database.idFromInt 0) "What's up?"
                    |> hitShortcutKey [] Enter
                    |> fillIn "content" "Content" "Not much, you?"
                    |> hitShortcutKey [] Esc
                    |> expectNote (Query.has [ Selector.text "Not much, you?" ])
        , test "after adding two notes, you should be able to click to select either" <|
            \_ ->
                start
                    |> addNote (Database.idFromInt 0) "What's up?"
                    |> addNote (Database.idFromInt 1) "Not much."
                    |> clickButton "What's up?"
                    |> expectSidebar (Query.has [ Selector.text "What's up?" ])
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
                    |> expectNote
                        (Query.find
                            [ Selector.tag "li"
                            , Selector.containing [ Selector.text "Parent" ]
                            ]
                            >> Query.children [ Selector.tag "li" ]
                            >> Query.first
                            >> Query.has [ Selector.text "Child" ]
                        )
        , test "when adding a note, shift-tab dedents" <|
            \_ ->
                start
                    |> addNote (Database.idFromInt 0) "Note"
                    |> hitShortcutKey [] Enter
                    |> fillIn "content" "Content" "Parent"
                    |> hitShortcutKey [] Enter
                    |> hitShortcutKey [] Tab
                    |> fillIn "content" "Content" "Child"
                    |> hitShortcutKey [ Shift ] Tab
                    |> hitShortcutKey [] Esc
                    |> expectNote
                        (Query.find
                            [ Selector.tag "li"
                            , Selector.containing [ Selector.text "Parent" ]
                            ]
                            >> Query.children [ Selector.tag "li" ]
                            >> Query.first
                            >> Query.hasNot [ Selector.text "Child" ]
                        )
        , test "after a note has been edited, clicking it repoens it for editing" <|
            \_ ->
                start
                    |> addNote (Database.idFromInt 0) "Hey I'm a Note"
                    |> hitShortcutKey [] Esc
                    |> clickButton "Hey I'm a Note"
                    |> expectViewHasInput
        , test "after a child has been edited, clicking it reopens it for editing" <|
            \_ ->
                start
                    |> addNote (Database.idFromInt 0) "Note"
                    |> hitShortcutKey [] Enter
                    |> fillIn "content" "Content" "I'm a child!"
                    |> hitShortcutKey [] Esc
                    |> clickButton "I'm a child!"
                    |> expectViewHasInput
        , test "when a child is tabbed into a list with other children, it's inserted as the last child" <|
            \_ ->
                start
                    |> addNote (Database.idFromInt 0) "Note"
                    |> hitShortcutKey [] Enter
                    |> fillIn "content" "Content" "Child"
                    |> hitShortcutKey [] Enter
                    |> fillIn "content" "Content" "Grandchild 1"
                    |> hitShortcutKey [] Enter
                    |> fillIn "content" "Content" "Grandchild 2"
                    |> clickButton "Grandchild 1"
                    |> hitShortcutKey [] Tab
                    |> clickButton "Grandchild 2"
                    |> hitShortcutKey [] Tab
                    |> hitShortcutKey [] Esc
                    |> Expect.all
                        [ expectNote
                            (Query.find
                                [ Selector.tag "li"
                                , Selector.containing [ Selector.text "Child" ]
                                ]
                                >> Query.children [ Selector.tag "li" ]
                                >> Query.index 0
                                >> Query.has [ Selector.text "Grandchild 1" ]
                            )
                        , expectNote
                            (Query.find
                                [ Selector.tag "li"
                                , Selector.containing [ Selector.text "Child" ]
                                ]
                                >> Query.children [ Selector.tag "li" ]
                                >> Query.index 1
                                >> Query.has [ Selector.text "Grandchild 2" ]
                            )
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


expectSidebar : (Query.Single Msg -> Expectation) -> NotesTest -> Expectation
expectSidebar expect notesTest =
    expectView (Query.find [ Selector.tag "nav" ] >> expect) notesTest


expectNote : (Query.Single Msg -> Expectation) -> NotesTest -> Expectation
expectNote expect notesTest =
    expectView (Query.find [ Selector.tag "section" ] >> expect) notesTest


expectViewHasInput : NotesTest -> Expectation
expectViewHasInput =
    expectView (Query.has [ input ])


expectViewDoesntHaveInput : NotesTest -> Expectation
expectViewDoesntHaveInput =
    expectView (Query.hasNot [ input ])


input : Selector
input =
    Selector.all
        [ Selector.tag "input"
        , Selector.id "content"
        ]
