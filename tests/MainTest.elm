module MainTest exposing (..)

import Database
import Expect exposing (Expectation)
import Json.Encode as Encode
import Main exposing (..)
import ProgramTest exposing (ProgramTest, SimulatedEffect, advanceTime, clickButton, done, ensureBrowserUrl, expectView, expectViewHas, expectViewHasNot, fillIn, simulateDomEvent)
import Route
import SimulatedEffect.Cmd as SCmd
import SimulatedEffect.Navigation as Navigation
import SimulatedEffect.Process as SProcess
import SimulatedEffect.Task as STask
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (Selector)
import Time


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

        GetTimeFor next ->
            STask.perform next (STask.succeed (Time.millisToPosix 0))

        SaveAfter amount ->
            SProcess.sleep amount
                |> STask.andThen (\_ -> STask.succeed (Time.millisToPosix 100000))
                |> STask.perform DelayTriggeredSave

        PersistLogEntry _ ->
            SCmd.none


programTest : Test
programTest =
    describe "notes"
        [ test "pass" (\_ -> Expect.pass)
        , test "it should be possible to add a note and see it in the sidebar after adding" <|
            \_ ->
                start
                    |> clickButton "New Note"
                    |> fillIn "content" "Content" "What's up?"
                    |> advanceTime 1000
                    |> expectSidebar (Query.find [ Selector.tag "li" ] >> Query.has [ Selector.text "What's up?" ])

        -- , test "after editing, blurring finalizes the note" <|
        --     \_ ->
        --         start
        --             |> addNote "What's up?"
        --             |> blur
        --             |> expectViewDoesntHaveInput
        -- , test "after editing, hitting escape finalizes the note" <|
        --     \_ ->
        --         start
        --             |> addNote "What's up?"
        --             |> hitShortcutKey [] Escape
        --             |> expectViewDoesntHaveInput
        -- , test "if I make a syntax error, I should see it" <|
        --     \_ ->
        --         start
        --             |> clickButton "New Note"
        --             |> fillIn "content" "Content" "test ["
        --             |> expectNote (Query.find [ Selector.tag "li" ] >> Query.has [ Selector.text "While parsing a link, I was expecting the 'link' part of a [link](url)" ])
        -- , test "after editing a note, hitting enter creates a new child note" <|
        --     \_ ->
        --         start
        --             |> addNoteAndChildren "What's up?" [ "Not much, you?" ]
        --             |> expectNote (Query.has [ Selector.text "Not much, you?" ])
        -- , test "after adding two notes, you should be able to click to select either" <|
        --     \_ ->
        --         start
        --             |> addNote "What's up?"
        --             |> addNote "Not much."
        --             |> clickButton "What's up?"
        --             |> expectSidebar (Query.has [ Selector.text "What's up?" ])
        -- , test "after a note has been edited, clicking it repoens it for editing" <|
        --     \_ ->
        --         start
        --             |> addNoteAndChildren "Hey, I'm a Note!" []
        --             |> clickButton "Hey, I'm a Note!"
        --             |> expectAnEditingNode
        -- , test "after a child has been edited, clicking it reopens it for editing" <|
        --     \_ ->
        --         start
        --             |> addNoteAndChildren "Note" [ "I'm a child!" ]
        --             |> clickButton "I'm a child!"
        --             |> expectAnEditingNode
        -- , test "when adding a note, tab indents" <|
        --     \_ ->
        --         start
        --             |> addNoteAndChildren "Note" [ "Parent", "Child" ]
        --             |> clickButton "Child"
        --             |> hitShortcutKey [] Tab
        --             |> hitShortcutKey [] Escape
        --             |> expectNote
        --                 (Query.find
        --                     [ Selector.tag "li"
        --                     , Selector.containing [ Selector.text "Parent" ]
        --                     ]
        --                     >> Query.children [ Selector.tag "li" ]
        --                     >> Query.first
        --                     >> Query.has [ Selector.text "Child" ]
        --                 )
        -- , test "when adding a note, shift-tab dedents" <|
        --     \_ ->
        --         start
        --             |> addNoteAndChildren "Note" [ "Parent", "Child" ]
        --             |> clickButton "Child"
        --             |> hitShortcutKey [] Tab
        --             |> hitShortcutKey [ Shift ] Tab
        --             |> hitShortcutKey [] Escape
        --             |> expectNote
        --                 (Query.find
        --                     [ Selector.tag "li"
        --                     , Selector.containing [ Selector.text "Parent" ]
        --                     ]
        --                     >> Query.children [ Selector.tag "li" ]
        --                     >> Query.first
        --                     >> Query.hasNot [ Selector.text "Child" ]
        --                 )
        -- , test "when a child is tabbed into a list with other children, it's inserted as the last child" <|
        --     \_ ->
        --         start
        --             |> addNoteAndChildren "Note" [ "Child", "Grandchild 1", "Grandchild 2" ]
        --             |> clickButton "Grandchild 1"
        --             |> hitShortcutKey [] Tab
        --             |> clickButton "Grandchild 2"
        --             |> hitShortcutKey [] Tab
        --             |> hitShortcutKey [] Escape
        --             |> expectSiblingsIn
        --                 (Query.find
        --                     [ Selector.tag "li"
        --                     , Selector.containing [ Selector.text "Child" ]
        --                     ]
        --                     >> Query.children [ Selector.tag "li" ]
        --                 )
        --                 [ Selector.text "Grandchild 1"
        --                 , Selector.text "Grandchild 2"
        --                 ]
        -- , test "hitting backspace in an empty node removes it from the note" <|
        --     \_ ->
        --         start
        --             |> addNote "Note"
        --             |> hitShortcutKey [] Enter
        --             |> hitShortcutKey [] Backspace
        --             |> expectNote
        --                 (Query.find
        --                     [ Selector.tag "h1"
        --                     , Selector.containing [ Selector.text "Note" ]
        --                     ]
        --                     >> Query.children [ Selector.tag "li" ]
        --                     >> Query.count (Expect.equal 0)
        --                 )
        -- , test "hitting alt-up while editing moves the node up" <|
        --     \_ ->
        --         start
        --             |> addNoteAndChildren "Note" [ "First", "Second" ]
        --             |> clickButton "Second"
        --             |> hitShortcutKey [ Alt ] ArrowUp
        --             |> hitShortcutKey [] Escape
        --             |> expectSiblingsIn
        --                 (Query.find [ Selector.tag "section" ] >> Query.children [ Selector.tag "li" ])
        --                 [ Selector.text "Second"
        --                 , Selector.text "First"
        --                 ]
        -- , test "hitting alt-down while editing moves the node down" <|
        --     \_ ->
        --         start
        --             |> addNoteAndChildren "Note" [ "First", "Second" ]
        --             |> clickButton "First"
        --             |> hitShortcutKey [ Alt ] ArrowDown
        --             |> hitShortcutKey [] Escape
        --             |> expectSiblingsIn
        --                 (Query.find [ Selector.tag "section" ] >> Query.children [ Selector.tag "li" ])
        --                 [ Selector.text "Second"
        --                 , Selector.text "First"
        --                 ]
        -- , test "hitting alt-up when the child is the first child moves it above the parent" <|
        --     \_ ->
        --         start
        --             |> addNoteAndChildren "Note" [ "Parent", "Child" ]
        --             |> clickButton "Child"
        --             |> hitShortcutKey [] Tab
        --             |> hitShortcutKey [ Alt ] ArrowUp
        --             |> hitShortcutKey [] Escape
        --             |> expectSiblingsIn
        --                 (Query.find [ Selector.tag "section" ] >> Query.children [ Selector.tag "li" ])
        --                 [ Selector.text "Child"
        --                 , Selector.text "Parent"
        --                 ]
        -- , test "hitting alt-down when the child is the last child moves it below the parent's next sibling" <|
        --     \_ ->
        --         start
        --             |> addNoteAndChildren "Note" [ "Parent", "Child", "Aunt" ]
        --             |> clickButton "Child"
        --             |> hitShortcutKey [] Tab
        --             |> hitShortcutKey [ Alt ] ArrowDown
        --             |> hitShortcutKey [] Escape
        --             |> expectSiblingsIn
        --                 (Query.find [ Selector.tag "section" ] >> Query.children [ Selector.tag "li" ])
        --                 [ Selector.text "Parent"
        --                 , Selector.text "Aunt"
        --                 , Selector.text "Child"
        --                 ]
        ]


addNote : String -> NotesTest -> NotesTest
addNote text =
    clickButton "New Note" >> fillIn "content" "Content" text


addNoteAndChildren : String -> List String -> NotesTest -> NotesTest
addNoteAndChildren note siblings test =
    let
        withNote =
            test
                |> addNote note
                |> hitShortcutKey [] Enter

        addSiblings =
            siblings
                |> List.map (fillIn "content" "Content")
                |> List.intersperse (hitShortcutKey [] Enter)
    in
    List.foldl (\action progress -> action progress) withNote addSiblings
        |> hitShortcutKey [] Escape


expectSiblingsIn : (Query.Single Msg -> Query.Multiple Msg) -> List Selector -> NotesTest -> Expectation
expectSiblingsIn parent assertions =
    assertions
        |> List.indexedMap
            (\i selector ->
                expectView
                    (parent
                        >> Query.index i
                        >> Query.has [ selector ]
                    )
            )
        |> Expect.all


blur : NotesTest -> NotesTest
blur =
    simulateDomEvent (Query.find [ input ]) ( "blur", Encode.object [] )


type Key
    = Enter
    | Escape
    | Tab
    | Backspace
    | ArrowUp
    | ArrowDown


type Modifier
    = Shift
    | Alt


hitShortcutKey : List Modifier -> Key -> NotesTest -> NotesTest
hitShortcutKey modifiers key =
    simulateDomEvent (Query.find [ Selector.id "content" ]) (keyDown modifiers key)


keyDown : List Modifier -> Key -> ( String, Encode.Value )
keyDown modifiers key =
    ( "keydown"
    , Encode.object
        [ ( "key", Encode.string (Debug.toString key) )
        , ( "shiftKey", Encode.bool (List.member Shift modifiers) )
        , ( "altKey", Encode.bool (List.member Alt modifiers) )
        ]
    )


expectSidebar : (Query.Single Msg -> Expectation) -> NotesTest -> Expectation
expectSidebar expect notesTest =
    expectView (Query.find [ Selector.tag "nav" ] >> expect) notesTest


expectNote : (Query.Single Msg -> Expectation) -> NotesTest -> Expectation
expectNote expect notesTest =
    expectView (Query.find [ Selector.tag "section" ] >> expect) notesTest


expectAnEditingNode : NotesTest -> Expectation
expectAnEditingNode =
    expectView (Query.has [ input ])


expectViewDoesntHaveInput : NotesTest -> Expectation
expectViewDoesntHaveInput =
    expectView (Query.hasNot [ input ])


input : Selector
input =
    Selector.all
        [ Selector.tag "textarea"
        , Selector.id "content"
        ]
