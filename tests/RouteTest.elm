module RouteTest exposing (..)

import Database.ID as ID
import Expect exposing (Expectation)
import Node.Content as Content exposing (fromList, link, noteLink, text)
import Route exposing (..)
import Test exposing (..)
import Url


routeTest : Test
routeTest =
    describe "Route"
        [ test "can navigate to the root route" <|
            \_ -> expectRoundTripWorks Root
        , test "handles bad routes gracefully" <|
            \_ ->
                parse
                    { protocol = Url.Https
                    , host = "localhost"
                    , port_ = Nothing
                    , path = "/blah"
                    , query = Nothing
                    , fragment = Nothing
                    }
                    |> Expect.equal NotFound
        , describe "nodes by ID"
            [ test "works only with UUIDs" <|
                \_ ->
                    expectRoundTripWorks (NodeById (ID.fromInt 0))
            ]
        , describe "notes by name"
            [ test "does not work with no content" <|
                \_ ->
                    parse
                        { protocol = Url.Https
                        , host = "localhost"
                        , port_ = Nothing
                        , path = "/node/"
                        , query = Nothing
                        , fragment = Nothing
                        }
                        |> Expect.equal NotFound
            , test "works with basic text" <|
                \_ -> expectRoundTripWorks (NoteByName (fromList [ text "basic" ]))
            , test "works with spaces" <|
                \_ -> expectRoundTripWorks (NoteByName (fromList [ text "one two" ]))
            , test "works with slashes" <|
                \_ -> expectRoundTripWorks (NoteByName (fromList [ text "one/two" ]))
            , test "works with note link characters" <|
                \_ -> expectRoundTripWorks (NoteByName (fromList [ noteLink [ text "link" ] ]))
            , test "works with link characters" <|
                \_ ->
                    expectRoundTripWorks
                        (NoteByName
                            (fromList
                                [ link
                                    { children = [ text "link" ]
                                    , href = "https://bytes.zone"
                                    }
                                ]
                            )
                        )
            ]
        ]


expectRoundTripWorks : Route -> Expectation
expectRoundTripWorks route =
    case Url.fromString ("https://localhost" ++ toString route) of
        Nothing ->
            Expect.fail ("There was some problem turning " ++ Debug.toString route ++ " into a URL")

        Just url ->
            Expect.equal route (parse url)
