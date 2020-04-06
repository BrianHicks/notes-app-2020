module Main exposing (main)

import Browser
import Html.Styled as Html exposing (Html)


main : Program () () ()
main =
    Browser.application
        { init = \flags url key -> ( (), Cmd.none )
        , update = \msg model -> ( model, Cmd.none )
        , subscriptions = \model -> Sub.none
        , onUrlRequest = \urlRequest -> ()
        , onUrlChange = \url -> ()
        , view =
            \model ->
                { title = "Notes"
                , body = [ Html.toUnstyled (Html.text "TODO") ]
                }
        }
