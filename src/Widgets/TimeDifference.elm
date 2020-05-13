module Widgets.TimeDifference exposing (timeDifference)

import Accessibility.Styled as Html exposing (Html)
import Time
import Time.Extra exposing (Interval(..), diff)


timeDifference : Time.Posix -> Time.Posix -> Html msg
timeDifference before after =
    case diff Year Time.utc before after of
        0 ->
            case diff Month Time.utc before after of
                0 ->
                    case diff Week Time.utc before after of
                        0 ->
                            case diff Day Time.utc before after of
                                0 ->
                                    case diff Hour Time.utc before after of
                                        0 ->
                                            case diff Minute Time.utc before after of
                                                0 ->
                                                    case diff Second Time.utc before after of
                                                        0 ->
                                                            Html.text "just now"

                                                        1 ->
                                                            Html.text "just now"

                                                        otherwise ->
                                                            Html.text (String.fromInt otherwise ++ " seconds")

                                                1 ->
                                                    Html.text "a minute"

                                                otherwise ->
                                                    Html.text (String.fromInt otherwise ++ " minutes")

                                        1 ->
                                            Html.text "an hour"

                                        otherwise ->
                                            Html.text (String.fromInt otherwise ++ " hours")

                                1 ->
                                    Html.text "a day"

                                otherwise ->
                                    Html.text (String.fromInt otherwise ++ " days")

                        1 ->
                            Html.text "a week"

                        otherwise ->
                            Html.text (String.fromInt otherwise ++ " weeks")

                1 ->
                    Html.text "a month"

                otherwise ->
                    Html.text (String.fromInt otherwise ++ " months")

        1 ->
            Html.text "a year"

        otherwise ->
            Html.text (String.fromInt otherwise ++ " years")
