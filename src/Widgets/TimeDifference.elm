module Widgets.TimeDifference exposing (compact)

import Accessibility.Styled as Html exposing (Html)
import Css
import Html.Styled.Attributes as Attrs
import Time
import Time.Extra exposing (Interval(..), diff)
import Widgets.Colors as Colors
import Widgets.Text as Text


compact : Time.Posix -> Time.Posix -> Html msg
compact before after =
    Html.span
        [ Attrs.css
            [ Text.text
            , Css.color (Colors.toCss Colors.greyDark)
            , Css.fontSize (Css.em 0.8)
            ]
        ]
        [ case diff Year Time.utc before after of
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
                                                        Html.text "< 1m"

                                                    minutes ->
                                                        Html.text (String.fromInt minutes ++ "m")

                                            hours ->
                                                Html.text (String.fromInt hours ++ "h")

                                    days ->
                                        Html.text (String.fromInt days ++ "d")

                            weeks ->
                                Html.text (String.fromInt weeks ++ "W")

                    months ->
                        Html.text (String.fromInt months ++ "M")

            years ->
                Html.text (String.fromInt years ++ "Y")
        ]
