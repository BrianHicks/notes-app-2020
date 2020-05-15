module Widgets.Button exposing (Action(..), Attribute, button, css, enabled, transparent)

import Accessibility.Styled as Html exposing (Html)
import Css
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events


type Attribute
    = Style Style
    | Css (List Css.Style)
    | Enabled Bool


type Style
    = Default
    | Transparent


type Action msg
    = Submit
    | OnClick msg


transparent : Attribute
transparent =
    Style Transparent


style : Style -> Html.Attribute msg
style style_ =
    case style_ of
        Default ->
            Attrs.css []

        Transparent ->
            Attrs.css
                [ Css.margin Css.zero
                , Css.padding Css.zero
                , Css.backgroundColor Css.transparent
                , Css.border Css.zero
                , Css.fontSize (Css.em 1)
                ]


css : List Css.Style -> Attribute
css styles =
    Css styles


enabled : Bool -> Attribute
enabled =
    Enabled


customize : Attribute -> Config -> Config
customize attr config =
    case attr of
        Style style_ ->
            { config | style = style_ }

        Css css_ ->
            { config | css = css_ }

        Enabled enabled_ ->
            { config | enabled = enabled_ }


type alias Config =
    { style : Style
    , css : List Css.Style
    , enabled : Bool
    }


button : Action msg -> List Attribute -> List (Html msg) -> Html msg
button action attrs children =
    let
        config =
            List.foldl customize
                { style = Default
                , css = []
                , enabled = True
                }
                attrs
    in
    Html.button
        [ case action of
            Submit ->
                Attrs.attribute "data-no-onclick" ""

            OnClick msg ->
                Events.onClick msg
        , style config.style
        , Attrs.css config.css
        , Attrs.disabled (not config.enabled)
        ]
        children
