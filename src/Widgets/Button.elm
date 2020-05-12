module Widgets.Button exposing (Attribute, button, css, transparent)

import Accessibility.Styled as Html exposing (Html)
import Css
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events


type Attribute
    = Style Style
    | Css (List Css.Style)


type Style
    = Default
    | Transparent


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
                ]


css : List Css.Style -> Attribute
css styles =
    Css styles


customize : Attribute -> Config -> Config
customize attr config =
    case attr of
        Style style_ ->
            { config | style = style_ }

        Css css_ ->
            { config | css = css_ }


type alias Config =
    { style : Style
    , css : List Css.Style
    }


button : msg -> List Attribute -> List (Html msg) -> Html msg
button msg attrs children =
    let
        config =
            List.foldl customize
                { style = Default
                , css = []
                }
                attrs
    in
    Html.button
        [ Events.onClick msg
        , style config.style
        , Attrs.css config.css
        ]
        children
