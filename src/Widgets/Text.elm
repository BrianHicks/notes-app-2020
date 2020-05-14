module Widgets.Text exposing (h1, text, textLineHeight)

import Css exposing (Style)
import Widgets.Colors as Colors


text : Style
text =
    Css.batch
        [ Css.fontSize (Css.px 18)
        , Css.lineHeight textLineHeight
        , Css.color (Colors.toCss Colors.blackDark)
        , Css.fontFamilies [ Css.fontFace "San Francisco", Css.fontFace "Arial", Css.sansSerif.value ]
        ]


textLineHeight : Css.Px
textLineHeight =
    Css.px 27


h1 : Style
h1 =
    Css.batch
        [ Css.fontSize (Css.px 40.5)
        , Css.lineHeight (Css.px 60.75)
        , Css.color (Colors.toCss Colors.blackDark)
        , Css.fontFamilies [ Css.fontFace "San Francisco", Css.fontFace "Arial", Css.sansSerif.value ]
        ]
