module Widgets.Text exposing
    ( text, textLineHeight
    , h1, h2
    )

{-|

@docs text, textLineHeight

@docs h1, h2

For finding sizes: <https://www.modularscale.com/?18&px&1.5>

-}

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


h2 : Style
h2 =
    Css.batch
        [ Css.fontSize (Css.px 27)
        , Css.lineHeight (Css.px 40.5)
        , Css.color (Colors.toCss Colors.blackDark)
        , Css.fontFamilies [ Css.fontFace "San Francisco", Css.fontFace "Arial", Css.sansSerif.value ]
        ]
