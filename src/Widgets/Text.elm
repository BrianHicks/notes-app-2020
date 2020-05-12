module Widgets.Text exposing (..)

import Css exposing (Style)


text : Style
text =
    Css.batch
        [ Css.fontSize (Css.px 18)
        , Css.lineHeight (Css.px 27)
        , Css.fontFamilies [ Css.fontFace "San Francisco", Css.fontFace "Arial", Css.sansSerif.value ]
        ]


h1 : Style
h1 =
    Css.batch
        [ Css.fontSize (Css.px 40.5)
        , Css.lineHeight (Css.px 60.75)
        , Css.fontFamilies [ Css.fontFace "San Francisco", Css.fontFace "Arial", Css.sansSerif.value ]
        ]
