module Colors exposing
    ( pinkLight, pinkDark
    , yellowLight, yellowDark
    , redLight, redDark
    , cyanLight, cyanDark
    , greenLight, greenDark
    , tealLight, tealDark
    , blueLight, blueDark
    , purpleLight, purpleDark
    , greyLight, greyDark
    , blackLight, blackDark
    , whiteLight, whiteDark
    )

{-|

@docs pinkLight, pinkDark
@docs yellowLight, yellowDark
@docs redLight, redDark
@docs cyanLight, cyanDark
@docs greenLight, greenDark
@docs tealLight, tealDark
@docs blueLight, blueDark
@docs purpleLight, purpleDark
@docs greyLight, greyDark
@docs blackLight, blackDark
@docs whiteLight, whiteDark

-}

import Color exposing (Color)



-- from the Canada palette: https://flatuicolors.com/palette/ca


{-| JIGGLYPUFF
-}
pinkLight : Color
pinkLight =
    Color.fromRGB ( 0xFF, 0x9F, 0xF3 )


{-| LIAN HONG LOTUS PINK
-}
pinkDark : Color
pinkDark =
    Color.fromRGB ( 0xF3, 0x68, 0xE0 )


{-| CASANODORA YELLOW
-}
yellowLight : Color
yellowLight =
    Color.fromRGB ( 0xFE, 0xCA, 0x57 )


{-| DOUBLE DRAGON SKIN
-}
yellowDark : Color
yellowDark =
    Color.fromRGB ( 0xFF, 0x9F, 0x43 )


{-| PASTEL RED
-}
redLight : Color
redLight =
    Color.fromRGB ( 0xFF, 0x6B, 0x6B )


{-| AMOUR
-}
redDark : Color
redDark =
    Color.fromRGB ( 0xEE, 0x52, 0x53 )


{-| MEGAMAN
-}
cyanLight : Color
cyanLight =
    Color.fromRGB ( 0x48, 0xDB, 0xFB )


{-| CYANITE
-}
cyanDark : Color
cyanDark =
    Color.fromRGB ( 0x0A, 0xBD, 0xE3 )


{-| WILD CARIBBEAN GREEN
-}
greenLight : Color
greenLight =
    Color.fromRGB ( 0x1D, 0xD1, 0xA1 )


{-| DARK MOUNTAIN MEADOW
-}
greenDark : Color
greenDark =
    Color.fromRGB ( 0x10, 0xAC, 0x84 )


{-| JADE DUST
-}
tealLight : Color
tealLight =
    Color.fromRGB ( 0x00, 0xD2, 0xD3 )


{-| AQUA VELVET
-}
tealDark : Color
tealDark =
    Color.fromRGB ( 0x01, 0xA3, 0xA4 )


{-| JOUSE BLUE
-}
blueLight : Color
blueLight =
    Color.fromRGB ( 0x54, 0xA0, 0xFF )


{-| BLEU DE FRANCE
-}
blueDark : Color
blueDark =
    Color.fromRGB ( 0x2E, 0x86, 0xDE )


{-| NASU PURPLE
-}
purpleLight : Color
purpleLight =
    Color.fromRGB ( 0x5F, 0x27, 0xCD )


{-| BLUEBELL
-}
purpleDark : Color
purpleDark =
    Color.fromRGB ( 0x34, 0x1F, 0x97 )


{-| LIGHT BLUE BALLERINA
-}
greyLight : Color
greyLight =
    Color.fromRGB ( 0xC8, 0xD6, 0xE5 )


{-| STORM PETREL
-}
greyDark : Color
greyDark =
    Color.fromRGB ( 0x83, 0x95, 0xA7 )


{-| FUEL TOWN
-}
blackLight : Color
blackLight =
    Color.fromRGB ( 0x57, 0x65, 0x74 )


{-| IMPERIAL PRIMER
-}
blackDark : Color
blackDark =
    Color.fromRGB ( 0x22, 0x2F, 0x3E )



-- from the British palette: https://flatuicolors.com/palette/gb


{-| LYNX WHITE
-}
whiteLight : Color
whiteLight =
    Color.fromRGB ( 0xF5, 0xF6, 0xFA )


{-| HINT OF PENSIVE
-}
whiteDark : Color
whiteDark =
    Color.fromRGB ( 0xDC, 0xDD, 0xE1 )
