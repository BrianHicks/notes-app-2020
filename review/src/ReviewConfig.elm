module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

TODO:

  - jfmengels/review-debug
  - sparksp/elm-review-ports

-}

import NoExposingEverything
import NoImportingEverything
import NoMissingTypeAnnotation
import Review.Rule exposing (Rule)


config : List Rule
config =
    [ NoExposingEverything.rule
        |> Review.Rule.ignoreErrorsForDirectories [ "tests/" ]
    , NoImportingEverything.rule []
        |> Review.Rule.ignoreErrorsForDirectories [ "tests/" ]
    , NoMissingTypeAnnotation.rule
    ]
