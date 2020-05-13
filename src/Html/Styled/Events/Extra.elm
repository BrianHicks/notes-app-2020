module Html.Styled.Events.Extra exposing (onClickPreventDefaultForLinkWithHref)

import Html.Styled as Html
import Html.Styled.Events as Events
import Json.Decode


{-| <https://gist.github.com/avh4/712d43d649b7624fab59285a70610707>
-}
onClickPreventDefaultForLinkWithHref : msg -> Html.Attribute msg
onClickPreventDefaultForLinkWithHref msg =
    let
        isSpecialClick : Json.Decode.Decoder Bool
        isSpecialClick =
            Json.Decode.map2
                (\isCtrl isMeta -> isCtrl || isMeta)
                (Json.Decode.field "ctrlKey" Json.Decode.bool)
                (Json.Decode.field "metaKey" Json.Decode.bool)

        succeedIfFalse : a -> Bool -> Json.Decode.Decoder ( a, Bool )
        succeedIfFalse msg_ preventDefault =
            case preventDefault of
                False ->
                    Json.Decode.succeed ( msg_, True )

                True ->
                    Json.Decode.fail "succeedIfFalse: condition was True"
    in
    Events.preventDefaultOn "click"
        (isSpecialClick
            |> Json.Decode.andThen (succeedIfFalse msg)
        )
