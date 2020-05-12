module Icons exposing (chick)

import Color exposing (Color)
import Html.Styled exposing (Html)
import Svg.Styled as Svg exposing (Svg)
import Svg.Styled.Attributes exposing (d, fill, style, version, viewBox, x, y)


{-| chick by Symbolon from the Noun Project
-}
chick : { shell : Color, chick : Color } -> Html msg
chick colors =
    Svg.svg
        [ version "1.1"
        , x "0px"
        , y "0px"
        , viewBox "0 0 160 200"
        , style "enable-background: new 0 0 160 160;"
        ]
        [ Svg.path
            [ fill (Color.toRGBString colors.chick)
            , d "M42.203,84.608c0.201,0.099,0.401,0.184,0.601,0.276c-0.492,2.908-0.744,5.879-0.744,8.869c0,1.095,0.044,2.189,0.111,3.281 l6.906-3.891c0.028-2.176,0.217-4.335,0.542-6.458c0.488,0.038,0.973,0.064,1.448,0.064c4.773,0,8.874-1.852,11.348-4.327 c1.367-1.366,1.367-3.583,0-4.949c-1.367-1.367-3.582-1.367-4.949,0c-1.927,1.926-5.727,2.951-9.812,1.773 c-0.016-0.004-0.03-0.012-0.046-0.016c-0.005-0.001-0.01-0.001-0.015-0.002c-0.736-0.216-1.48-0.495-2.224-0.862 c-6.127-3.196-7.962-9.668-8.51-13.241c-0.743-4.838-0.122-9.471,1.138-13.01c5.866,10.22,16.789,15.313,24.214,15.824 c4.948,3.607,11.132,5.758,17.838,5.758c6.704,0,12.885-2.15,17.833-5.755c7.427-0.472,18.418-5.562,24.311-15.828 c1.26,3.539,1.881,8.172,1.138,13.01c-0.548,3.573-2.383,10.046-8.44,13.206c-4.971,2.452-9.883,1.423-12.167-0.858 c-1.367-1.367-3.582-1.367-4.949,0c-1.367,1.366-1.367,3.583,0,4.949c2.47,2.47,6.555,4.318,11.331,4.318 c0.421,0,0.849-0.019,1.28-0.049c0.359,2.32,0.554,4.682,0.554,7.062c0,1.878-0.128,3.738-0.351,5.576l3.852-5.589 c0.844-1.225,2.076-2.042,3.432-2.394c-0.096-2.148-0.327-4.278-0.675-6.378c0.287-0.127,0.574-0.253,0.862-0.395 c6.567-3.427,10.897-9.956,12.191-18.386c1.42-9.248-1.094-19.212-6.112-24.23c-0.877-0.878-2.155-1.224-3.353-0.914 c-1.2,0.311-2.146,1.234-2.487,2.427c-2.1,7.348-7.064,11.899-11.843,14.519c1.887-3.678,2.948-7.806,2.948-12.165 c0-3.592-0.727-7.023-2.036-10.179l-1.771-0.281l-3.822,5.557c0.407,1.574,0.629,3.214,0.629,4.903 c0,11.509-10.027,20.873-22.354,20.873c-12.325,0-22.354-9.364-22.354-20.873c0-0.58,0.032-1.153,0.082-1.722l-5.426-4.311 l-1.044,0.367c-0.4,1.83-0.611,3.725-0.611,5.666c0,4.329,1.045,8.43,2.907,12.089c-4.738-2.631-9.631-7.162-11.711-14.443 c-0.341-1.192-1.287-2.116-2.487-2.427c-1.202-0.311-2.476,0.037-3.353,0.913c-5.018,5.019-7.532,14.982-6.112,24.23 C31.234,74.617,35.564,81.146,42.203,84.608z"
            ]
            []
        , Svg.path
            [ fill (Color.toRGBString colors.chick)
            , d "M75.039,42.602c0-1.273-1.032-2.305-2.305-2.305s-2.305,1.032-2.305,2.305s1.032,2.305,2.305,2.305 S75.039,43.875,75.039,42.602z"
            ]
            []
        , Svg.path
            [ fill (Color.toRGBString colors.chick)
            , d "M87.366,40.298c-1.273,0-2.305,1.032-2.305,2.305s1.032,2.305,2.305,2.305s2.305-1.032,2.305-2.305 S88.639,40.298,87.366,40.298z"
            ]
            []
        , Svg.path
            [ fill (Color.toRGBString colors.shell)
            , d "M130.76,98.466l-9.933-4.508c-1.544-0.697-3.366-0.194-4.328,1.201l-6.937,10.063l-13.757-15.56 c-1.213-1.373-3.282-1.578-4.741-0.467L83.7,94.8l-3.95-4.99c-0.723-0.914-1.849-1.404-3.015-1.317 c-1.162,0.09-2.202,0.752-2.776,1.767l-9.989,17.662l-6.985-12.352c-0.95-1.681-3.081-2.274-4.764-1.327l-12.233,6.893l-7.763-6.522 c-0.631-0.53-1.432-0.82-2.256-0.82c-1.909,0-3.465,1.529-3.5,3.438c-0.297,16.715,5.295,31.777,15.745,42.415 c9.662,9.835,22.65,15.033,37.561,15.033h0.542c29.709,0,51.595-21.631,53.223-52.602C133.63,100.328,132.422,98.812,130.76,98.466z M80.316,147.678h-0.542c-12.99,0-24.252-4.474-32.567-12.938c-7.504-7.639-12.137-17.978-13.399-29.653l3.533,2.968 c1.117,0.938,2.699,1.086,3.969,0.37l11.301-6.367l8.312,14.697c0.621,1.099,1.785,1.777,3.047,1.777 c1.262,0,2.426-0.679,3.047-1.777L77.49,98.233l2.853,3.604c1.183,1.495,3.346,1.768,4.863,0.613l7.51-5.714l14.567,16.476 c0.713,0.807,1.764,1.249,2.833,1.175c1.074-0.064,2.06-0.62,2.671-1.507l7.814-11.337l5.767,2.617 C124.272,126.21,109.503,147.678,80.316,147.678z"
            ]
            []
        , Svg.path
            [ fill (Color.toRGBString colors.shell)
            , d "M41.928,39.675c0.849,0.425,1.832,0.487,2.726,0.172l8.172-2.872l7.293,5.793c0.626,0.497,1.395,0.759,2.177,0.759 c0.287,0,0.576-0.035,0.859-0.107c1.059-0.268,1.932-1.015,2.36-2.02l3.551-8.327l4.547,4.085c1.25,1.124,3.128,1.2,4.465,0.177 l7.071-5.408l7.981,9.37c0.705,0.828,1.745,1.285,2.845,1.226c1.087-0.056,2.086-0.615,2.703-1.512l5.752-8.364l7.697,1.224 c0.872,0.139,1.762-0.058,2.494-0.546c0.785-0.523,1.326-1.342,1.499-2.27s-0.035-1.886-0.579-2.658 C103.549,11.365,89.071,5.322,80.316,5.322c-0.091,0-0.182,0.003-0.271,0.01c-0.09-0.007-0.181-0.01-0.271-0.01 c-10.246,0-26.689,7.717-39.31,29.465c-0.485,0.836-0.602,1.835-0.325,2.76C40.417,38.473,41.064,39.243,41.928,39.675z M79.774,12.322c0.091,0,0.182-0.003,0.271-0.01c0.089,0.007,0.18,0.01,0.271,0.01c1.359,0,12.164,0.424,23.968,13.213l-0.935-0.148 c-1.337-0.212-2.669,0.361-3.434,1.473L95.5,33.282l-7.174-8.421c-1.205-1.417-3.312-1.64-4.79-0.511l-7.403,5.662l-6.026-5.415 c-0.84-0.754-1.991-1.055-3.096-0.814c-1.103,0.244-2.02,1.005-2.463,2.044l-3.669,8.604l-5.212-4.14 c-0.628-0.499-1.398-0.759-2.178-0.759c-0.39,0-0.782,0.065-1.16,0.198l-0.078,0.028C62.788,16.218,74.076,12.322,79.774,12.322z"
            ]
            []
        , Svg.path
            [ fill (Color.toRGBString colors.chick)
            , d "M77.997,61.629c0.467,0.673,1.234,1.075,2.054,1.075s1.587-0.401,2.054-1.075l6.455-9.3c0.39-0.562,0.533-1.258,0.396-1.928 s-0.544-1.254-1.123-1.617c-4.801-3.002-10.762-3.002-15.563,0c-0.579,0.363-0.985,0.947-1.123,1.617s0.006,1.367,0.396,1.928 L77.997,61.629z M82.75,51.929l-2.7,3.891l-2.7-3.891C79.116,51.41,80.985,51.41,82.75,51.929z"
            ]
            []
        ]
