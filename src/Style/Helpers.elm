module Style.Helpers exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Lazy as Lazy
import Element.Region as Region
import Html.Attributes as HtmlAttr
import Style.Palette exposing (..)


sides =
    { bottom = 0
    , left = 0
    , right = 0
    , top = 0
    }


noAttr =
    htmlAttribute <| HtmlAttr.class ""


noHtmlAttr =
    HtmlAttr.class ""


buttonStyle isActive =
    [ centerX
    , padding 10
    , Background.color white
    , Font.family
        [ Font.typeface "Montserrat"
        , Font.sansSerif
        ]
    , Border.width 1
    , Border.rounded 2
    , focused [ Border.glow (rgb 1 1 1) 0 ]
    ]
        ++ (if isActive then
                [ mouseOver
                    [ Background.color grey
                    ]
                ]

            else
                [ alpha 0.3
                , htmlAttribute <| HtmlAttr.style "cursor" "default"
                ]
           )


textInputStyle =
    [ width (px 250)
    , paddingXY 5 5
    , spacing 15
    , focused [ Border.glow (rgb 1 1 1) 0 ]
    ]
