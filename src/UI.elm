module UI exposing (..)

import Element as El
import Element.Background as Bg
import Element.Border as Bd
import Element.Font as Font
import Element.Input as In
import Html as H
import Html.Attributes as A
import Colors exposing (..)

activeButton : List (El.Attribute msg)
activeButton =
    [ Bg.color primaryColor
    , Font.color (El.rgb 255 255 255)
    , El.paddingXY 16 12
    , Bd.rounded 5
    , El.width (El.px 140)
    , Font.center
    ]


passiveButton : El.Attribute msg -> List (El.Attribute msg)
passiveButton onClick =
    [ Bg.color primaryLightColor
    , Font.color primaryDarkColor
    , El.paddingXY 16 12
    , Bd.rounded 5
    , El.width (El.px 140)
    , Font.center
    , onClick
    ]


checkbox : (Bool -> msg) -> Bool -> El.Element msg
checkbox onChange isChecked =
    let
        ( icon, color ) =
            if isChecked then
                ( "fa fa-check-square-o", successColor )

            else
                ( "fa fa-square-o", primaryColor )
    in
    El.column
        []
        [ In.checkbox
            [ El.width (El.px 30)
            , Font.color color
            ]
            { icon =
                El.html
                    (H.i [ A.class icon ] [])
                    |> always
            , label = In.labelHidden ""
            , checked = isChecked
            , onChange = onChange
            }
        ]
