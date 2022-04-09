module Pokemon.SharedStyles exposing (..)

import Css exposing (..)


badgeStyle : Style
badgeStyle =
    Css.batch
        [ borderRadius (rem 1)
        , overflow hidden
        , boxSizing borderBox
        , property "box-shadow" "inset 0 -2px 2px rgba(0, 0, 0, 0.2), inset 0 2px 2px rgba(255, 255, 255, 0.2);"
        , cursor pointer
        , width (rem 7)
        , padding4 (em 0) (em 0.2) (em 0.5) (em 0.2)
        ]
