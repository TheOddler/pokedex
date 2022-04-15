module Pokemon.List exposing (view)

import Css exposing (..)
import Css.Media as Media exposing (canHover, withMedia)
import Css.Transitions as Transitions exposing (cubicBezier, transition)
import Helpers exposing (stopPropagationOnClick)
import Html.Styled exposing (Html, div, img, text)
import Html.Styled.Attributes exposing (css, src)
import Html.Styled.Lazy as Lazy
import Maybe.Extra as Maybe
import Pokemon exposing (Pokemon)
import Pokemon.Details
import Pokemon.SharedStyles as SharedStyles
import Type


view : List Pokemon -> (Pokemon -> Bool) -> Html Pokemon.Details.Msg
view pokemonList filter =
    div
        [ css
            [ displayFlex
            , flexWrap wrap
            , justifyContent center
            , alignItems stretch
            ]
        ]
    <|
        List.map (\p -> viewListElementWrapper (filter p) p) pokemonList


viewListElementWrapper : Bool -> Pokemon -> Html Pokemon.Details.Msg
viewListElementWrapper isVisible pkm =
    let
        style =
            margin (em 0.2)

        invisibleStyle =
            display none
    in
    div
        [ css <|
            if isVisible then
                [ style ]

            else
                [ style, invisibleStyle ]
        ]
        [ Lazy.lazy viewListElement pkm ]


viewListElementWrapperAnimated : Bool -> Pokemon -> Html Pokemon.Details.Msg
viewListElementWrapperAnimated isVisible pkm =
    let
        style =
            Css.batch
                [ width (rem 7)
                , margin (em 0.2)
                , transition
                    [ Transitions.width3 200 0 Transitions.easeInOut
                    , Transitions.visibility3 200 0 Transitions.easeInOut
                    ]
                ]

        invisibleStyle =
            Css.batch
                [ width (px 0)
                , padding (em 0)
                , margin (em 0)
                , visibility hidden
                ]
    in
    div
        [ css <|
            Maybe.values
                [ Just style
                , if isVisible then
                    Nothing

                  else
                    Just invisibleStyle
                ]
        ]
        [ Lazy.lazy viewListElement pkm ]


viewListElement : Pokemon -> Html Pokemon.Details.Msg
viewListElement pkm =
    div
        [ stopPropagationOnClick <| Pokemon.Details.Select pkm
        , css <|
            [ Type.backgroundFor pkm.typing
            , SharedStyles.badgeStyle
            , position relative -- to make zIndex work
            , height (pct 100)
            , withMedia [ Media.all [ Media.hover canHover ] ]
                [ hover
                    [ transform <| scale 1.5
                    , zIndex (int 100)
                    , property "box-shadow" "inset 0 -2px 2px rgba(0, 0, 0, 0.2), inset 0 2px 2px rgba(255, 255, 255, 0.2), 1px 3px 3px 3px rgba(0, 0, 0, .3);"
                    ]
                ]
            , transition
                [ Transitions.transform3 500 0 (cubicBezier 0 1 0.5 1.5)
                , Transitions.boxShadow3 500 0 (cubicBezier 0 1 0.5 1.5)
                ]
            ]
        ]
        [ img
            [ src pkm.imageUrl
            , css
                [ width (rem 6)
                , minHeight (rem 6)
                ]
            ]
            []
        , div [] [ text pkm.fullName ]
        ]
