module Pokemon.List exposing (view)

import Css exposing (..)
import Css.Media as Media exposing (canHover, withMedia)
import Css.Transitions as Transitions exposing (cubicBezier, transition)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, src)
import Html.Styled.Events exposing (onClick)
import Pokemon exposing (Pokemon)
import Pokemon.SharedStyles as SharedStyles
import Type


view : List Pokemon -> (Pokemon -> Bool) -> Html Pokemon.Msg
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
        List.map (\p -> viewListElement (filter p) p) pokemonList


viewListElement : Bool -> Pokemon -> Html Pokemon.Msg
viewListElement isVisible pkm =
    div
        [ onClick <| Pokemon.Select pkm
        , css <|
            [ Type.backgroundFor pkm.typing
            , SharedStyles.badgeStyle
            , if isVisible then
                Css.batch []

              else
                display none
            , position relative -- to make zIndex work
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
                , height (rem 6)
                ]
            ]
            []
        , div [] [ text pkm.fullName ]
        ]
