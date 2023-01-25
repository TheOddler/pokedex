module Pokemon.List exposing (view)

import Css exposing (..)
import Css.Media as Media exposing (canHover, withMedia)
import Css.Transitions as Transitions exposing (cubicBezier, transition)
import Helpers exposing (stopPropagationOnClick)
import Html.Styled exposing (Html, div, img, text)
import Html.Styled.Attributes exposing (alt, attribute, css, src)
import Html.Styled.Keyed as Keyed
import Html.Styled.Lazy as Lazy
import Pokemon exposing (Pokemon)
import Pokemon.Details
import Pokemon.SharedStyles as SharedStyles
import Type


view : List Pokemon -> (Pokemon -> Bool) -> Html Pokemon.Details.Msg
view pokemonList filter =
    Keyed.node "div"
        [ css
            [ displayFlex
            , flexWrap wrap
            , justifyContent center
            , alignItems stretch
            ]
        ]
    <|
        List.map (\p -> ( String.fromInt p.id, viewListElementWrapper (filter p) p )) pokemonList


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


viewListElement : Pokemon -> Html Pokemon.Details.Msg
viewListElement pkm =
    div
        [ stopPropagationOnClick <| Pokemon.Details.Select pkm
        , css <|
            [ Type.backgroundFor pkm.typing
            , SharedStyles.badgeStyle
            , position relative -- to make zIndex work
            , height (pct 100)
            , paddingTop (rem 0.5)
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
            , alt pkm.fullName
            , attribute "loading" "lazy"
            , css
                [ width (rem 6)
                , height (rem 6)
                , property "object-fit" "contain"
                ]
            ]
            []
        , div [] [ text pkm.fullName ]
        ]
