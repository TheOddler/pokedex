module Pokemon.List exposing (view)

import Helpers exposing (stopPropagationOnClick)
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (alt, attribute, class, src)
import Html.Keyed as Keyed
import Html.Lazy as Lazy
import Pokemon exposing (Pokemon)
import Pokemon.Details
import Type


view : List Pokemon -> (Pokemon -> Bool) -> Html Pokemon.Details.Msg
view pokemonList filter =
    Keyed.node "div" [ class "list" ] <|
        List.map (\p -> ( String.fromInt p.id, viewListElementWrapper (filter p) p )) pokemonList


viewListElementWrapper : Bool -> Pokemon -> Html Pokemon.Details.Msg
viewListElementWrapper isVisible pkm =
    div
        [ class "itemWrapper" -- so the inner-item can be lazy
        , if isVisible then
            class "visible"

          else
            class "hidden"
        ]
        [ Lazy.lazy viewListElement pkm ]


viewListElement : Pokemon -> Html Pokemon.Details.Msg
viewListElement pkm =
    div
        [ stopPropagationOnClick <| Pokemon.Details.Select pkm
        , Type.backgroundFor pkm.typing
        , class "item"
        , class "pokemonBadge"
        ]
        [ img
            [ src pkm.imageUrl
            , alt pkm.fullName
            , attribute "loading" "lazy"
            ]
            []
        , text pkm.fullName
        ]
