module Pokemon.List exposing (view)

import Helpers exposing (stopPropagationOnClick)
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (alt, attribute, class, src)
import Html.Keyed as Keyed
import Pokemon exposing (Pokemon)
import Pokemon.Details
import Type


view : List Pokemon -> (Pokemon -> Bool) -> Html Pokemon.Details.Msg
view pokemonList filter =
    Keyed.node "div" [ class "list" ] <|
        List.map (\p -> ( String.fromInt p.id, viewListElement (filter p) p )) pokemonList


viewListElement : Bool -> Pokemon -> Html Pokemon.Details.Msg
viewListElement isVisible pkm =
    div
        [ stopPropagationOnClick <| Pokemon.Details.Select pkm
        , Type.backgroundFor pkm.typing
        , class "item"
        , class "pokemonBadge"
        , if isVisible then
            class "visible"

          else
            class "hidden"
        ]
        [ img
            [ src pkm.imageUrl
            , alt pkm.fullName
            , attribute "loading" "lazy"
            ]
            []
        , text pkm.fullName
        ]
