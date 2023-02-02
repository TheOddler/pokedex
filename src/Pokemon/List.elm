module Pokemon.List exposing (view)

import Helpers exposing (stopPropagationOnClick)
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Keyed as Keyed
import Pokemon exposing (Pokemon)
import Pokemon.Details


view : List Pokemon -> (Pokemon -> Bool) -> Html Pokemon.Details.Msg
view pokemonList filter =
    Keyed.node "div" [ class "list" ] <|
        List.map (viewListElement filter) pokemonList


viewListElement : (Pokemon -> Bool) -> Pokemon -> ( String, Html Pokemon.Details.Msg )
viewListElement filter pkm =
    ( -- Unique id for the keyed
      String.fromInt pkm.id
    , Pokemon.viewBadge pkm
        [ stopPropagationOnClick <| Pokemon.Details.Select pkm
        , class "item"
        , if filter pkm then
            class "visible"

          else
            class "hidden"
        ]
    )
