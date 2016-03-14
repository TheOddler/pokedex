module NamedAPIResourceList where

import Json.Decode exposing (Decoder, (:=), int, string, list, object2, object4)
import DecodeExt exposing (nullOr)
import Html exposing (Html, div, img, text, ul, li, table, tr, td)
import Html.Attributes exposing (class, style, src)
import Html.Events exposing (onClick)
import Http
import HttpExt
import ListExt
import Effects

type alias NamedAPIResourceList =
    { count: Int
    , next: Maybe String
    , previous: Maybe String
    , results: List NamedAPIResource
    }

type alias NamedAPIResource =
    { name: String
    , url: String
    }

empty : NamedAPIResourceList
empty =
    { count = 0
    , next = Nothing
    , previous = Nothing
    , results = []
    }

decoder : Decoder NamedAPIResourceList
decoder =
    object4 NamedAPIResourceList
        ("count" := int)
        ("next" := nullOr string)
        ("previous" := nullOr string)
        ("results" := list resourceDecoder)

resourceDecoder : Decoder NamedAPIResource
resourceDecoder =
    object2 NamedAPIResource
        ("name" := string)
        ("url" := string)

viewWithSelect : Signal.Address a -> (String -> a) -> NamedAPIResourceList -> Html
viewWithSelect address select resourceList =
    div []
        [ table [ class "pokemonTable" ] <| List.map (toTr address select) (ListExt.split 10 resourceList.results)
        ]

toTr address select resources =
    tr [] <| List.map (toTd address select) resources

toTd address select resource =
    td  [ onClick address (select resource.name)
        ]
        [ text resource.name
        ]

resourceAsLI : Signal.Address a -> (String -> a) -> NamedAPIResource -> Html
resourceAsLI address select resource =
    li  [ onClick address (select resource.name)
        ]
        [ text resource.name
        ]

fetchPokemonList : (Result Http.Error NamedAPIResourceList -> a) -> Effects.Effects a
fetchPokemonList callback = HttpExt.fetch decoder "http://pokeapi.co/api/v2/pokemon-species/?limit=100000" callback --100000 to get all
