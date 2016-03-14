module NamedAPIResourceList where

import Json.Decode exposing (Decoder, (:=), int, string, list, object2, object4)
import DecodeExt exposing (..)
import Html exposing (Html, div, img, text, ul, li)
import Html.Attributes exposing (class, style, src)
import Html.Events exposing (onClick)

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
        [ ul [ class "pokemonList" ] <| List.map (resourceAsLI address select) resourceList.results
        ]

resourceAsLI : Signal.Address a -> (String -> a) -> NamedAPIResource -> Html
resourceAsLI address select resource =
    li  [ onClick address (select resource.name)
        ]
        [ text resource.name
        ]
