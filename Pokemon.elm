module Pokemon where

import Json.Decode exposing (Decoder, (:=), int, string, list, object2, object4, object8)
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (class, style, src)
import DecodeExt exposing (nullOr)
import Http
import HttpExt
import Effects
import Dict exposing (Dict)
import Type exposing (Type)

import NamedAPIResource exposing (NamedAPIResource)

type alias Pokemon =
    { id: Int
    , name: String
    , sprites: PokemonSprites
    , types: List PokemonType
    }

type alias PokemonSprites =
    { back_female: (Maybe String)
    , back_shiny_female: (Maybe String)
    , back_default: (Maybe String)
    , front_female: (Maybe String)
    , front_shiny_female: (Maybe String)
    , back_shiny: (Maybe String)
    , front_default: (Maybe String)
    , front_shiny: (Maybe String)
    }

type alias PokemonType =
    { slot: Int
    , typeResource: NamedAPIResource
    }

decoder : Decoder Pokemon
decoder =
    object4 Pokemon
        ("id" := int)
        ("name" := string)
        ("sprites" := spritesDecoder)
        ("types" := list typeDecoder)

spritesDecoder : Decoder PokemonSprites
spritesDecoder =
    object8 PokemonSprites
        ("back_female" := nullOr string)
        ("back_shiny_female" := nullOr string)
        ("back_default" := nullOr string)
        ("front_female" := nullOr string)
        ("front_shiny_female" := nullOr string)
        ("back_shiny" := nullOr string)
        ("front_default" := nullOr string)
        ("front_shiny" := nullOr string)

typeDecoder : Decoder PokemonType
typeDecoder =
    object2 PokemonType
        ("slot" := int)
        ("type" := NamedAPIResource.decoder)

view : Pokemon -> Dict String Type -> Html.Html
view pmon typeCache =
    div [ class "pokemonDetail" ]
        [ img [ src <| Maybe.withDefault "" pmon.sprites.front_default ] []
        , text pmon.name
        , text "Types: "
        , div [] <| List.map viewType pmon.types
        , text "Weaknesses: "
        , div [] <| List.map (viewWeaknesses typeCache) pmon.types
        ]

viewType : PokemonType -> Html.Html
viewType t =
    div [] [ text t.typeResource.name ]

viewWeaknesses : Dict String Type -> PokemonType -> Html.Html
viewWeaknesses typeCache t =
    let maybeType = Dict.get t.typeResource.name typeCache
    in case maybeType of
        Just type' -> Type.viewDamageRelationsOf type'
        Nothing -> div [] [ text "Loading type data..." ]

simpleView : Pokemon -> Html.Html
simpleView pokemon =
    div [ class "pokemonDetail" ]
        [ img [ src <| Maybe.withDefault "" pokemon.sprites.front_default ] []
        , text pokemon.name
        ]

fetch : String -> (Result Http.Error Pokemon -> a) -> Effects.Effects a
fetch name callback = HttpExt.fetch decoder ("http://pokeapi.co/api/v2/pokemon/" ++ name) callback
