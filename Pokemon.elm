module Pokemon where

import Json.Decode exposing (Decoder, (:=), int, string, list, object2, object4, object8)
import Html exposing (..)
import Html.Attributes exposing (class, style, src)
import DecodeExt exposing (nullOr)
import Http
import HttpExt
import Effects
import Dict exposing (Dict)
import Type exposing (Type)
import MaybeExt

import NamedAPIResource exposing (NamedAPIResource)

type alias Pokemon =
    { id: Int
    , name: String
    , sprites: PokemonSprites
    , typeSlots: List TypeSlot
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

type alias TypeSlot =
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

typeDecoder : Decoder TypeSlot
typeDecoder =
    object2 TypeSlot
        ("slot" := int)
        ("type" := NamedAPIResource.decoder)

view : Pokemon -> Dict String Type -> Html.Html
view pmon typeCache =
    div [ class "pokemonDetail" ]
        [ img [ src <| Maybe.withDefault "images/missing-image.png" pmon.sprites.front_default ] []
        , div [ class "info" ]
            [ div [ class "name" ] [ text pmon.name ]
            , ul [ class "types" ] <| List.map typeSlotToLi pmon.typeSlots
            ]
        , div [ class "damageChartWrapper" ]
            [ div [class "title" ] [ text "Damage taken:" ]
            , viewDamagesTaken typeCache pmon.typeSlots
            ]
        ]

typeSlotToLi : TypeSlot -> Html.Html
typeSlotToLi t =
    li [ class t.typeResource.name ] [ text t.typeResource.name ]

viewDamagesTaken : Dict String Type -> List TypeSlot -> Html.Html
viewDamagesTaken typeCache tss =
    let maybeTypes = MaybeExt.allOf <| List.map (\t -> Dict.get t.typeResource.name typeCache) tss
    in case maybeTypes of
        Just types -> Type.viewDamageRelationsOfList types
        Nothing -> div [] [ text "Loading type data..." ]

getMissingTypes : Dict String Type -> Pokemon -> List TypeSlot
getMissingTypes typeCache pmon = List.filter (\ts -> not <| Dict.member ts.typeResource.name typeCache) pmon.typeSlots

getMissingTypesEffect : Dict String Type -> Pokemon -> (String -> Result Http.Error Type -> a) -> Effects.Effects a
getMissingTypesEffect typeCache pmon callback =
    let missingTypes = getMissingTypes typeCache pmon
        effects = List.map (\ts -> Type.fetch ts.typeResource.name (callback ts.typeResource.name)) missingTypes
    in Effects.batch effects

fetch : String -> (Result Http.Error Pokemon -> a) -> Effects.Effects a
fetch name callback = HttpExt.fetch decoder ("http://pokeapi.co/api/v2/pokemon/" ++ name) callback
