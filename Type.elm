module Type where

import Json.Decode exposing (Decoder, (:=), int, string, list, object3)
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (class, style, src)
import Http
import HttpExt
import Effects

import NamedAPIResource exposing (NamedAPIResource)

type alias Type =
    { id: Int
    , name: String
    , damage_relations: TypeRelations
    }

type alias TypeRelations =
    { no_damage_from: List NamedAPIResource
    , half_damage_from: List NamedAPIResource
    , double_damage_from: List NamedAPIResource
    }

decoder : Decoder Type
decoder =
    object3 Type
        ("id" := int)
        ("name" := string)
        ("damage_relations" := typeRelationsDecoder)

typeRelationsDecoder : Decoder TypeRelations
typeRelationsDecoder =
    object3 TypeRelations
        ("no_damage_from" := list NamedAPIResource.decoder)
        ("half_damage_from" := list NamedAPIResource.decoder)
        ("double_damage_from" := list NamedAPIResource.decoder)

viewDamageRelationsOf : Type -> Html.Html
viewDamageRelationsOf t =
    div []
        [ text "No damage from:"
        , div [] <| List.map (\a -> text a.name) t.damage_relations.no_damage_from
        , text "Half damage from:"
        , div [] <| List.map (\a -> text a.name) t.damage_relations.half_damage_from
        , text "Double damage from:"
        , div [] <| List.map (\a -> text a.name) t.damage_relations.double_damage_from
        ]

viewDamageRelationsOfList : List Type -> Html.Html
viewDamageRelationsOfList types
    = div [] <| List.map viewDamageRelationsOf types

fetch : String -> (Result Http.Error Type -> a) -> Effects.Effects a
fetch name callback = HttpExt.fetch decoder ("http://pokeapi.co/api/v2/type/" ++ name) callback
