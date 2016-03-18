module Type where

import Json.Decode exposing (Decoder, (:=), int, string, list, object3)
import Html exposing (..)
import Html.Attributes exposing (class, style, src)
import Http
import HttpExt
import Effects
import Dict exposing (Dict)

import NamedAPIResource exposing (NamedAPIResource)

type alias Type =
    { id: Int
    , name: String
    , damage_relations: DamageRelations
    }

type alias DamageRelations =
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

typeRelationsDecoder : Decoder DamageRelations
typeRelationsDecoder =
    object3 DamageRelations
        ("no_damage_from" := list NamedAPIResource.decoder)
        ("half_damage_from" := list NamedAPIResource.decoder)
        ("double_damage_from" := list NamedAPIResource.decoder)



viewDamageRelationsOfList : List Type -> Html.Html
viewDamageRelationsOfList types
    = viewDamageRelations <| combineDamageRelationsOfTypes types

viewDamageRelations : DamageRelations -> Html.Html
viewDamageRelations dr =
    let chart = damageRelationsToMultiplyers dr
        withoutNeutral = Dict.filter (\_ d -> d /= 1) chart
        damageHtmls = Dict.foldr (\t d l -> damageToDiv t d :: l ) [] withoutNeutral
        --damageTrs = Dict.foldr (\t d l -> damageToTr t d :: l ) [] withoutNeutral
    in  div [ class "damageChart" ] damageHtmls
        --table [ class "damageChart" ] damageTrs

damageToTr : String -> Float -> Html.Html
damageToTr typeName dam =
    tr  [ class typeName ]
        [ td [] [ text typeName ]
        , td [] [ text <| toString dam ]
        ]

damageToDiv : String -> Float -> Html.Html
damageToDiv typeName dam =
    div [ class typeName ]
        [ div [] [ text typeName ]
        , div [] [ text <| toString dam ]
        ]

combineDamageRelationsOfTypes : List Type -> DamageRelations
combineDamageRelationsOfTypes types =
    { no_damage_from = List.concatMap (\t -> t.damage_relations.no_damage_from) types
    , half_damage_from = List.concatMap (\t -> t.damage_relations.half_damage_from) types
    , double_damage_from = List.concatMap (\t -> t.damage_relations.double_damage_from) types
    }

damageRelationsToMultiplyers : DamageRelations -> Dict String Float
damageRelationsToMultiplyers relations =
    let no = List.foldr (updateDamageDict 0) Dict.empty relations.no_damage_from
        withHalf = List.foldr (updateDamageDict 0.5) no relations.half_damage_from
        withDouble = List.foldr (updateDamageDict 2) withHalf relations.double_damage_from
    in withDouble

updateDamageDict : Float -> NamedAPIResource -> Dict String Float -> Dict String Float
updateDamageDict mult resource dict =
    let update dam =
        case dam of
            Just dam -> Just (dam * mult)
            Nothing -> Just mult
    in Dict.update resource.name update dict

fetch : String -> (Result Http.Error Type -> a) -> Effects.Effects a
fetch name callback = HttpExt.fetch decoder ("http://pokeapi.co/api/v2/type/" ++ name) callback
