module Pokemon exposing (Pokemon, parse, view, viewDetail)

import Html exposing (Html, div, ul, li, text, img, figure, figcaption)
import Html.Attributes exposing (class, src, style)
import Html.Events exposing (on)
import Dict exposing (Dict)
import Csv
import Maybe.Extra exposing (values)
import List.Extra exposing (last)
import String.Extra exposing (toTitleCase)

import Types exposing (Type, viewBadge, totalEffectivenessAgainst, idsToTypes, backgroundFor)

type alias Pokemon =
    { id: Int
    , speciesId: Int
    , name: String
    , types: List Int
    }


parse : String -> String -> List Pokemon
parse pokemonCsvString pokemonToTypesMappingCsvString = 
    let
        pokemonToTypesMapping = parsePokemonToTypeMappingsCsvString pokemonToTypesMappingCsvString
    in
        Csv.parse pokemonCsvString |> .records |> List.filterMap (parsePokemon pokemonToTypesMapping)


view : Dict Int Type -> Pokemon -> Html msg
view allTypes pkm =
    figure 
        [ backgroundFor <| idsToTypes allTypes pkm.types
        , class "pokemon"
        ]
        [ img
            [ src <| imageUrl pkm
            ] []
        , figcaption 
            [ class "name"
            ]
            [ text pkm.name
            ]
        ]


viewDetail : Dict Int Type -> Pokemon -> Html msg
viewDetail allTypes pkm =
    let
        viewBadgeWE (t, e) = viewBadge t (Just e)
        viewBadgeWoE t = viewBadge t Nothing
        types = idsToTypes allTypes pkm.types
    in
        div
            [ class "details"
            , backgroundFor types
            ]
            [ figure 
                [ class "pokemon"
                ]
                [ img
                    [ src <| imageUrl pkm
                    ] []
                , figcaption 
                    [ class "name"
                    ] 
                    [ text pkm.name
                    ]
                ]
            , div [ class "typeChart" ]
                <| List.map viewBadgeWoE types
            , div [ class "damageChart" ]
                <| List.map viewBadgeWE <| totalEffectivenessAgainst pkm.types allTypes
            ]


-- Helper functions


parsePokemonToTypeMappingsCsvString : String -> List (Int, Int)
parsePokemonToTypeMappingsCsvString csv =
    Csv.parse csv
    |> .records
    |> List.filterMap parsePokemonToTypeMapping


parsePokemonToTypeMapping : List String -> Maybe (Int, Int)
parsePokemonToTypeMapping mapping = -- pokemon_id,type_id,slot
    case mapping of
        pokemonIdString::typeIdString::_ -> 
            case (String.toInt pokemonIdString, String.toInt typeIdString) of
                (Just pokemonId, Just typeId) -> Just (pokemonId, typeId)
                _ -> Nothing
        _ -> Nothing


parsePokemon : List (Int, Int) -> List String -> Maybe Pokemon
parsePokemon pokemonToTypesMapping csv = 
    case csv of
        idStr::identifier::speciesIdStr::_ ->
            case (String.toInt idStr, String.toInt speciesIdStr) of
                (Just id, Just speciesId) -> Just 
                    { id = id
                    , speciesId = speciesId
                    , name = toTitleCase identifier
                    , types = 
                        pokemonToTypesMapping 
                        |> List.filter (\(p, t) -> p == id)
                        |> List.map (\(p, t) -> t)
                    }
                _ -> Nothing
        _ -> Nothing


imageUrl : Pokemon -> String
imageUrl pkm =
    let
        base = "https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/"
    in
        if String.endsWith "-alola" pkm.name then base ++ (String.fromInt pkm.speciesId) ++ "-alola.png"
        else base ++ (String.fromInt pkm.id) ++ ".png"
