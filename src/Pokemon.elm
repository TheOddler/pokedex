module Pokemon exposing (Pokemon, parse, view, viewDetail)

import Html exposing (Html, div, ul, li, text, img, figure, figcaption)
import Html.Attributes exposing (class, src, style)
import Html.Events exposing (on)
import Dict exposing (Dict)
import Csv
import Maybe.Extra exposing (values)
import List.Extra exposing (last)
import String.Extra exposing (toTitleCase)

import Types exposing (Type, viewBadge, totalEffectivenessAgainst)

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
        [ backgroundFor pkm allTypes
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
        viewBedgeWE (t, e) = viewBadge t (Just e)
    in
        div
            [ class "details"
            , backgroundFor pkm allTypes
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
            , div [ class "damageChart" ]
                <| List.map viewBedgeWE <| totalEffectivenessAgainst pkm.types allTypes
            ]


-- Helper view functions


backgroundFor : Pokemon -> Dict Int Type -> Html.Attribute msg
backgroundFor pkm allTypes =
    let
        types = List.filterMap (\t -> Dict.get t allTypes) pkm.types
        duplicate c = [c, c]
    in
        style "background" <| "linear-gradient(to right, " ++ String.join "," (List.concatMap (.color >> duplicate) types) ++ ")"


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
