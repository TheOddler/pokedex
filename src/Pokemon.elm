module Pokemon exposing (Pokemon, parse, view, viewDetail)

import Html exposing (Html, div, text, img, figure, figcaption)
import Html.Attributes exposing (class, src, style)
import Html.Events exposing (on)
import Dict exposing (Dict)
import Csv
import Maybe.Extra exposing (values)
import List.Extra exposing (last)
import String.Extra exposing (toTitleCase)

import Types exposing (Type)

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


view : (Pokemon -> msg) -> Dict Int Type -> Pokemon -> Html msg
view onClick allTypes pkm =
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


-- Helper view functions


viewTypeEffectivenessBadge : (Type, Float) -> Html msg
viewTypeEffectivenessBadge (type_, effectivenesss) =
    let
        beautify f = 
            if (abs (f - 0.5) < 0.001) then "½"
            else if (abs (f - 0.25) < 0.001) then "¼"
            else String.fromFloat f
    in
        div [ style "background-color" type_.color
            , class "effectivenessBadge"
            ]
            [ div [] [ text  type_.name ]
            , div [] [ text <| String.fromFloat effectivenesss ]
            ]


backgroundFor : Pokemon -> Dict Int Type -> Html.Attribute msg
backgroundFor pkm allTypes =
    let
        types = List.filterMap (\t -> Dict.get t allTypes) pkm.types
    in
        case types of
            [ one ] -> style "background-color" one.color
            many -> style "background-image" <| "linear-gradient(to right, " ++ String.join "," (List.map .color types |> List.concatMap (\c -> [c, c])) ++ ")"


-- Helper functions


calcTotalEffectivenessAgainst : Pokemon -> Dict Int Type -> List (Type, Float)
calcTotalEffectivenessAgainst pkm allTypes =
    let
        effectivenessAgainstType : Int -> Type -> Float
        effectivenessAgainstType targetId source = Dict.get targetId source.effectiveness |> Maybe.withDefault 1

        typesEffectivenessAgainstType : Int -> List (Type, Float)
        typesEffectivenessAgainstType targetId = List.map (\t -> (t, effectivenessAgainstType targetId t)) (Dict.values allTypes) |> List.filter (\(_, f) -> f /= 1)

        typesEffectivenessAgainstThisPokemon : List (Type, Float)
        typesEffectivenessAgainstThisPokemon = List.concatMap typesEffectivenessAgainstType pkm.types

        combineOrAdd : List (Type, Float) -> (Type, Float) -> List (Type, Float) -> List (Type, Float)
        combineOrAdd all toAdd existing =
            let
                (toAddT, toAddF) = toAdd
            in
                case List.any (\(t, _) -> t == toAddT) existing of
                    True -> existing
                    False -> (toAddT, List.product <| List.map (\(_, f) -> f) <| List.filter (\(t, _) -> t == toAddT) all) :: existing

        combined : List (Type, Float)
        combined = List.foldl (combineOrAdd typesEffectivenessAgainstThisPokemon) [] typesEffectivenessAgainstThisPokemon |> List.filter (\(_, f) -> f /= 1)
    in
        List.sortBy (\(_, f) -> -f) combined


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
