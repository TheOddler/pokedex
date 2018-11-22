module Pokemon exposing (Pokemon, parse, view, viewDetail)

import Dict exposing (..)
import Csv
import Maybe.Extra exposing (values)
import List.Extra exposing (last)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Html.Attributes

import Types exposing (Type)

type alias Pokemon =
    { id: Int
    , name: String
    , types: List Int
    }


parse : String -> String -> List Pokemon
parse pokemonCsvString pokemonToTypesMappingCsvString = 
    let
        pokemonToTypesMapping = parsePokemonToTypeMappingsCsvString pokemonToTypesMappingCsvString
    in
        Csv.parse pokemonCsvString |> .records |> List.filterMap (parsePokemon pokemonToTypesMapping)


view : (Pokemon -> msg) -> Dict Int Type -> Pokemon -> Element msg
view onClick types pkm = 
    column 
        [ Border.rounded 10
        , width <| px 112
        , height <| px 130
        , Events.onClick <| onClick pkm
        , Background.gradient
            { angle = pi / 2
            , steps = List.filterMap (\i -> Dict.get i types) pkm.types |> List.map .color |> List.concatMap (\c -> [c, c])
            --, steps = 
            --    List.filterMap (\i -> Dict.get i types) pkm.types
            --    |> List.concatMap (\t -> Dict.keys t.effectiveness)
            --    |> List.filterMap (\i -> Dict.get i types)
            --    |> List.map .color
            }
        , mouseOver 
            [ scale 1.5
            ]
        ]
        [ image 
            [ centerX
            , height (shrink |> minimum 96)
            , width (shrink |> minimum 96)
            ]
            { src = imageUrl pkm
            , description = pkm.name
            }
        , el [centerX] <| text pkm.name
        ]


viewDetail : Dict Int Type -> Pokemon -> Element msg
viewDetail types pkm =
    column 
        [ Border.rounded 10
        , Background.gradient
            { angle = pi / 2
            , steps = List.filterMap (\i -> Dict.get i types) pkm.types |> List.map .color |> List.concatMap (\c -> [c, c])
            }
        , padding 10
        ]
        [ image 
            [ centerX
            , height (shrink |> minimum 96)
            , width (shrink |> minimum 96)
            ]
            { src = imageUrl pkm
            , description = pkm.name
            }
        , el [centerX] <| text pkm.name
        , row 
            [ spacing 5
            ]
            ( calcTotalEffectivenessAgainst pkm types
            |> List.map viewTypeEffectivenessBadge
            )
        ]


-- Helper functions


calcTotalEffectivenessAgainst : Pokemon -> Dict Int Type -> List (Type, Float)
calcTotalEffectivenessAgainst pkm types =
    let
        effectivenessAgainstType : Int -> Type -> Float
        effectivenessAgainstType targetId source = Dict.get targetId source.effectiveness |> Maybe.withDefault 1

        typesEffectivenessAgainstType : Int -> List (Type, Float)
        typesEffectivenessAgainstType targetId = List.map (\t -> (t, effectivenessAgainstType targetId t)) (Dict.values types) |> List.filter (\(_, f) -> f /= 1)

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


viewTypeEffectivenessBadge : (Type, Float) -> Element msg
viewTypeEffectivenessBadge (type_, effectivenesss) =
    let
        beautify f = 
            if (abs (f - 0.5) < 0.001) then "½"
            else if (abs (f - 0.25) < 0.001) then "¼"
            else String.fromFloat f
    in
        row [ Background.color type_.color
            , padding 5
            , spacing 5
            , Border.rounded 10
            ]
            [ text type_.name
            , text <| beautify effectivenesss
            ]


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
        id::identifier::_ ->
            case String.toInt id of
                Just i -> Just 
                    { id = i
                    , name = identifier
                    , types = 
                        pokemonToTypesMapping 
                        |> List.filter (\(p, t) -> p == i)
                        |> List.map (\(p, t) -> t)
                    }
                Nothing -> Nothing
        other -> Nothing


imageUrl : Pokemon -> String
imageUrl pkm = "https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/" ++ (String.fromInt pkm.id) ++ ".png"
