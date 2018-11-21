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
        , row []
            ( calcTotalEffectiveness types pkm
            |> List.map viewTypeEffectivenessBadge
            )
        ]


calcTotalEffectiveness : Dict Int Type -> Pokemon -> List (Type, Float)
calcTotalEffectiveness types pkm =
    List.filterMap (\i -> Dict.get i types) pkm.types
    |> List.map .effectiveness
    |> List.concatMap Dict.toList
    |> List.map (\(i, f) -> (Dict.get i types, f))
    |> List.filterMap 
        ( \(mt, f) ->
            case mt of
                Just t -> Just (t, f)
                Nothing -> Nothing
        )


viewTypeEffectivenessBadge : (Type, Float) -> Element msg
viewTypeEffectivenessBadge (type_, effectivenesss) =
    el [] <| text (type_.name ++ String.fromFloat effectivenesss)


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
                Nothing -> Debug.log ("Incorrect id: " ++ id) Nothing
        other -> Debug.log ("Wrong pokemon csv line: " ++ String.join "," other) Nothing


imageUrl : Pokemon -> String
imageUrl pkm = "https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/" ++ (String.fromInt pkm.id) ++ ".png"
