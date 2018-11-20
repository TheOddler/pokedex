module Pokemon exposing (Pokemon, parse, view)

import Dict exposing (..)
import Csv
import Maybe.Extra exposing (values)
import List.Extra exposing (last)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html.Attributes

import Types exposing (Type)
import Helpers exposing (..)

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


view : Dict Int Type -> Pokemon -> Element msg
view types pkm = 
    column 
        [ Border.rounded 10
        , Background.gradient
            { angle = pi / 2
            , steps = List.filterMap (\i -> Dict.get i types) pkm.types |> List.map .color
            }
        , mouseOver 
            [ scale 1.5
            ]
        ]
        [ image 
            [ centerX
            , height (fill |> minimum 96)
            , width (fill |> minimum 96)
            ]
            { src = imageUrl pkm
            , description = pkm.name
            }
        , el [centerX] <| text pkm.name
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
        id::identifier::_ ->
            case parseId id of
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
