module Pokemon exposing (..)

import Dict exposing (..)
import Csv exposing (Csv, parse)
import Maybe.Extra exposing (values)
import List.Extra exposing (last)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html.Attributes

import PokemonCsvString exposing (..)
import PokemonTypesCsvString exposing (..)
import Types exposing (..)
import Helpers exposing (..)

type alias Pokemon =
    { id: Int
    , name: String
    , types: List Int
    }

allPokemon : List Pokemon
allPokemon = 
    let
        pokemonCsv = parse pokemonCsvString
        pokemonTypesCsv = parse pokemonTypesCsvString
    in
        List.map parsePokemon pokemonCsv.records
        |> values

parsePokemon : List String -> Maybe Pokemon
parsePokemon csv = 
    case csv of
        id::identifier::_ ->
            case parseId id of
                Just i -> Just 
                    { id = i
                    , name = identifier
                    , types = [1, 2]
                    }
                Nothing -> Debug.log ("Incorrect id: " ++ id) Nothing
        other -> Debug.log ("Wrong pokemon csv line: " ++ String.join "," other) Nothing

typesFor : Int -> Csv -> List Type
typesFor id csv =
    let 
        idString = String.fromInt id
        parse typeArr =
            case typeArr of
                pokemonId::typeID::slot -> Just { pokemonId = pokemonId, typeID = typeID, slot = slot }
                _ -> Nothing
        pokemonTypes = List.filterMap parse csv.records -- pokemon_id, type_id, slot
        thisPokemonTypes = List.filter (\p -> p.pokemonId == idString) pokemonTypes
    in
        thisPokemonTypes
        |> List.sortBy .slot
        |> List.map .typeID
        |> List.map String.toInt
        |> List.map (Maybe.withDefault -1)
        |> List.filterMap (\i -> Dict.get i allTypes)

view : Pokemon -> Element msg
view pkm = 
    column 
        [ Border.rounded 10
        , Background.gradient
            { angle = pi / 2
            , steps = List.map .color (typesFor pkm.id (parse pokemonTypesCsvString))
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

imageUrl : Pokemon -> String
imageUrl pkm = "https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/" ++ (String.fromInt pkm.id) ++ ".png"
