module Pokemon exposing (..)

import Csv exposing (Csv, parse)
import Maybe.Extra exposing (values)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html.Attributes

import PokemonCsvString exposing (..)

type alias Pokemon =
    { speciesId: Int
    , name: String
    }

pokemon : String -> Int -> Pokemon
pokemon name speciesId =
    { speciesId = speciesId
    , name = name
    }

allPokemon : List Pokemon
allPokemon = 
    let
        pokemonCsvData = parse pokemonCsvString
    in
        List.map parseCsv pokemonCsvData.records
        |> values

parseCsv : List String -> Maybe Pokemon
parseCsv csv = 
    case csv of
        id::identifier::species_id::_ ->
            parseId species_id
            |> Maybe.map (pokemon identifier)
        other -> Debug.log (String.join "," other) Nothing

parseId : String -> Maybe Int
parseId idString = 
    case String.toInt idString of 
        Just id -> Just id
        Nothing -> Debug.log ("Not found: " ++ idString) Nothing

view : Pokemon -> Element msg
view pkm = 
    column 
        [ Border.rounded 10
        , Background.color (rgb255 255 255 255)
        , mouseOver 
            [ scale 1.5
            ]
        ]
        [ image [centerX]
            { src = imageUrl pkm
            , description = pkm.name
            }
        , el [centerX] <| text pkm.name
        ]

imageUrl : Pokemon -> String
imageUrl pkm = "https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/" ++ (String.fromInt pkm.speciesId) ++ ".png"
