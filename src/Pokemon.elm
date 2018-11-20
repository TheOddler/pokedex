module Pokemon exposing (Pokemon, allPokemon, view)

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

typesFor : Int -> List Type
typesFor id =
    List.filter (\p -> p.pokemonId == id) pokemonTypes
    |> List.sortBy .slot
    |> List.map .typeID
    |> List.filterMap (\i -> Dict.get i allTypes)

pokemonTypes =
    let 
        parseType typeArr =
            case typeArr of
                pokemonIdString::typeIDString::slotString::_ ->
                    case (String.toInt pokemonIdString, String.toInt typeIDString, String.toInt slotString) of
                        (Just pokemonId, Just typeID, Just slot) -> Just { pokemonId = pokemonId, typeID = typeID, slot = slot }
                        _ -> Nothing
                _ -> Nothing
    in 
        List.filterMap parseType <| .records <| parse pokemonTypesCsvString

view : Pokemon -> Element msg
view pkm = 
    column 
        [ Border.rounded 10
        , Background.gradient
            { angle = pi / 2
            , steps = List.map .color (typesFor pkm.id)
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
