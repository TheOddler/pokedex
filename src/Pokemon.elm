module Pokemon exposing (Pokemon, parse, view, viewDetail)

import Html exposing (Html, div, ul, li, text, img, figure, figcaption)
import Html.Attributes exposing (class, src, style)
import Html.Events exposing (on)
import Dict exposing (Dict)
import Csv
import Maybe.Extra exposing (values)
import List.Extra exposing (last)
import Regex exposing (Regex, fromString, replace)
import String.Extra exposing (humanize, toTitleCase)

import Types exposing (Type, viewBadge, totalEffectivenessAgainst, idsToTypes, backgroundFor)

type alias Pokemon =
    { id: Int
    , speciesId: Int
    , identifier: String
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
            [ text <| beautyName pkm.identifier
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
                    [ text <| beautyName pkm.identifier
                    ]
                ]
            , div [ class "typeChart" ]
                <| List.map viewBadgeWoE types
            , div [ class "damageChart" ]
                <| List.map viewBadgeWE <| totalEffectivenessAgainst pkm.types allTypes
            ]


-- Helper functions


beautyName : String -> String
beautyName name =
    let
        female : Regex
        female = fromString "(\\bf\\b|\\bfemale\\b)" |> Maybe.withDefault Regex.never

        male : Regex
        male = fromString "(\\bm\\b|\\bmale\\b)" |> Maybe.withDefault Regex.never

        moveMega : String -> String
        moveMega n =
            if String.contains "-mega" n then "Mega " ++ String.replace "-mega" "" n
            else n
    in name
        |> replace female (always "♀")
        |> replace male (always "♂")
        |> moveMega
        |> humanize
        |> toTitleCase


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
                    , identifier = identifier
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
        totems = -- Special case totems to use normal image (there are more, but those have a proper image)
            [ 10128 -- Lurantis
            , 10129 -- Salazzle
            , 10146 -- Kommo-o
            , 10153 -- Araquanid
            , 10154 -- Togedemaru
            ]
    in
        -- special ovverride for some of the Pikachu
        if pkm.id >= 10080 && pkm.id <= 10085 then base ++ (String.fromInt pkm.id) ++ ".png"
        -- special ovverride for some totem pokemon (no image found, so use normal one)
        else if List.member pkm.id totems then base ++ (String.fromInt pkm.speciesId) ++ ".png"
        -- special override for Meowstic female
        else if pkm.identifier == "meowstic-female" then base ++ "female/" ++ (String.fromInt pkm.speciesId) ++ ".png"
        -- special forms
        else if pkm.id > 10000 && String.contains "-" pkm.identifier  then base ++ (String.fromInt pkm.speciesId) ++ "-" ++ String.Extra.rightOf "-" pkm.identifier ++ ".png"
        else base ++ (String.fromInt pkm.id) ++ ".png"
