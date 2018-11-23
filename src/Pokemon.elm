module Pokemon exposing (Pokemon, parse, view, viewDetail)

import Html exposing (Html, text, img, figure, figcaption)
import Html.Attributes exposing (class, src, attribute, style)
import Dict exposing (Dict)
import Csv
import Maybe.Extra exposing (values)
import List.Extra exposing (last)

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


view : (Pokemon -> msg) -> Dict Int Type -> Pokemon -> Html msg
view onClick allTypes pkm = 
    let
        types = List.filterMap (\t -> Dict.get t allTypes) pkm.types
        background =
            case types of
                [ one ] -> style "background-color" one.color
                many -> style "background-image" <| "linear-gradient(to right, " ++ String.join "," (List.map .color types |> List.concatMap (\c -> [c, c])) ++ ")"
    in
        figure 
            [ background
            , class "pokemon"
            ]
            [ img   [ src <| imageUrl pkm
                    , attribute "onerror" "this.onerror=null;this.src='images/missing-image.png';"
                    ] []
            , figcaption [] [ text pkm.name ]
            ]


viewDetail : Dict Int Type -> Pokemon -> Html msg
viewDetail allTypes pkm =
    text pkm.name
    -- column 
    --     [ Border.rounded 10
    --     , Background.gradient
    --         { angle = pi / 2
    --         , steps = List.filterMap (\i -> Dict.get i allTypes) pkm.types |> List.map .color |> List.concatMap (\c -> [c, c])
    --         }
    --     , padding 10
    --     ]
    --     [ image 
    --         [ centerX
    --         , height (shrink |> minimum 96)
    --         , width (shrink |> minimum 96)
    --         ]
    --         { src = imageUrl pkm
    --         , description = pkm.name
    --         }
    --     , el [centerX] <| text pkm.name
    --     , row 
    --         [ spacing 5
    --         ]
    --         ( calcTotalEffectivenessAgainst pkm allTypes
    --         |> List.map viewTypeEffectivenessBadge
    --         )
    --     ]


-- Helper view functions


viewTypeEffectivenessBadge : (Type, Float) -> Html msg
viewTypeEffectivenessBadge (type_, effectivenesss) =
    text <| type_.name ++ String.fromFloat effectivenesss
    -- let
    --     beautify f = 
    --         if (abs (f - 0.5) < 0.001) then "½"
    --         else if (abs (f - 0.25) < 0.001) then "¼"
    --         else String.fromFloat f
    -- in
    --     row [ Background.color type_.color
    --         , padding 5
    --         , spacing 5
    --         , Border.rounded 10
    --         ]
    --         [ text type_.name
    --         , text <| beautify effectivenesss
    --         ]


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
