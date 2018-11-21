module Types exposing (Type, parse)

import Csv
import Maybe.Extra
import Dict exposing (Dict, fromList)
import Element exposing (Color, rgb255)

type alias Type =
    { name: String
    , color: Color
    , effectiveness: Dict Int Float
    }


parse : String -> String -> Dict Int Type
parse typesCsvString typeEffectivenessCsvString = 
    let
        effectiveness =
            Csv.parse typeEffectivenessCsvString
            |> .records
            |> List.filterMap parseEffectiveness
        types = 
            Csv.parse typesCsvString
            |> .records
            |> List.filterMap (parseType effectiveness)
    in
        fromList types


-- Helper functions


parseType : List (Int, Int, Float) -> List String -> Maybe (Int, Type)
parseType effectiveness csv = 
    case csv of
        idString::identifier::_ ->
            case String.toInt idString of
                Just id -> 
                    let
                        myEffectiveness = 
                            effectiveness
                            |> List.filter (\(sourceId, _, damageFactor) -> sourceId == id && damageFactor /= 100)
                            |> List.map (\(_, targetId, damageFactor) -> (targetId, damageFactor))
                            |> Dict.fromList
                        type_ = {name = identifier, color = toColor identifier, effectiveness = myEffectiveness}
                    in 
                        Just (id, type_)
                Nothing -> Nothing
        other -> Debug.log ("Failed parse type: " ++ String.join "," other) Nothing


parseEffectiveness : List String -> Maybe (Int, Int, Float)
parseEffectiveness csv =
    case csv of
        damageTypeIdString::targetTypeIdString::damageFactorString::_ ->
            case (String.toInt damageTypeIdString, String.toInt targetTypeIdString, String.toFloat damageFactorString) of
                (Just damageTypeId, Just targetTypeId, Just damageFactor) ->
                    Just (damageTypeId, targetTypeId, damageFactor / 100)
                _ -> Nothing
        _ -> Nothing



toColor : String -> Color
toColor name =
    case name of
        "normal" -> rgb255 168 167 122
        "fighting" -> rgb255 194 46 40
        "flying" -> rgb255 169 143 243
        "poison" -> rgb255 163 62 161
        "ground" -> rgb255 226 191 101
        "rock" -> rgb255 182 161 54
        "bug" -> rgb255 166 185 26
        "ghost" -> rgb255 115 87 151
        "steel" -> rgb255 183 183 206
        "fire" -> rgb255 238 129 48
        "water" -> rgb255 99 144 240
        "grass" -> rgb255 122 199 76
        "electric" -> rgb255 247 208 44
        "psychic" -> rgb255 249 85 135
        "ice" -> rgb255 150 217 214
        "dragon" -> rgb255 111 53 252
        "dark" -> rgb255 112 87 70
        "fairy" -> rgb255 214 133 173
        "unknown" -> rgb255 255 255 255
        "shadow" -> rgb255 20 20 20
        _ -> rgb255 255 255 255
