module Types exposing (..)

import Csv exposing (Csv, parse)
import Maybe.Extra
import Dict exposing (..)
import Element exposing (..)

import TypesCsvString exposing (..)
import Helpers exposing (..)

type alias Type =
    { name: String
    , color: Color
    }


allTypes : Dict Int Type
allTypes = 
    let
        csv = parse typesCsvString
        list = List.map parseCsv csv.records |> Maybe.Extra.values
    in
        fromList list


parseCsv : List String -> Maybe (Int, Type)
parseCsv csv = 
    case csv of
        id::identifier::_ ->
            parseId id
            |> Maybe.map (\i -> (i, {name = identifier, color = toColor identifier}))
        other -> Debug.log (String.join "," other) Nothing


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
