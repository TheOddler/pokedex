module Pokemon.Mode exposing (..)


type Mode
    = TypeEffectiveness
    | Evolutions


toString : Mode -> String
toString mode =
    case mode of
        TypeEffectiveness ->
            "TypeEffectiveness"

        Evolutions ->
            "Evolutions"


fromString : String -> Mode
fromString str =
    case str of
        "TypeEffectiveness" ->
            TypeEffectiveness

        _ ->
            Evolutions
