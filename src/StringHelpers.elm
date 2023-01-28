module StringHelpers exposing (..)

import List exposing (foldl)


removeAll : List String -> String -> String
removeAll allToRmove str =
    foldl (\toRemove -> String.replace toRemove "") str allToRmove


replaceAll : List ( String, String ) -> String -> String
replaceAll allToReplace str =
    foldl (\( toReplace, replacement ) -> String.replace toReplace replacement) str allToReplace


removeTrailingDot : String -> String
removeTrailingDot str =
    if String.endsWith "." str then
        String.dropRight 1 str

    else
        str
