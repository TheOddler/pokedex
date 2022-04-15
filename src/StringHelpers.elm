module StringHelpers exposing (..)

import List exposing (foldl)


removeAll : List String -> String -> String
removeAll allToRmove str =
    foldl (\toRemove -> String.replace toRemove "") str allToRmove


replaceAll : List ( String, String ) -> String -> String
replaceAll allToReplace str =
    foldl (\( toReplace, replacement ) -> String.replace toReplace replacement) str allToReplace
