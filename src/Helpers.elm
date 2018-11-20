module Helpers exposing (..)


parseId : String -> Maybe Int
parseId idString = 
    case String.toInt idString of 
        Just id -> Just id
        Nothing -> Debug.log ("Not found: " ++ idString) Nothing
