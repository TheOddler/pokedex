module Pokemon exposing
    ( Pokemon
    , fromCSVRows
    )

import Css exposing (..)
import Html exposing (p)
import PokemonCSVRow exposing (PokemonCSVRow)
import Type exposing (Type(..), Typing(..))


type alias Pokemon =
    { id : Int
    , fullName : String
    , typing : Typing
    , superEffective : List ( Type, Float )
    , notVeryEffective : List ( Type, Float )
    , imageUrl : String
    , evolvesFromIDs : List Int
    , evolvesFromDetails : Maybe String
    , evolvesIntoIDs : List Int
    , transformGroupDetails : Maybe String
    , othersInTransformGroup : List Int
    }


fromCSVRows : List PokemonCSVRow -> List Pokemon
fromCSVRows pkmCSVRows =
    List.map (fromCSVRow pkmCSVRows) pkmCSVRows


fromCSVRow : List PokemonCSVRow -> PokemonCSVRow -> Pokemon
fromCSVRow pkmCSVRows pkm =
    let
        effectivenessList =
            PokemonCSVRow.effectivenessAgainst pkm

        isSuperEffective ( _, eff ) =
            eff >= 1.5

        isNotVeryEffective ( _, eff ) =
            eff < 0.75
    in
    { id = pkm.id
    , fullName =
        case pkm.alternateFormName of
            Just alternateFormName ->
                pkm.name ++ " (" ++ alternateFormName ++ ")"

            Nothing ->
                pkm.name
    , typing =
        case pkm.secondaryType of
            Just second ->
                Double pkm.primaryType second

            Nothing ->
                Single pkm.primaryType
    , superEffective = List.filter isSuperEffective effectivenessList
    , notVeryEffective = List.filter isNotVeryEffective effectivenessList
    , imageUrl = "https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/" ++ pkm.image ++ ".png"
    , evolvesFromIDs = pkm.evolvesFromID
    , evolvesFromDetails = pkm.evolutionDetails
    , evolvesIntoIDs = List.map .id <| List.filter (\p -> List.member pkm.id p.evolvesFromID) pkmCSVRows
    , transformGroupDetails = pkm.transformGroupDetails
    , othersInTransformGroup =
        case pkm.transformGroupID of
            Nothing ->
                []

            Just transformGroupID ->
                List.map .id <| List.filter (\p -> p.transformGroupID == Just transformGroupID && p.id /= pkm.id) pkmCSVRows
    }
