module Pokemon exposing
    ( Pokemon
    , fromCSVRows
    )

import Css exposing (..)
import Pokemon.CSVRow exposing (PokemonCSVRow)
import String.Extra as String
import StringHelpers as String
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
            Pokemon.CSVRow.effectivenessAgainst pkm

        isSuperEffective ( _, eff ) =
            eff >= 1.5

        isNotVeryEffective ( _, eff ) =
            eff < 0.75

        imageIDCleanup =
            String.removeAll [ ".", "'", ":", "%" ]
                << String.replaceAll [ ( " ", "-" ), ( "♀", "f" ), ( "♂", "m" ) ]
                << String.removeAccents
                << String.toLower

        imageGen =
            case pkm.imageGen of
                Just imgGen ->
                    imgGen

                Nothing ->
                    case pkm.alternateFormName of
                        Just "Hisuian" ->
                            "legends-arceus"

                        _ ->
                            "home"

        imageID =
            case pkm.imageID of
                Just imgID ->
                    imgID

                Nothing ->
                    let
                        baseName =
                            imageIDCleanup pkm.name

                        variantSuffix =
                            case pkm.alternateFormName of
                                Nothing ->
                                    ""

                                Just alternateFormName ->
                                    "-" ++ imageIDCleanup alternateFormName
                    in
                    baseName ++ variantSuffix
    in
    { id = pkm.id
    , fullName =
        case pkm.fullName of
            Just fullName ->
                fullName

            Nothing ->
                case pkm.alternateFormName of
                    Just alternateFormName ->
                        alternateFormName ++ " " ++ pkm.name

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
    , imageUrl = "https://img.pokemondb.net/sprites/" ++ imageGen ++ "/normal/" ++ imageID ++ ".png"
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
