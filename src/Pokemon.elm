module Pokemon exposing
    ( Pokemon
    , fromCSVRows
    )

import Ability exposing (Ability)
import Css exposing (..)
import Pokemon.CSVRow exposing (PokemonCSVRow)
import String.Extra as String
import StringHelpers as String
import Type exposing (Type(..), Typing(..))


type alias Pokemon =
    { id : Int
    , fullName : String
    , typing : Typing
    , ability : Maybe Ability
    , imageUrl : String
    , evolvesFromIDs : List Int
    , evolvesFromDetails : Maybe String
    , transformGroupID : Maybe Int
    , transformGroupDetails : Maybe String
    }


fromCSVRows : List PokemonCSVRow -> List Pokemon
fromCSVRows pkmCSVRows =
    List.map fromCSVRow pkmCSVRows


fromCSVRow : PokemonCSVRow -> Pokemon
fromCSVRow pkm =
    let
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
    , ability = pkm.ability
    , imageUrl = "https://img.pokemondb.net/sprites/" ++ imageGen ++ "/normal/" ++ imageID ++ ".png"
    , evolvesFromIDs = pkm.evolvesFromID
    , evolvesFromDetails = pkm.evolutionDetails
    , transformGroupID = pkm.transformGroupID
    , transformGroupDetails = pkm.transformGroupDetails
    }
