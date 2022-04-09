module Pokemon exposing
    ( Msg(..)
    , Pokemon
    , Selected(..)
    , Settings
    , fromCSVRows
    , initSettings
    , updateSettings
    )

import Css exposing (..)
import Html exposing (p)
import LocalStorage exposing (LocalStorage)
import Maybe.Extra as Maybe
import Pokemon.Mode as Mode exposing (Mode, toString)
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


type Msg
    = Select Pokemon
    | Deselect
    | ChangeMode Mode


type Selected
    = Selected Pokemon
    | Deselected Pokemon


type alias Settings =
    { pokemon : Pokemon
    , mode : Mode
    , visible : Bool -- this allows us to always draw the view for nicer animations
    }


initSettings : LocalStorage -> Pokemon -> Settings
initSettings localSotrage first =
    { pokemon = first
    , mode = Maybe.withDefault Mode.Evolutions (Maybe.map Mode.fromString localSotrage.mode)
    , visible = False
    }


updateSettings : Msg -> Settings -> ( Settings, Cmd Msg )
updateSettings msg settings =
    case msg of
        Select p ->
            if settings.pokemon == p then
                updateSettings Deselect settings

            else
                ( { settings | pokemon = p, visible = True }, Cmd.none )

        Deselect ->
            ( { settings | visible = False }, Cmd.none )

        ChangeMode mode ->
            ( { settings | mode = mode }, saveMode mode )


saveMode : Mode -> Cmd Msg
saveMode mode =
    LocalStorage.save LocalStorage.modeKey <| toString mode


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
