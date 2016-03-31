import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Http
import HttpExt
import Effects exposing (Effects, Never)
import Task exposing (Task)
import Json.Decode exposing (Decoder)
import StartApp
import Async exposing (..)
import Window
import Dict exposing (Dict)
import Keyboard
import Char
import String
import Set

import PokemonTable
import Pokemon exposing (..)
import Type exposing (..)

type alias Model =
    { pokemonTable: Async PokemonTable.Model
    , selectedPokemon: Maybe String
    , searchString: String
    , pokemonCache: Dict String Pokemon
    , typeCache: Dict String Type
    }

initModel : Model
initModel =
    { pokemonTable = Requested
    , selectedPokemon = Nothing
    , searchString = ""
    , pokemonCache = Dict.empty
    , typeCache = Dict.empty
    }

init : (Model, Effects Action)
--init = (initModel, Effects.none)
init = (initModel, PokemonTable.fetch OnPokemonTableLoaded)

inputs : List (Signal Action)
inputs =
    [ Signal.map (\down -> if down then DeselectPokemon else NoAction) (Keyboard.isDown 27) --27 = escape
    ]

type Action = NoAction
            | OnPokemonTableLoaded (Result Http.Error PokemonTable.Model)
            | SelectPokemon String
            | DeselectPokemon
            | ChangeSearchString String
            | OnPokemonLoaded String (Result Http.Error Pokemon)
            | OnTypeLoaded String (Result Http.Error Type)

update : Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        NoAction -> (model, Effects.none)
        OnPokemonTableLoaded result ->
            case result of
                Ok list -> ({ model | pokemonTable = Finished list }, Effects.none)
                Err msg ->
                    let temp = Debug.log "Failed loading Pokémon table" msg
                    in ({ model | pokemonTable = Error ("Failed loading Pokémon: " ++ toString msg) }, Effects.none)
        SelectPokemon name ->
            let alreadySelected =
                case model.selectedPokemon of
                    Just selName -> selName == name
                    Nothing -> False
            in
                if alreadySelected || (Dict.member name model.pokemonCache)
                    then ({ model | selectedPokemon = Just name}, Effects.none)
                    else ({ model | selectedPokemon = Just name}, Pokemon.fetch name (OnPokemonLoaded name))
        DeselectPokemon -> ({ model | selectedPokemon = Nothing}, Effects.none)
        ChangeSearchString string -> ({ model | searchString = Debug.log "Change" string}, Effects.none)
        OnPokemonLoaded name result ->
            case result of
                Ok pmon ->
                    let missingTypesEffects = Pokemon.getMissingTypesEffect model.typeCache pmon OnTypeLoaded
                    in ({ model | pokemonCache = Dict.insert name pmon model.pokemonCache }, missingTypesEffects)
                Err msg ->
                    let temp = Debug.log "Failed loading Pokémon" msg
                    in ({ model | selectedPokemon = Nothing}, Effects.none)
        OnTypeLoaded name result ->
            case result of
                Ok type' -> ({ model | typeCache = Dict.insert name type' model.typeCache }, Effects.none)
                Err msg ->
                    let temp = Debug.log "Failed loading Type" msg
                    in ({ model | selectedPokemon = Nothing}, Effects.none)

view : Signal.Address Action -> Model -> Html
view address model =
    div []
        [ case model.selectedPokemon of
            Just name ->
                case Dict.get name model.pokemonCache of
                    Just pmon -> Pokemon.viewDetail pmon model.typeCache address DeselectPokemon
                    Nothing -> div [ class "loadingPokemonMessage" ] [ text "Loading Pokémon, please wait..." ]
            Nothing -> div [ class "pokemonSelectInfo" ] [ text "Click on a Pokémon to select it, or search at the bottom of the screen." ]
        , case model.pokemonTable of
            NotRequested -> div [] [ text "Nothing here :(" ]
            Requested -> div [class "pokemonTableLoadingMessage"] [ text "Loading Pokémon, please wait..." ]
            Finished pmonTable -> PokemonTable.viewWithSelect address pmonTable model.searchString SelectPokemon
            Error msg -> div [] [ text msg ]
        , input
            [ class "pokemonSearchString"
            , id "pokemonSearch"
            , placeholder "Search for a Pokémon..."
            , value model.searchString
            , on "input" targetValue (Signal.message <| Signal.forwardTo address ChangeSearchString)
            ] []
        ]

app : StartApp.App Model
app = StartApp.start { init = init, view = view, update = update, inputs = inputs }

main : Signal.Signal Html.Html
main = app.html

port tasks : Signal (Task.Task Never ())
port tasks = app.tasks
