import Html exposing (..)
import Html.Events exposing (onClick, on, onKeyPress, targetValue)
import Html.Attributes exposing (class, value, placeholder)
import Http
import HttpExt
import Effects exposing (Effects, Never)
import Task exposing (Task)
import Json.Decode exposing (Decoder)
import StartApp
import Async exposing (..)
import Window
import Dict exposing (Dict)

import PokemonTable
import Pokemon exposing (..)
import Type exposing (..)

type alias Model =
    { width: Int
    , pokemonTable: Async PokemonTable.Model
    , selectedPokemon: Maybe String
    , pokemonCache: Dict String Pokemon
    , typeCache: Dict String Type
    }

initModel : Model
initModel =
    { width = 0
    , pokemonTable = Requested
    , selectedPokemon = Nothing
    , pokemonCache = Dict.empty
    , typeCache = Dict.empty
    }

init : (Model, Effects Action)
--init = (initModel, Effects.none)
init = (initModel, PokemonTable.fetch OnPokemonTableLoaded)

inputs : List (Signal Action)
inputs =
    [ Signal.map SetWidth Window.width
    ]

type Action = NoAction
            | SetWidth Int
            | OnPokemonTableLoaded (Result Http.Error PokemonTable.Model)
            | SelectPokemon String
            | OnPokemonLoaded String (Result Http.Error Pokemon)
            | OnTypeLoaded String (Result Http.Error Type)

update : Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        NoAction -> (model, Effects.none)
        SetWidth w -> ({ model | width = w }, Effects.none)
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
        OnPokemonLoaded name result ->
            case result of
                Ok pmon ->
                    let missingTypes = List.filter (\t -> not <| Dict.member t.typeResource.name model.typeCache) pmon.types
                        typeLoadEffects = List.map (\t -> Type.fetch t.typeResource.name (OnTypeLoaded t.typeResource.name)) missingTypes
                    in ({ model | pokemonCache = Dict.insert name pmon model.pokemonCache }, Effects.batch typeLoadEffects)
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
                    Just pmon -> Pokemon.view pmon model.typeCache
                    Nothing -> div [] [ text "Loading Pokémon, please wait..." ]
            Nothing -> div [] [ text "Click on a Pokémon to select it." ]
        , case model.pokemonTable of
            NotRequested -> div [] [ text "Nothing here :(" ]
            Requested -> div [class "pokemonTableLoadingMessage"] [ text "Loading Pokémon, please wait..." ]
            Finished pmonTable -> PokemonTable.viewWithSelect address pmonTable SelectPokemon
            Error msg -> div [] [ text msg ]
        ]

app : StartApp.App Model
app = StartApp.start { init = init, view = view, update = update, inputs = [] }

main : Signal.Signal Html.Html
main = app.html

port tasks : Signal (Task.Task Never ())
port tasks = app.tasks
