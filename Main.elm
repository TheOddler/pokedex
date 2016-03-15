import Html exposing (Html, div, button, text, input)
import Html.Events exposing (onClick, on, onKeyPress, targetValue)
import Html.Attributes exposing (class, value, placeholder)
import Http
import HttpExt
import Effects exposing (Effects, Never)
import Task exposing (Task)
import Json.Decode exposing (Decoder)
import StartApp
import Async exposing (..)

import PokemonTable
import Pokemon exposing (..)

type alias Model =
    { pokemonTable: Async PokemonTable.Model
    , selectedPokemon: Maybe Pokemon
    }

initModel : Model
initModel =
    { pokemonTable = Waiting "Loading Pokémon, please wait..."
    , selectedPokemon = Nothing
    }

init : (Model, Effects Action)
--init = (initModel, Effects.none)
init = (initModel, PokemonTable.fetch OnPokemonTableLoaded)

type Action = NoAction
            | OnPokemonTableLoaded (Result Http.Error PokemonTable.Model)
            | SelectPokemon String
            | OnPokemonLoaded (Result Http.Error Pokemon)

update : Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        NoAction -> (model, Effects.none)
        OnPokemonTableLoaded result ->
            case result of
                Ok list -> ({ model | pokemonTable = Finished list }, Effects.none)
                Err msg -> ({ model | pokemonTable = Error ("Failed loading Pokémon: " ++ toString msg) }, Effects.none)
        SelectPokemon name -> ({ model | selectedPokemon = Nothing}, Pokemon.fetch name OnPokemonLoaded)
        OnPokemonLoaded result ->
            case result of
                Ok pmon -> ({ model | selectedPokemon = Just pmon }, Effects.none)
                Err msg -> (model, Effects.none)

view : Signal.Address Action -> Model -> Html
view address model =
    div []
        [ text "Pokemon: "
        , case model.selectedPokemon of
            Just pmon -> Pokemon.view pmon
            Nothing -> div [] []
        , case model.pokemonTable of
            Finished pmonTable -> PokemonTable.viewWithSelect address SelectPokemon pmonTable
            Waiting msg -> div [class "pokemonTableLoadingMessage"] [ text msg ]
            Error msg -> div [] [ text msg ]
        ]

app : StartApp.App Model
app = StartApp.start { init = init, view = view, update = update, inputs = [] }

main : Signal.Signal Html.Html
main = app.html

port tasks : Signal (Task.Task Never ())
port tasks = app.tasks
