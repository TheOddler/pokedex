import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Dict exposing (Dict)

import Pokemon exposing (Pokemon)
import Data.Pokemon
import Data.PokemonTypes
import Types exposing (Type)
import Data.Types
import Data.TypeEffectiveness

type alias Model =
    { searchString: String
    , pokemon: List Pokemon
    , types: Dict Int Type
    , selected: Maybe Pokemon
    }


type Msg 
    = SetSearch String
    | Select Pokemon
    | Deselect


init : Model
init = 
    { searchString = ""
    , pokemon = Pokemon.parse Data.Pokemon.csv Data.PokemonTypes.csv
    , types = Types.parse Data.Types.csv Data.TypeEffectiveness.csv
    , selected = Nothing
    }


main = Browser.sandbox { init = init, update = update, view = view }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetSearch s -> {model | searchString = s}
        Select p -> { model | selected = Just p}
        Deselect -> { model | selected = Nothing}


view : Model -> Html Msg
view model =
    div [ class "pokedex" ]
        [ text "Pokédex"
        , ul [ class "list" ]
            <| List.map (viewWrapPokemon model.types) model.pokemon
        ]

viewWrapPokemon : Dict Int Type -> Pokemon -> Html Msg
viewWrapPokemon types pkm = 
    li [ class "item" ] 
        [ Pokemon.view Select types pkm ]
