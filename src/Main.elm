import Browser
import Html exposing (Html, div, ul, li, text, input)
import Html.Attributes exposing (class, id, placeholder, value, classList)
import Html.Events exposing (onInput, onClick, stopPropagationOn)
import Dict exposing (Dict)
import String exposing (toLower)
import Json.Decode as Decode

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
    div [ class "pokedex"
        , onClick Deselect
        ]
        [ input
            [ id "search"
            , placeholder "Type to search for a Pokémon..."
            , value model.searchString
            , onInput SetSearch
            ] []
        , case model.selected of
            Just pkm -> Pokemon.viewDetail model.types pkm
            Nothing -> div [] []
        , ul [ class "list" ]
            <| List.map (viewWrapPokemon model) model.pokemon
        ]

viewWrapPokemon : Model -> Pokemon -> Html Msg
viewWrapPokemon model pkm = 
    li 
        [ classList
            [ ("item", True)
            , ("hidden", not <| String.contains (toLower model.searchString) (toLower pkm.name))
            ]
        , stopPropagationOn "click" <| Decode.succeed (Select pkm, True)
        ] 
        [ Pokemon.view model.types pkm ]
