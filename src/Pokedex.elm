module Pokedex exposing (Pokedex, Msg, init, update, view)

import Html exposing (Html, div, text, input)
import Html.Attributes exposing (class, id, placeholder, value, classList)
import Html.Events exposing (onInput, onClick)
import Html.Lazy as Lazy
import Dict exposing (Dict)
import String exposing (toLower)

import Pokemon exposing (Pokemon)
import Types exposing (Type)


type alias Pokedex =
    { searchString: String
    , pokemon: List Pokemon
    , types: Dict Int Type
    , selected: Selected
    }


type Msg 
    = SetSearch String
    | Select Pokemon
    | Deselect


type Selected 
    = Selected Pokemon
    | Deselected Pokemon


init : String -> String -> String -> String -> Pokedex
init pokemonCsv pokemonToTypesCsv typesCsv typeEffectivenessCsv = 
    let
        allPokemon = Pokemon.parse pokemonCsv pokemonToTypesCsv
    in
        { searchString = ""
        , pokemon = allPokemon
        , types = Types.parse typesCsv typeEffectivenessCsv
        , selected = Deselected (List.head allPokemon |> Maybe.withDefault { id = -1, speciesId = -1, identifier = "", types = []})
        }


update : Msg -> Pokedex -> Pokedex
update msg model =
    case msg of
        SetSearch s -> {model | searchString = s}
        Select p -> { model | selected = Selected p}
        Deselect -> 
            case model.selected of 
                Selected p -> { model | selected = Deselected p }
                _ -> model


view : Pokedex -> Html Msg
view model =
    div [ class "pokedex"
        ]
        [ input
            [ id "search"
            , placeholder "Type to search for a Pokémon..."
            , value model.searchString
            , onInput SetSearch
            ] []
        , case model.selected of
            Selected pkm -> div [ class "detailsWrapper", onClick Deselect ] [ Pokemon.viewDetail model.types pkm ]
            Deselected pkm -> div [ class "hidden", class "detailsWrapper", onClick Deselect ] [ Pokemon.viewDetail model.types pkm ]
        , div [ class "list" ]
            <| List.map (viewWrapPokemon model) model.pokemon
        ]


viewWrapPokemon : Pokedex -> Pokemon -> Html Msg
viewWrapPokemon model pkm = 
    div 
        [ classList
            [ ("item", True)
            , ("hidden", not <| String.contains (toLower model.searchString) pkm.identifier)
            ]
        , onClick <| Select pkm
        ] 
        [ Lazy.lazy2 Pokemon.view model.types pkm ]
