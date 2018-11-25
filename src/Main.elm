import Browser
import Html exposing (Html, div, text, input)
import Html.Attributes exposing (class, id, placeholder, value, classList)
import Html.Events exposing (onInput, onClick)
import Html.Lazy as Lazy
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
    , selected: Selected
    }


type Msg 
    = SetSearch String
    | Select Pokemon
    | Deselect


type Selected 
    = Selected Pokemon
    | Deselected Pokemon


init : Model
init = 
    let
        allPokemon = Pokemon.parse Data.Pokemon.csv Data.PokemonTypes.csv
    in
        { searchString = ""
        , pokemon = allPokemon
        , types = Types.parse Data.Types.csv Data.TypeEffectiveness.csv
        , selected = Deselected (List.head allPokemon |> Maybe.withDefault { id = -1, speciesId = -1, identifier = "", types = []})
        }


main = Browser.sandbox { init = init, update = update, view = view }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetSearch s -> {model | searchString = s}
        Select p -> { model | selected = Selected p}
        Deselect -> 
            case model.selected of 
                Selected p -> { model | selected = Deselected p }
                _ -> model


view : Model -> Html Msg
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

viewWrapPokemon : Model -> Pokemon -> Html Msg
viewWrapPokemon model pkm = 
    div 
        [ classList
            [ ("item", True)
            , ("hidden", not <| String.contains (toLower model.searchString) pkm.identifier)
            ]
        , onClick <| Select pkm
        ] 
        [ Lazy.lazy2 Pokemon.view model.types pkm ]
