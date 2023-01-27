module Pokedex exposing (Msg, Pokedex, init, update, view)

import Amyfy exposing (amyfyPokemon)
import Html as Html exposing (Html, a, div, input, text)
import Html.Attributes exposing (class, href, id, placeholder, value)
import Html.Events exposing (onClick, onFocus, onInput)
import Pokemon exposing (Pokemon)
import Pokemon.Data
import Pokemon.Details
import Pokemon.List
import Pokemon.Mode exposing (Mode(..))
import Simple.Fuzzy as Fuzzy


type alias Pokedex =
    { searchString : String
    , pokemon : List Pokemon
    , details : Pokemon.Details.Model
    }


type Msg
    = SetSearch String
    | PokemonDetailsMsg Pokemon.Details.Msg
    | ClearAll


init : Maybe Mode -> Bool -> Pokedex
init mode amyfy =
    { searchString = ""
    , pokemon =
        if amyfy then
            List.map amyfyPokemon Pokemon.Data.all

        else
            Pokemon.Data.all
    , details = Pokemon.Details.init mode Pokemon.Data.first
    }


update : Msg -> Pokedex -> ( Pokedex, Cmd Msg )
update msg model =
    case msg of
        SetSearch s ->
            ( { model | searchString = s }, Cmd.none )

        PokemonDetailsMsg pkmDetailsMsg ->
            let
                ( updatedDetails, cmd ) =
                    Pokemon.Details.update pkmDetailsMsg model.details
            in
            ( { model | details = updatedDetails }, Cmd.map PokemonDetailsMsg cmd )

        ClearAll ->
            update (SetSearch "") model
                -- throw away the Cmd from set search as it's just none anyway
                |> Tuple.first
                |> update (PokemonDetailsMsg Pokemon.Details.Deselect)


view : Pokedex -> Html Msg
view model =
    div
        [ class "pokedex"
        , onClick <| PokemonDetailsMsg Pokemon.Details.Deselect
        ]
        [ input
            [ id "search" -- Needed for the auto select script
            , placeholder "Search for a Pokémon..."
            , value model.searchString
            , onInput SetSearch
            , onFocus ClearAll
            ]
            []
        , Html.map PokemonDetailsMsg <| Pokemon.Details.view model.pokemon model.details
        , Html.map PokemonDetailsMsg <| Pokemon.List.view model.pokemon (searchPokemonFilter model.searchString)
        , div
            [ class "externalLinks"
            ]
            [ text "Source code available at "
            , a [ href "https://github.com/TheOddler/pokedex" ] [ text "github.com/TheOddler/pokedex" ]
            , text "."
            ]
        , div []
            [ text "Images by "
            , a [ href "https://archives.bulbagarden.net/wiki/Category:Ken_Sugimori_Pok%C3%A9mon_artwork" ] [ text "Ken Sugimori" ]
            , text "."
            ]
        ]


searchPokemonFilter : String -> Pokemon -> Bool
searchPokemonFilter searchStr pkm =
    Fuzzy.match searchStr pkm.fullName
