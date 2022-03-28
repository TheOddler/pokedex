module Pokedex exposing (Msg, Pokedex, init, update, view)

import Csv.Decode as Decode
import Dict exposing (Dict)
import Html exposing (Html, div, input)
import Html.Attributes exposing (class, classList, id, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Html.Lazy as Lazy
import Pokemon exposing (Pokemon)
import PokemonCSVRow
import String exposing (toLower)


type alias Pokedex =
    { searchString : String
    , pokemon : List Pokemon
    , pokemonIdDict : Dict Int Pokemon
    , selected : Selected
    }


type Msg
    = SetSearch String
    | Select Pokemon
    | Deselect


type Selected
    = Selected Pokemon
    | Deselected Pokemon


init : String -> Result String Pokedex
init pokemonCsv =
    case Result.map Pokemon.fromCSVRows (Decode.decodeCsv Decode.FieldNamesFromFirstRow PokemonCSVRow.decoder pokemonCsv) of
        Ok (first :: rest) ->
            Ok
                { searchString = ""
                , pokemon = first :: rest
                , pokemonIdDict = Dict.fromList <| List.map (\p -> ( p.id, p )) (first :: rest)
                , selected = Deselected first
                }

        Ok [] ->
            Err "CSV file was empty."

        Err err ->
            Err (Decode.errorToString err)


update : Msg -> Pokedex -> Pokedex
update msg model =
    case msg of
        SetSearch s ->
            { model | searchString = s }

        Select p ->
            { model | selected = Selected p }

        Deselect ->
            case model.selected of
                Selected p ->
                    { model | selected = Deselected p }

                _ ->
                    model


view : Pokedex -> Html Msg
view model =
    div
        [ class "pokedex"
        ]
        [ input
            [ id "search"
            , placeholder "Search for a Pokémon..."
            , value model.searchString
            , onInput SetSearch
            ]
            []
        , case model.selected of
            Selected pkm ->
                div [ class "detailsWrapper", onClick Deselect ] [ Pokemon.viewDetail model.pokemonIdDict pkm ]

            Deselected pkm ->
                div [ class "hidden", class "detailsWrapper", onClick Deselect ] [ Pokemon.viewDetail model.pokemonIdDict pkm ]
        , div [ class "list" ] <|
            List.map (viewWrapPokemon model) model.pokemon
        ]


viewWrapPokemon : Pokedex -> Pokemon -> Html Msg
viewWrapPokemon model pkm =
    div
        [ classList
            [ ( "item", True )
            , ( "hidden", not <| String.contains (toLower model.searchString) (toLower pkm.fullName) )
            ]
        , onClick <|
            if Selected pkm == model.selected then
                Deselect

            else
                Select pkm
        ]
        [ Lazy.lazy Pokemon.view pkm ]
