module Pokedex exposing (Msg, Pokedex, init, update, view)

import Css exposing (..)
import Csv.Decode as Decode
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html, div, input)
import Html.Styled.Attributes exposing (css, id, placeholder, value)
import Html.Styled.Events exposing (onInput)
import LocalStorage exposing (LocalStorage)
import Pokemon exposing (Pokemon)
import Pokemon.Details
import Pokemon.List
import PokemonCSVRow
import String exposing (toLower)


type alias Pokedex =
    { searchString : String
    , pokemon : List Pokemon
    , pokemonIdDict : Dict Int Pokemon
    , pokemonSettings : Pokemon.Settings
    }


type Msg
    = SetSearch String
    | PokemonMsg Pokemon.Msg


init : LocalStorage -> String -> Result String Pokedex
init localStorage pokemonCsv =
    case Result.map Pokemon.fromCSVRows (Decode.decodeCsv Decode.FieldNamesFromFirstRow PokemonCSVRow.decoder pokemonCsv) of
        Ok (first :: rest) ->
            Ok
                { searchString = ""
                , pokemon = first :: rest
                , pokemonIdDict = Dict.fromList <| List.map (\p -> ( p.id, p )) (first :: rest)
                , pokemonSettings = Pokemon.initSettings localStorage first
                }

        Ok [] ->
            Err "CSV file was empty."

        Err err ->
            Err (Decode.errorToString err)


update : Msg -> Pokedex -> ( Pokedex, Cmd Msg )
update msg model =
    case msg of
        SetSearch s ->
            ( { model | searchString = s }, Cmd.none )

        PokemonMsg pkmMsg ->
            let
                ( updatedSettings, cmd ) =
                    Pokemon.updateSettings pkmMsg model.pokemonSettings
            in
            ( { model | pokemonSettings = updatedSettings }, Cmd.map PokemonMsg cmd )


view : Pokedex -> Html Msg
view model =
    div
        [ css [ paddingTop (em 4) ] ]
        [ input
            [ id "search" -- Needed for the auto select script
            , css [ searchStyle ]
            , placeholder "Search for a Pokémon..."
            , value model.searchString
            , onInput SetSearch
            ]
            []
        , Html.map PokemonMsg <| Pokemon.Details.view model.pokemonIdDict model.pokemonSettings
        , Html.map PokemonMsg <| Pokemon.List.view model.pokemon (searchPokemonFilter model.searchString)
        ]


searchPokemonFilter : String -> Pokemon -> Bool
searchPokemonFilter searchStr pkm =
    String.contains searchStr (toLower pkm.fullName)


searchStyle : Style
searchStyle =
    Css.batch
        [ fontSize (em 1)
        , textAlign center
        , position fixed
        , top (px 0)
        , transform (translate2 (pct -50) (px 0))
        , zIndex (int 200)
        , margin2 (em 0.8) auto
        , padding (em 0.5)
        , borderRadius (em 5)
        , border (px 0)
        , width (em 18)
        , maxWidth (pct 90)
        , backgroundColor (rgba 255 255 255 0.8)
        ]
