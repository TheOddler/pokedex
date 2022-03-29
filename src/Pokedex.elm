module Pokedex exposing (Msg, Pokedex, init, update, view)

import Css exposing (..)
import Csv.Decode as Decode
import Dict exposing (Dict)
import Html.Styled exposing (Html, div, input)
import Html.Styled.Attributes exposing (class, css, id, placeholder, value)
import Html.Styled.Events exposing (onClick, onInput)
import Html.Styled.Lazy as Lazy
import Maybe.Extra as Maybe
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
        [ css [ paddingTop (em 4) ] ]
        [ input
            [ id "search" -- Needed for the auto select script
            , css [ searchStyle ]
            , placeholder "Search for a Pokémon..."
            , value model.searchString
            , onInput SetSearch
            ]
            []
        , case model.selected of
            Selected pkm ->
                div [ css [ detailsWrapperStyle ], onClick Deselect ] [ Pokemon.viewDetail True model.pokemonIdDict pkm ]

            Deselected pkm ->
                div
                    [ css [ detailsWrapperStyle ]
                    , onClick Deselect
                    ]
                    [ Pokemon.viewDetail False model.pokemonIdDict pkm
                    ]
        , div
            [ css
                [ textAlign center
                , padding (px 0)
                ]
            ]
          <|
            List.map (viewWrapPokemon model) model.pokemon
        ]


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


detailsWrapperStyle : Style
detailsWrapperStyle =
    Css.batch
        [ pointerEvents none
        , position fixed
        , left (pct 50)
        , top (pct 50)
        , height auto
        , zIndex (int 200)
        , transform <| translate2 (pct -50) (pct -50)
        ]


viewWrapPokemon : Pokedex -> Pokemon -> Html Msg
viewWrapPokemon model pkm =
    div
        [ css <|
            Maybe.values
                [ Just <| display inlineBlock
                , if String.contains (toLower model.searchString) (toLower pkm.fullName) then
                    Nothing

                  else
                    Just <| display none
                ]
        , onClick <|
            if Selected pkm == model.selected then
                Deselect

            else
                Select pkm
        ]
        [ Lazy.lazy Pokemon.view pkm ]
