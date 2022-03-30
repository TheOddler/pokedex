module Pokedex exposing (Msg, Pokedex, init, update, view)

import Css exposing (..)
import Csv.Decode as Decode
import Dict exposing (Dict)
import Html.Styled exposing (Html, div, input)
import Html.Styled.Attributes exposing (css, id, placeholder, value)
import Html.Styled.Events exposing (onClick, onInput)
import Pokemon exposing (Pokemon)
import PokemonCSVRow


type alias Pokedex =
    { searchString : String
    , pokemon : List Pokemon
    , pokemonIdDict : Dict Int Pokemon
    , selected : Selected
    , mode : Mode
    }


type Msg
    = SetSearch String
    | Select Pokemon
    | Deselect
    | ChangeMode Mode


type Selected
    = Selected Pokemon
    | Deselected Pokemon


type Mode
    = TypeEffectiveness
    | Evolution


init : String -> Result String Pokedex
init pokemonCsv =
    case Result.map Pokemon.fromCSVRows (Decode.decodeCsv Decode.FieldNamesFromFirstRow PokemonCSVRow.decoder pokemonCsv) of
        Ok (first :: rest) ->
            Ok
                { searchString = ""
                , pokemon = first :: rest
                , pokemonIdDict = Dict.fromList <| List.map (\p -> ( p.id, p )) (first :: rest)
                , selected = Deselected first
                , mode = Evolution
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

        ChangeMode mode ->
            { model | mode = mode }


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
                , displayFlex
                , flexWrap wrap
                , justifyContent center
                , alignItems stretch
                ]
            ]
          <|
            List.map (Pokemon.view Select model.searchString) model.pokemon
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
