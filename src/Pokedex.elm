module Pokedex exposing (Msg, Pokedex, init, update, view)

import Css exposing (..)
import Csv.Decode as Decode
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html, div, input)
import Html.Styled.Attributes exposing (css, id, placeholder, value)
import Html.Styled.Events exposing (onInput)
import Http
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
    , details : Maybe Pokemon.Details.Model
    }


type Msg
    = SetSearch String
    | PokemonDetailsMsg Pokemon.Details.Msg
    | DoneLoading LocalStorage (Result Http.Error String)


init : LocalStorage -> ( Pokedex, Cmd Msg )
init localStorage =
    ( { searchString = ""
      , pokemon = []
      , pokemonIdDict = Dict.empty
      , details = Nothing
      }
    , Http.get
        { url = "data/pokemon.csv"
        , expect = Http.expectString (DoneLoading localStorage)
        }
    )


update : Msg -> Pokedex -> ( Pokedex, Cmd Msg )
update msg model =
    case msg of
        SetSearch s ->
            ( { model | searchString = s }, Cmd.none )

        PokemonDetailsMsg pkmDetailsMsg ->
            case model.details of
                Nothing ->
                    ( model, Cmd.none )

                Just details ->
                    let
                        ( updatedDetails, cmd ) =
                            Pokemon.Details.update pkmDetailsMsg details
                    in
                    ( { model | details = Just updatedDetails }, Cmd.map PokemonDetailsMsg cmd )

        DoneLoading localStorage errorOrCsv ->
            case errorOrCsv of
                Err _ ->
                    ( model, Cmd.none )

                Ok csv ->
                    case Result.map Pokemon.fromCSVRows (Decode.decodeCsv Decode.FieldNamesFromFirstRow PokemonCSVRow.decoder csv) of
                        Ok (first :: rest) ->
                            ( { model
                                | pokemon = first :: rest
                                , pokemonIdDict = Dict.fromList <| List.map (\p -> ( p.id, p )) (first :: rest)
                                , details = Just <| Pokemon.Details.init localStorage first
                              }
                            , Cmd.none
                            )

                        Ok [] ->
                            ( model, Cmd.none )

                        Err _ ->
                            ( model, Cmd.none )


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
        , case model.details of
            Just details ->
                Html.map PokemonDetailsMsg <| Pokemon.Details.view model.pokemonIdDict details

            Nothing ->
                div [] []
        , Html.map PokemonDetailsMsg <| Pokemon.List.view model.pokemon (searchPokemonFilter model.searchString)
        ]


searchPokemonFilter : String -> Pokemon -> Bool
searchPokemonFilter searchStr_ pkm =
    let
        searchStr =
            toLower searchStr_

        nameMatch =
            String.contains searchStr (toLower pkm.fullName)

        evolvesFromDetailsMatch =
            case pkm.evolvesFromDetails of
                Nothing ->
                    False

                Just evolvesFromDetails ->
                    String.contains searchStr (toLower evolvesFromDetails)

        transformGroupDetailsMatch =
            case pkm.transformGroupDetails of
                Nothing ->
                    False

                Just transformGroupDetails ->
                    String.contains searchStr (toLower transformGroupDetails)
    in
    nameMatch || evolvesFromDetailsMatch || transformGroupDetailsMatch


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
