module PokemonTable where

import Html exposing (Html, div, img, text, ul, li, table, tr, td)
import Html.Attributes exposing (class, style, src)
import Html.Events exposing (onClick)
import Http
import HttpExt
import ListExt
import Effects
import String

import NamedAPIResourceList

type alias Model = NamedAPIResourceList.NamedAPIResourceList

empty : Model
empty = NamedAPIResourceList.empty

listUrl : String
listUrl =  "http://pokeapi.co/api/v2/pokemon-species/?limit=100000" --100000 to get all

fetch : (Result Http.Error Model -> a) -> Effects.Effects a
fetch callback = HttpExt.fetch NamedAPIResourceList.decoder listUrl callback

viewWithSelect : Signal.Address a -> (String -> a) -> Model -> Html
viewWithSelect address select model =
    div []
        [ table [ class "pokemonTable" ] <| List.map (toTr address select) (ListExt.split 10 model.results)
        ]

toTr address select resources =
    tr [] <| List.map (toTd address select) resources

toTd address select resource =
    td  [ onClick address (select resource.name)
        ]
        [ img [ src (guessImage resource) ] []
        , text resource.name
        ]

guessImage : NamedAPIResourceList.NamedAPIResource -> String
guessImage res = "http://pokeapi.co/media/sprites/pokemon/" ++ (guessId res) ++ ".png"

guessId : NamedAPIResourceList.NamedAPIResource -> String
guessId res = Maybe.withDefault "" (List.head (List.reverse (List.filter isNotEmpty (String.split "/" res.url))))

isNotEmpty list = not (String.isEmpty list)
