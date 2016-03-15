module PokemonTable where

import Html exposing (..)
import Html.Attributes exposing (class, style, src)
import Html.Events exposing (onClick)
import Http
import HttpExt
import ListExt
import Effects
import String
import StringExt

import NamedAPIResourceList

type alias Model = NamedAPIResourceList.NamedAPIResourceList

empty : Model
empty = NamedAPIResourceList.empty

listUrl : String
listUrl = "http://pokeapi.co/api/v2/pokemon-species/?limit=100000" --100000 to get all

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
    let id = Maybe.withDefault "" <| guessIdString resource
    in  td  [ onClick address (select resource.name)
            ]
            [ img [ src (imageFromId id) ] []
            , figcaption [] [ text <| id ++ ". " ++ (StringExt.capitalize resource.name) ]
            ]

imageFromId : String -> String
imageFromId id = "http://pokeapi.co/media/sprites/pokemon/" ++ id ++ ".png"

guessIdString : NamedAPIResourceList.NamedAPIResource -> Maybe String
guessIdString res = List.head (List.reverse (List.filter isNotEmpty (String.split "/" res.url)))

isNotEmpty list = not (String.isEmpty list)
