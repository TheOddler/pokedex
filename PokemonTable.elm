module PokemonTable where

import Html exposing (..)
import Html.Attributes exposing (class, style, src, style)
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

viewWithSelect : Signal.Address a -> Model -> (String -> a) -> Html
viewWithSelect address model select =
    div [ class "pokemonTable" ]
        [ ul [] <| List.map (toLi address select) model.results
        ]

toLi address select resource =
    let id = Maybe.withDefault "" <| guessIdString resource
    in  li  [ onClick address (select resource.name)
            ]
            [ figure []
                [ img [ src (imageFromId id) ] []
                , figcaption [] [ text <| id ++ ". " ++ StringExt.capitalize resource.name ]
                ]
            ]

imageFromId : String -> String
imageFromId id = "http://pokeapi.co/media/sprites/pokemon/" ++ id ++ ".png"

guessIdString : NamedAPIResourceList.NamedAPIResource -> Maybe String
guessIdString res = List.head (List.reverse (List.filter isNotEmpty (String.split "/" res.url)))

isNotEmpty list = not (String.isEmpty list)
