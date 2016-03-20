module PokemonTable where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import HttpExt
import ListExt
import Effects
import String
import Json.Encode

import NamedAPIResource exposing (NamedAPIResource)
import NamedAPIResourceList exposing (NamedAPIResourceList)

type alias Model = NamedAPIResourceList

empty : Model
empty = NamedAPIResourceList.empty

listUrl : String
listUrl = "http://pokeapi.co/api/v2/pokemon/?limit=100000" --100000 to get all

fetch : (Result Http.Error Model -> a) -> Effects.Effects a
fetch callback = HttpExt.fetch NamedAPIResourceList.decoder listUrl callback

viewWithSelect : Signal.Address a -> Model -> String -> (String -> a) -> Html
viewWithSelect address model searchString select =
    let search = String.toLower searchString
        pokemon = List.filter (String.contains search << .name) model.results
    in  div [ class "pokemonTable" ]
            [ ul [] <| List.map (toLi address select) pokemon
            ]

toLi address select resource =
    let id = Maybe.withDefault "" <| guessIdString resource
        guessedImageUrl = imageFromId id
        caption = resource.name
    in  li  [ onClick address (select resource.name)
            ]
            [ figure []
                [ img   [ src guessedImageUrl
                        , attribute "onerror" "this.onerror=null;this.src='images/missing-image.png';"
                        ] []
                , figcaption [] [ text caption ]
                ]
            ]

imageFromId : String -> String
imageFromId id = "http://pokeapi.co/media/sprites/pokemon/" ++ id ++ ".png"

guessIdString : NamedAPIResource -> Maybe String
guessIdString res = List.head (List.reverse (List.filter isNotEmpty (String.split "/" res.url)))

isNotEmpty list = not (String.isEmpty list)
