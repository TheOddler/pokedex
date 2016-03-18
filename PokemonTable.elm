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
                [ img   [ src (imageFromId id)
                        , attribute "onerror" "this.onerror=null;this.src='images/missing-image.png';"
                        ] []
                , figcaption [] [ text <| id ++ ". " ++ resource.name ]
                ]
            ]

imageFromId : String -> String
imageFromId id = "http://pokeapi.co/media/sprites/pokemon/" ++ id ++ ".png"

guessIdString : NamedAPIResource -> Maybe String
guessIdString res = List.head (List.reverse (List.filter isNotEmpty (String.split "/" res.url)))

isNotEmpty list = not (String.isEmpty list)
