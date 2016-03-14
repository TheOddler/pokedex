module Pokemon where

import Json.Decode exposing (Decoder, (:=), int, string, object3, object8)
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (class, style, src)
import DecodeExt exposing (nullOr)

type alias Pokemon =
    { id: Int
    , name: String
    , sprites: PokemonSprites
    }

type alias PokemonSprites =
    { back_female: (Maybe String)
    , back_shiny_female: (Maybe String)
    , back_default: (Maybe String)
    , front_female: (Maybe String)
    , front_shiny_female: (Maybe String)
    , back_shiny: (Maybe String)
    , front_default: (Maybe String)
    , front_shiny: (Maybe String)
    }

decoder : Decoder Pokemon
decoder =
    object3 Pokemon
        ("id" := int)
        ("name" := string)
        ("sprites" := spritesDecoder)

spritesDecoder : Decoder PokemonSprites
spritesDecoder =
    object8 PokemonSprites
        ("back_female" := nullOr string)
        ("back_shiny_female" := nullOr string)
        ("back_default" := nullOr string)
        ("front_female" := nullOr string)
        ("front_shiny_female" := nullOr string)
        ("back_shiny" := nullOr string)
        ("front_default" := nullOr string)
        ("front_shiny" := nullOr string)

view : Pokemon -> Html.Html
view = simpleView

simpleView : Pokemon -> Html.Html
simpleView pokemon =
    div [ style [("backgroundColor", "grey")]
        ]
        [ img [ src <| Maybe.withDefault "" pokemon.sprites.front_default ] []
        , text pokemon.name
        ]
