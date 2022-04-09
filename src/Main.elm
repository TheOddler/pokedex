module Main exposing (..)

import Browser
import Css exposing (..)
import Html.Styled as Html exposing (Html, a, div, text, toUnstyled)
import Html.Styled.Attributes exposing (css, href)
import LocalStorage exposing (LocalStorage)
import Pokedex exposing (Pokedex)


type Model
    = Pokedex Pokedex


type Msg
    = PokedexMsg Pokedex.Msg


main : Program LocalStorage Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view >> toUnstyled
        }


init : LocalStorage -> ( Model, Cmd Msg )
init localStorage =
    let
        ( pokedex, pokedexMsg ) =
            Pokedex.init localStorage
    in
    ( Pokedex pokedex
    , Cmd.map PokedexMsg pokedexMsg
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update (PokedexMsg pokedexMsg) (Pokedex pokedex) =
    let
        ( updatedPokedex, nextPokedexMsg ) =
            Pokedex.update pokedexMsg pokedex
    in
    ( Pokedex updatedPokedex, Cmd.map PokedexMsg nextPokedexMsg )


view : Model -> Html Msg
view (Pokedex pokedex) =
    div
        [ css
            [ fontFamily sansSerif
            , color (rgb 255 255 255)
            , property "text-shadow" "1px 1px black, 0 0 4px rgba(0, 0, 0, 0.5)"
            , textAlign center
            , overflowX hidden
            , height (pct 100)
            , backgroundColor (rgb 230 230 230)
            ]
        ]
        [ Html.map PokedexMsg <| Pokedex.view pokedex
        , div []
            [ text "Images from "
            , a [ href "https://github.com/PokeAPI/sprites" ] [ text "PokeAPI/sprites" ]
            , text "."
            ]
        ]


infoStyle : Style
infoStyle =
    Css.batch
        [ position fixed
        , left (pct 50)
        , top (pct 50)
        , transform (translate2 (pct -50) (pct -50))
        ]
