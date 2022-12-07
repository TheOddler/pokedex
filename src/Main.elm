module Main exposing (..)

import Browser
import Css exposing (..)
import Html.Styled as Html exposing (Html, a, div, text, toUnstyled)
import Html.Styled.Attributes exposing (css, href)
import Http
import LocalStorage exposing (LocalStorage)
import Pokedex exposing (Pokedex)


type alias Model =
    Pokedex


type alias Msg =
    Pokedex.Msg


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
    ( Pokedex.init localStorage
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg pokedex =
    let
        ( updatedPokedex, nextMsg ) =
            Pokedex.update msg pokedex
    in
    ( updatedPokedex, nextMsg )


view : Model -> Html Msg
view pokedex =
    div
        [ css
            [ height (pct 100)
            , color (rgb 255 255 255)
            , backgroundColor (rgb 0 0 0)
            , property "text-shadow" "1px 1px black, 0 0 4px rgba(0, 0, 0, 0.5)"
            , fontFamily sansSerif
            , textAlign center
            , overflowX hidden
            , overflowY scroll
            , displayFlex
            , flexDirection column
            ]
        ]
        [ Pokedex.view pokedex
        , div
            [ css
                [ marginTop auto ]
            ]
            [ text "Source code available at "
            , a [ href "https://github.com/TheOddler/pokedex" ] [ text "github.com/TheOddler/pokedex" ]
            , text "."
            ]
        , div []
            [ text "Images from "
            , a [ href "https://pokemondb.net/sprites" ] [ text "PokemonDB.net" ]
            , text "."
            ]
        ]


infoCss : Html.Attribute msg
infoCss =
    css
        [ marginTop auto ]


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "Bad url: " ++ url

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus status ->
            "Bad status: " ++ String.fromInt status

        Http.BadBody body ->
            "Bad body: " ++ body
