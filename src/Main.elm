module Main exposing (main)

import Browser
import Html as Html exposing (Html, a, div, text)
import Html.Attributes exposing (class, href)
import Http
import Pokedex exposing (Pokedex)
import Pokemon.Mode as Mode


type alias Model =
    Pokedex


type alias Msg =
    Pokedex.Msg


type alias Flags =
    { mode : Maybe String
    , amyfy : Bool
    }


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Pokedex.init (Maybe.map Mode.fromString flags.mode) flags.amyfy
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
        [ class "main"
        ]
        [ Pokedex.view pokedex
        , div
            [ class "externalLinks"
            ]
            [ text "Source code available at "
            , a [ href "https://github.com/TheOddler/pokedex" ] [ text "github.com/TheOddler/pokedex" ]
            , text "."
            ]
        , div []
            [ text "Images by "
            , a [ href "https://archives.bulbagarden.net/wiki/Category:Ken_Sugimori_Pok%C3%A9mon_artwork" ] [ text "Ken Sugimori" ]
            , text "."
            ]
        ]
