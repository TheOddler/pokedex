module Main exposing (..)

import Browser
import Css exposing (..)
import Html.Styled as Html exposing (Html, a, div, text, toUnstyled)
import Html.Styled.Attributes exposing (css, href)
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
        , view = view >> toUnstyled
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
            [ text "Images by "
            , a [ href "https://archives.bulbagarden.net/wiki/Category:Ken_Sugimori_Pok%C3%A9mon_artwork" ] [ text "Ken Sugimori" ]
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
