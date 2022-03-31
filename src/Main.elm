module Main exposing (..)

import Browser
import Css exposing (..)
import Html.Styled as Html exposing (Html, a, div, text, toUnstyled)
import Html.Styled.Attributes exposing (css, href)
import Http
import LocalStorage exposing (LocalStorage)
import Pokedex exposing (Pokedex)


type Model
    = Loading LocalStorage (Maybe String)
    | DoneLoading Pokedex
    | Error String Http.Error


type Msg
    = GotPokemonCsv (Result Http.Error String)
    | PokedexMsg Pokedex.Msg


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
    ( Loading localStorage Nothing
    , Http.get
        { url = "data/pokemon.csv"
        , expect = Http.expectString GotPokemonCsv
        }
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Loading localStorage _ ->
            case msg of
                GotPokemonCsv result ->
                    case result of
                        Ok csv ->
                            case Pokedex.init localStorage csv of
                                Ok pokedex ->
                                    ( DoneLoading pokedex, Cmd.none )

                                Err err ->
                                    ( Error ("Failed initilizing Pokedex: " ++ err) (Http.BadStatus 500), Cmd.none )

                        Err err ->
                            ( Error "Failed getting 'Pokémon' CSV" err, Cmd.none )

                PokedexMsg _ ->
                    ( model, Cmd.none )

        DoneLoading dex ->
            case msg of
                PokedexMsg m ->
                    let
                        ( updatedModel, cmd ) =
                            Pokedex.update m dex
                    in
                    ( DoneLoading updatedModel, Cmd.map PokedexMsg cmd )

                _ ->
                    ( model, Cmd.none )

        Error _ _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
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
        [ case model of
            Loading _ _ ->
                div
                    [ css [ infoStyle ]
                    ]
                    [ text "Loading..." ]

            Error message error ->
                let
                    errorMessage =
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
                in
                div [ css [ infoStyle ] ]
                    [ div [] [ text <| message ]
                    , div [] [ text <| errorMessage ]
                    , a
                        [ href <| "mailto:pablo.bollansee@gmail.com?Subject=Error in Pokedex: " ++ message ++ "&body=" ++ errorMessage
                        ]
                        [ text "Click here to report this error."
                        ]
                    ]

            DoneLoading dex ->
                Html.map PokedexMsg <| Pokedex.view dex
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
