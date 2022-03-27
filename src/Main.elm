module Main exposing (..)

import Browser
import Html exposing (Html, a, div, text)
import Html.Attributes exposing (href, id)
import Http
import Pokedex exposing (Pokedex)


type Model
    = Loading (Maybe String)
    | DoneLoading Pokedex
    | Error String Http.Error


type Msg
    = GotPokemonCsv (Result Http.Error String)
    | PokedexMsg Pokedex.Msg


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading Nothing
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
        Loading _ ->
            case msg of
                GotPokemonCsv result ->
                    case result of
                        Ok csv ->
                            case Pokedex.init csv of
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
                    ( DoneLoading <| Pokedex.update m dex, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Error _ _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        Loading _ ->
            div [ id "loading" ]
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
            div [ id "error" ]
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
