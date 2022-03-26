module Main exposing (..)

import Browser
import Html exposing (Html, div, text, input, a)
import Html.Attributes exposing (class, id, placeholder, value, classList, href)
import Html.Events exposing (onInput, onClick)
import Http

import Pokedex exposing (Pokedex)


type Model 
    = Loading (Maybe String) (Maybe String) (Maybe String) (Maybe String)
    | DoneLoading Pokedex
    | Error String Http.Error


type Msg
    = GotPokemonCsv (Result Http.Error String)
    | GotPokemonTypesCsv (Result Http.Error String)
    | GotTypesCsv (Result Http.Error String)
    | GotTypeEffectivenessCsv (Result Http.Error String)
    | PokedexMsg Pokedex.Msg


main = 
    Browser.element 
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view 
    }


init : () -> (Model, Cmd Msg)
init _ = 
    ( Loading Nothing Nothing Nothing Nothing 
    , Cmd.batch
        [ Http.get
            { url = "data/pokemon.csv"
            , expect = Http.expectString GotPokemonCsv
            }
        , Http.get
            { url = "data/pokemon_types.csv"
            , expect = Http.expectString GotPokemonTypesCsv
            }
        , Http.get
            { url = "data/types.csv"
            , expect = Http.expectString GotTypesCsv
            }
        , Http.get
            { url = "data/type_efficacy.csv"
            , expect = Http.expectString GotTypeEffectivenessCsv
            }
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case model of
        Loading p ptt t te ->
            case msg of
                GotPokemonCsv result ->
                    case result of
                        Ok csv -> (checkLoadingDone <| Loading (Just csv) ptt t te, Cmd.none)
                        Err err -> (Error "Failed getting 'Pokémon' CSV" err, Cmd.none)
                        
                GotPokemonTypesCsv result ->
                    case result of
                        Ok csv -> (checkLoadingDone <| Loading p (Just csv) t te, Cmd.none)
                        Err err -> (Error "Failed getting 'Pokémon to types' CSV" err, Cmd.none)
                        
                GotTypesCsv result ->
                    case result of
                        Ok csv -> (checkLoadingDone <| Loading p ptt (Just csv) te, Cmd.none)
                        Err err -> (Error "Failed getting 'types' CSV" err, Cmd.none)
                        
                GotTypeEffectivenessCsv result ->
                    case result of
                        Ok csv -> (checkLoadingDone <| Loading p ptt t (Just csv), Cmd.none)
                        Err err -> (Error "Failed getting 'Type effectiveness' CSV" err, Cmd.none)

                PokedexMsg pmsg -> (model, Cmd.none)

        DoneLoading dex ->
            case msg of
                PokedexMsg m -> (DoneLoading <| Pokedex.update m dex, Cmd.none)
                _ -> (model, Cmd.none)

        Error _ _ -> (model, Cmd.none)


checkLoadingDone : Model -> Model
checkLoadingDone model = 
    case model of 
        Loading (Just p) (Just ptt) (Just t) (Just te) -> DoneLoading <| Pokedex.init p ptt t te
        _ -> model


view : Model -> Html Msg
view model = 
    case model of
        Loading p ptt t te -> 
            div [ id "loading" ]
                [ text "Loading..." ]
        Error message error ->
            let errorMessage = case error of 
                    Http.BadUrl url -> "Bad url: " ++ url
                    Http.Timeout -> "Timeout"
                    Http.NetworkError -> "Network error"
                    Http.BadStatus status -> "Bad status: " ++ String.fromInt status
                    Http.BadBody body -> "Bad body: " ++ body
            in
                div [ id "error" ]
                    [ div [] [ text <| message ]
                    , div [] [ text <| errorMessage ]
                    , a [ href <| "mailto:pablo.bollansee@gmail.com?Subject=Error in Pokedex: " ++ message ++ "&body=" ++ errorMessage
                        ]
                        [ text "Click here to report this error."
                        ]
                    ]
        DoneLoading dex -> Html.map PokedexMsg <| Pokedex.view dex
