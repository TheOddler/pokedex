import Browser
import Html exposing (Html, div, text, input)
import Html.Attributes exposing (class, id, placeholder, value, classList)
import Html.Events exposing (onInput, onClick)
import Http

import Pokedex exposing (Pokedex)


type Model 
    = Loading (Maybe String) (Maybe String) (Maybe String) (Maybe String)
    | DoneLoading Pokedex
    | Error Http.Error


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
                        Err err -> (Error err, Cmd.none)
                        
                GotPokemonTypesCsv result ->
                    case result of
                        Ok csv -> (checkLoadingDone <| Loading p (Just csv) t te, Cmd.none)
                        Err err -> (Error err, Cmd.none)
                        
                GotTypesCsv result ->
                    case result of
                        Ok csv -> (checkLoadingDone <| Loading p ptt (Just csv) te, Cmd.none)
                        Err err -> (Error err, Cmd.none)
                        
                GotTypeEffectivenessCsv result ->
                    case result of
                        Ok csv -> (checkLoadingDone <| Loading p ptt t (Just csv), Cmd.none)
                        Err err -> (Error err, Cmd.none)

                PokedexMsg pmsg -> (model, Cmd.none)

        DoneLoading dex ->
            case msg of
                PokedexMsg m -> (DoneLoading <| Pokedex.update m dex, Cmd.none)
                _ -> (model, Cmd.none)

        Error err -> (model, Cmd.none)


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
        Error error ->
            div [ id "error" ]
                [ case error of 
                    Http.BadUrl url -> text <| "Error, bad url: " ++ url
                    Http.Timeout -> text <| "Error, timeout"
                    Http.NetworkError -> text <| "Error, network error"
                    Http.BadStatus status -> text <| "Error, bad status: " ++ String.fromInt status
                    Http.BadBody body -> text <| "Error, bad body: " ++ body
                ]
        DoneLoading dex -> Html.map PokedexMsg <| Pokedex.view dex
