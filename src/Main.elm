module Main exposing (..)

import Browser
import Css exposing (..)
import Html.Styled as Html exposing (Html, a, div, h1, h2, text, toUnstyled)
import Html.Styled.Attributes exposing (css, href)
import Http
import LocalStorage exposing (LocalStorage)
import Pokedex exposing (Pokedex)


type Model
    = Loading
    | Running Pokedex
    | Error String String


type Msg
    = DoneLoading LocalStorage (Result Http.Error String)
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
    ( Loading
    , Http.get
        { url = "data/pokemon.csv"
        , expect = Http.expectString (DoneLoading localStorage)
        }
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PokedexMsg pokedexMsg ->
            case model of
                Loading ->
                    ( model, Cmd.none )

                Error _ _ ->
                    ( model, Cmd.none )

                Running pokedex ->
                    let
                        ( updatedPokedex, nextPokedexMsg ) =
                            Pokedex.update pokedexMsg pokedex
                    in
                    ( Running updatedPokedex, Cmd.map PokedexMsg nextPokedexMsg )

        DoneLoading localStorage errorOrCsv ->
            case errorOrCsv of
                Err err ->
                    ( Error "Failed getting Pokémon CSV" <| httpErrorToString err, Cmd.none )

                Ok csv ->
                    case Pokedex.init localStorage csv of
                        Err err ->
                            ( Error "Failed initilizing Pokedex" err, Cmd.none )

                        Ok pokedex ->
                            ( Running pokedex, Cmd.none )


view : Model -> Html Msg
view model =
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
        [ case model of
            Loading ->
                h1 [ infoCss ] [ text "Loading..." ]

            Error title message ->
                div [ infoCss ]
                    [ h1 [] [ text <| title ]
                    , a
                        [ href <| "mailto:pablo.bollansee@gmail.com?Subject=Error in Pokedex: " ++ title ++ "&body=" ++ message
                        ]
                        [ h2 [] [ text "Click here to report this error." ]
                        ]
                    , div [] [ text <| message ]
                    ]

            Running pokedex ->
                Html.map PokedexMsg <| Pokedex.view pokedex
        , div
            [ css
                [ marginTop auto ]
            ]
            [ text "Images from "
            , a [ href "https://github.com/PokeAPI/sprites" ] [ text "PokeAPI/sprites" ]
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
