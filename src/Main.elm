import Browser
import Html exposing (Html)
import Dict exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Element.Events as Events

import Pokemon exposing (Pokemon)
import Data.Pokemon
import Data.PokemonTypes
import Types exposing (Type)
import Data.Types
import Data.TypeEffectiveness

type alias Model =
    { searchString: String
    , pokemon: List Pokemon
    , types: Dict Int Type
    , selected: Maybe Pokemon
    }


type Msg 
    = SetSearch String
    | Select Pokemon
    | Deselect


init : Model
init = 
    { searchString = ""
    , pokemon = Pokemon.parse Data.Pokemon.csv Data.PokemonTypes.csv
    , types = Types.parse Data.Types.csv Data.TypeEffectiveness.csv
    , selected = Nothing
    }


main = Browser.sandbox { init = init, update = update, view = view }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetSearch s -> {model | searchString = s}
        Select p -> { model | selected = Just p}
        Deselect -> { model | selected = Nothing}


view : Model -> Html Msg
view model =
    layout 
        [ Font.size 16
        , Background.color (rgb255 200 200 200)
        , case model.selected of
            Just p -> inFront <| el [centerX, centerY] (Pokemon.viewDetail model.types p)
            Nothing -> onRight <| text "Click to see details"
        ]
        <| viewPokemonList model


viewPokemonList : Model -> Element Msg
viewPokemonList model =
    column 
        [ height shrink
        , spacing 36
        , padding 10
        ]
        [ row [spacing 20]
            [ text "Pokédex"
            , Input.text [ Input.focusedOnLoad ]
                { onChange = SetSearch
                , text = model.searchString
                , placeholder = Just (Input.placeholder [centerX, centerY] (text "Search"))
                , label = Input.labelHidden "Search"
                }
            ]
        , wrappedRow 
            [ spacing 16
            ]
            <| List.map (Pokemon.view Select model.types)
            <| List.filter (String.contains model.searchString << .name) model.pokemon
        ]