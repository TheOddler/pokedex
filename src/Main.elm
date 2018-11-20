import Browser
import Html exposing (Html)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region

import Pokemon exposing (..)

type alias Model =
    { searchString: String
    }


type Msg 
    = SetSearch String


init : Model
init = 
    { searchString = ""
    }


main = Browser.sandbox { init = init, update = update, view = htmlView }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetSearch s -> {model | searchString = s}


htmlView : Model -> Html Msg
htmlView model = 
    layout 
        [ Font.size 16
        , Background.color (rgb255 200 200 200)
        ] 
        (view model)


view : Model -> Element Msg
view model =
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
                , placeholder = Just (Input.placeholder [] (text "Search"))
                , label = Input.labelHidden "Search"
                }
            ]
        , wrappedRow [ spacing 16 ]
            <| List.map Pokemon.view
            <| List.filter (String.contains model.searchString << .name) allPokemon
        ]

