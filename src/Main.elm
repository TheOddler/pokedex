import Browser
import Html exposing (Html)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region

type alias Model =
    { searchString: String
    }


type Msg 
    = SetSearch String


init : Model
init = 
    { searchString = ""
    }


main = Browser.sandbox { init = init, update = update, view = view }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetSearch s -> {model | searchString = s}


view : Model -> Html Msg
view model = layout [Font.size 16] <|
    column [ height shrink, spacing 36, padding 10 ]
    [ row []
        [ text "Pokédex"
        , Input.text [ Input.focusedOnLoad ]
            { onChange = SetSearch
            , text = model.searchString
            , placeholder = Just (Input.placeholder [] (text "Search"))
            , label = Input.labelHidden "Search"
            }
        ]
    , wrappedRow [width shrink, centerX]
        <| List.repeat 10 (text model.searchString)
    ]
