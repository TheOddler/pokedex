import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

type alias Model = (Int, Int)

type Msg = ChangeFirst Int | ChangeSecond Int

init : Model
init = (0, 0)

main = Browser.sandbox { init = init, update = update, view = view }


update : Msg -> Model -> Model
update msg (first, second) =
    case msg of
        ChangeFirst i -> (first + i, second)

        ChangeSecond i -> (first, second + i)


view : Model -> Html Msg
view (first, second) =
    div []
        [ div []
            [ button [ onClick <| ChangeFirst -1 ] [ text "-" ]
            , div [] [ text (String.fromInt first) ]
            , button [ onClick <| ChangeFirst 1 ] [ text "+" ]
            ]
        , div []
            [ button [ onClick <| ChangeSecond -1 ] [ text "-" ]
            , div [] [ text (String.fromInt second) ]
            , button [ onClick <| ChangeSecond 1 ] [ text "+" ]
            ]
        ]
