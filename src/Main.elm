module Main exposing (main)

import Browser
import Html exposing (Html)
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
        , view = view
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
update =
    Pokedex.update


view : Model -> Html Msg
view =
    Pokedex.view
