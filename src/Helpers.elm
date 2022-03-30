module Helpers exposing (..)

import Html.Styled as Html exposing (Attribute)
import Html.Styled.Events exposing (stopPropagationOn)
import Json.Decode as Json


stopPropagationOnClick : msg -> Attribute msg
stopPropagationOnClick msg =
    stopPropagationOn "click" (Json.succeed ( msg, True ))
