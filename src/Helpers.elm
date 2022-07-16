module Helpers exposing (..)

import Html.Styled as Html
import Html.Styled.Events exposing (stopPropagationOn)
import Json.Decode as Json


stopPropagationOnClick : msg -> Html.Attribute msg
stopPropagationOnClick msg =
    stopPropagationOn "click" (Json.succeed ( msg, True ))
