module NamedAPIResource where

import Json.Decode exposing (Decoder, (:=), int, string, list, object2, object4)
import DecodeExt exposing (nullOr)

type alias NamedAPIResource =
    { name: String
    , url: String
    }

decoder : Decoder NamedAPIResource
decoder =
    object2 NamedAPIResource
        ("name" := string)
        ("url" := string)
