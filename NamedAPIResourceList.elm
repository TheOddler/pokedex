module NamedAPIResourceList where

import Json.Decode exposing (Decoder, (:=), int, string, list, object2, object4)
import DecodeExt exposing (nullOr)

type alias NamedAPIResourceList =
    { count: Int
    , next: Maybe String
    , previous: Maybe String
    , results: List NamedAPIResource
    }

type alias NamedAPIResource =
    { name: String
    , url: String
    }

empty : NamedAPIResourceList
empty =
    { count = 0
    , next = Nothing
    , previous = Nothing
    , results = []
    }

decoder : Decoder NamedAPIResourceList
decoder =
    object4 NamedAPIResourceList
        ("count" := int)
        ("next" := nullOr string)
        ("previous" := nullOr string)
        ("results" := list resourceDecoder)

resourceDecoder : Decoder NamedAPIResource
resourceDecoder =
    object2 NamedAPIResource
        ("name" := string)
        ("url" := string)
