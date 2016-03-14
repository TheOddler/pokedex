module HttpExt where

import Http
import Effects exposing (Effects, Never)
import Task exposing (Task)
import Json.Decode exposing (Decoder)

fetch : Decoder a -> String -> (Result Http.Error a -> b) -> Effects b
fetch decoder url action =
    Http.get decoder url
    |> Task.toResult
    |> Task.map action
    |> Effects.task
