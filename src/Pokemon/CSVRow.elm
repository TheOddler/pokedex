module Pokemon.CSVRow exposing (..)

import Ability exposing (Ability)
import Csv.Decode as Decode exposing (Decoder)
import Maybe.Extra as Maybe
import Type exposing (Type)


type alias PokemonCSVRow =
    { id : Int
    , name : String
    , alternateFormName : Maybe String
    , fullName : Maybe String
    , originalPokemonID : Maybe Int
    , primaryType : Type
    , secondaryType : Maybe Type
    , ability : Maybe Ability
    , evolvesFromID : List Int
    , evolutionDetails : Maybe String
    , transformGroupID : Maybe Int
    , transformGroupDetails : Maybe String
    , imageGen : Maybe String
    , imageID : Maybe String
    }


decoder : Decoder PokemonCSVRow
decoder =
    Decode.into PokemonCSVRow
        |> Decode.pipeline (Decode.field "ID" Decode.int)
        |> Decode.pipeline (Decode.field "Name" Decode.string)
        |> Decode.pipeline (Decode.field "Alternate Form Name" (Decode.blank Decode.string))
        |> Decode.pipeline (Decode.field "Full Name" (Decode.blank Decode.string))
        |> Decode.pipeline (Decode.field "Original Pokemon" (Decode.blank Decode.int))
        |> Decode.pipeline (Decode.field "Primary Type" Type.decoder)
        |> Decode.pipeline (Decode.field "Secondary Type" (Decode.blank Type.decoder))
        |> Decode.pipeline (Decode.field "Ability" (Decode.blank Ability.decoder))
        |> Decode.pipeline (Decode.field "Evolves From" intListDecoder)
        |> Decode.pipeline (Decode.field "Evolution Details" (Decode.blank Decode.string))
        |> Decode.pipeline (Decode.field "Transform Group" (Decode.blank Decode.int))
        |> Decode.pipeline (Decode.field "Transform Details" (Decode.blank Decode.string))
        |> Decode.pipeline (Decode.field "Image Gen" (Decode.blank Decode.string))
        |> Decode.pipeline (Decode.field "Image ID" (Decode.blank Decode.string))


intListDecoder : Decoder (List Int)
intListDecoder =
    Decode.andThen
        (\maybeString ->
            let
                strings =
                    case maybeString of
                        Nothing ->
                            []

                        Just string ->
                            String.split ";" string

                listMaybeInts =
                    List.map String.toInt strings

                maybeIntsList =
                    Maybe.combine listMaybeInts
            in
            case maybeIntsList of
                Nothing ->
                    Decode.fail <| "Failed parsing int list"

                Just ints ->
                    Decode.succeed ints
        )
        (Decode.blank Decode.string)
