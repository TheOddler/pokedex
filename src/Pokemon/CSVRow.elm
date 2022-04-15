module Pokemon.CSVRow exposing (..)

import Csv.Decode as Decode exposing (Decoder)
import Maybe.Extra as Maybe
import Type exposing (Type)


type alias PokemonCSVRow =
    { id : Int
    , speciesID : Int
    , name : String
    , alternateFormName : Maybe String
    , fullName : Maybe String
    , originalPokemonID : Maybe Int
    , primaryType : Type
    , secondaryType : Maybe Type
    , evolvesFromID : List Int
    , evolutionDetails : Maybe String
    , transformGroupID : Maybe Int
    , transformGroupDetails : Maybe String
    , normal : Float
    , fire : Float
    , water : Float
    , electric : Float
    , grass : Float
    , ice : Float
    , fighting : Float
    , poison : Float
    , ground : Float
    , flying : Float
    , psychic : Float
    , bug : Float
    , rock : Float
    , ghost : Float
    , dragon : Float
    , dark : Float
    , steel : Float
    , fairy : Float
    , imageGen : Maybe String
    , imageID : Maybe String
    }


decoder : Decoder PokemonCSVRow
decoder =
    Decode.into PokemonCSVRow
        |> Decode.pipeline (Decode.field "ID" Decode.int)
        |> Decode.pipeline (Decode.field "Species ID" Decode.int)
        |> Decode.pipeline (Decode.field "Name" Decode.string)
        |> Decode.pipeline (Decode.field "Alternate Form Name" (Decode.blank Decode.string))
        |> Decode.pipeline (Decode.field "Full Name" (Decode.blank Decode.string))
        |> Decode.pipeline (Decode.field "Original Pokemon" (Decode.blank Decode.int))
        |> Decode.pipeline (Decode.field "Primary Type" Type.decoder)
        |> Decode.pipeline (Decode.field "Secondary Type" (Decode.blank Type.decoder))
        |> Decode.pipeline (Decode.field "Evolves From" intListDecoder)
        |> Decode.pipeline (Decode.field "Evolution Details" (Decode.blank Decode.string))
        |> Decode.pipeline (Decode.field "Transform Group" (Decode.blank Decode.int))
        |> Decode.pipeline (Decode.field "Transform Details" (Decode.blank Decode.string))
        |> Decode.pipeline (Decode.field "Normal" Decode.float)
        |> Decode.pipeline (Decode.field "Fire" Decode.float)
        |> Decode.pipeline (Decode.field "Water" Decode.float)
        |> Decode.pipeline (Decode.field "Electric" Decode.float)
        |> Decode.pipeline (Decode.field "Grass" Decode.float)
        |> Decode.pipeline (Decode.field "Ice" Decode.float)
        |> Decode.pipeline (Decode.field "Fighting" Decode.float)
        |> Decode.pipeline (Decode.field "Poison" Decode.float)
        |> Decode.pipeline (Decode.field "Ground" Decode.float)
        |> Decode.pipeline (Decode.field "Flying" Decode.float)
        |> Decode.pipeline (Decode.field "Psychic" Decode.float)
        |> Decode.pipeline (Decode.field "Bug" Decode.float)
        |> Decode.pipeline (Decode.field "Rock" Decode.float)
        |> Decode.pipeline (Decode.field "Ghost" Decode.float)
        |> Decode.pipeline (Decode.field "Dragon" Decode.float)
        |> Decode.pipeline (Decode.field "Dark" Decode.float)
        |> Decode.pipeline (Decode.field "Steel" Decode.float)
        |> Decode.pipeline (Decode.field "Fairy" Decode.float)
        |> Decode.pipeline (Decode.field "Image Gen" (Decode.blank Decode.string))
        |> Decode.pipeline (Decode.field "Image ID" (Decode.blank Decode.string))


effectivenessAgainst : PokemonCSVRow -> List ( Type, Float )
effectivenessAgainst pkm =
    [ ( Type.Normal, pkm.normal )
    , ( Type.Fire, pkm.fire )
    , ( Type.Water, pkm.water )
    , ( Type.Electric, pkm.electric )
    , ( Type.Grass, pkm.grass )
    , ( Type.Ice, pkm.ice )
    , ( Type.Fighting, pkm.fighting )
    , ( Type.Poison, pkm.poison )
    , ( Type.Ground, pkm.ground )
    , ( Type.Flying, pkm.flying )
    , ( Type.Psychic, pkm.psychic )
    , ( Type.Bug, pkm.bug )
    , ( Type.Rock, pkm.rock )
    , ( Type.Ghost, pkm.ghost )
    , ( Type.Dragon, pkm.dragon )
    , ( Type.Dark, pkm.dark )
    , ( Type.Steel, pkm.steel )
    , ( Type.Fairy, pkm.fairy )
    ]


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
