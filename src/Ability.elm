module Ability exposing (..)

import Csv.Decode as Decode exposing (Decoder)


type Ability
    = DeltaStream
    | DesolateLand
    | DrySkin
    | FlashFire
    | Levitate
    | LightningRod
    | MotorDrive
    | PrimordialSea
    | PrismArmor
    | SapSipper
    | ThickFat
    | VoltAbsorb
    | WaterAbsorb
    | WaterBubble
    | WonderGuard


decoder : Decoder Ability
decoder =
    Decode.andThen
        (\value ->
            Decode.fromMaybe (value ++ " is not a valid ability") (fromString value)
        )
        Decode.string


fromString : String -> Maybe Ability
fromString abilityStr =
    case abilityStr of
        "Delta Stream" ->
            Just DeltaStream

        "Desolate Land" ->
            Just DesolateLand

        "Dry Skin" ->
            Just DrySkin

        "Flash Fire" ->
            Just FlashFire

        "Levitate" ->
            Just Levitate

        "Lightning Rod" ->
            Just LightningRod

        "Motor Drive" ->
            Just MotorDrive

        "Primordial Sea" ->
            Just PrimordialSea

        "Prism Armor" ->
            Just PrismArmor

        "Sap Sipper" ->
            Just SapSipper

        "Thick Fat" ->
            Just ThickFat

        "Volt Absorb" ->
            Just VoltAbsorb

        "Water Absorb" ->
            Just WaterAbsorb

        "Water Bubble" ->
            Just WaterBubble

        "Wonder Guard" ->
            Just WonderGuard

        _ ->
            Nothing
