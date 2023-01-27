module Ability exposing (Ability(..), Effect(..), getEffect)

import Type exposing (Type(..))


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


type Effect
    = None
    | Multiply Float
    | Override Float
    | Calc (Float -> Float)


getEffect : Type -> Ability -> Effect
getEffect attacker defenderAbility =
    case ( attacker, defenderAbility ) of
        ( Electric, DeltaStream ) ->
            Override 1

        ( Ice, DeltaStream ) ->
            Override 1

        ( Rock, DeltaStream ) ->
            Override 1

        ( Water, DesolateLand ) ->
            Override 0

        ( Water, DrySkin ) ->
            Override 0

        ( Fire, DrySkin ) ->
            Multiply 1.25

        ( Fire, FlashFire ) ->
            Override 0

        ( Ground, Levitate ) ->
            Override 0

        ( Electric, LightningRod ) ->
            Override 0

        ( Electric, MotorDrive ) ->
            Override 0

        ( Fire, PrimordialSea ) ->
            Override 0

        ( _, PrismArmor ) ->
            let
                weakerSuperEffectiveMoves eff =
                    if eff > 1.1 then
                        eff * 0.75

                    else
                        eff
            in
            Calc weakerSuperEffectiveMoves

        ( Grass, SapSipper ) ->
            Override 0

        ( Fire, ThickFat ) ->
            Multiply 0.5

        ( Ice, ThickFat ) ->
            Multiply 0.5

        ( Electric, VoltAbsorb ) ->
            Override 0

        ( Water, WaterAbsorb ) ->
            Override 0

        ( Fire, WaterBubble ) ->
            Multiply 0.5

        ( _, WonderGuard ) ->
            let
                onlyDamageFromSuperEffective eff =
                    if eff > 1.1 then
                        eff

                    else
                        0
            in
            Calc onlyDamageFromSuperEffective

        ( _, _ ) ->
            None
