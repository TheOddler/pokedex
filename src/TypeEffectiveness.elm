module TypeEffectiveness exposing (getAll)

import Ability exposing (Ability)
import Type exposing (Type(..))


getAll : Type -> Maybe Type -> Maybe Ability -> List ( Type, Float )
getAll primaryType secondaryType ability =
    let
        calcFor t =
            attackEffectiveness t primaryType
                * Maybe.withDefault 1 (Maybe.map (attackEffectiveness t) secondaryType)
                * Maybe.withDefault 1 (Maybe.map (abilityEffectivenessMultiplier t) ability)
    in
    [ ( Type.Normal, calcFor Type.Normal )
    , ( Type.Fire, calcFor Type.Fire )
    , ( Type.Water, calcFor Type.Water )
    , ( Type.Electric, calcFor Type.Electric )
    , ( Type.Grass, calcFor Type.Grass )
    , ( Type.Ice, calcFor Type.Ice )
    , ( Type.Fighting, calcFor Type.Fighting )
    , ( Type.Poison, calcFor Type.Poison )
    , ( Type.Ground, calcFor Type.Ground )
    , ( Type.Flying, calcFor Type.Flying )
    , ( Type.Psychic, calcFor Type.Psychic )
    , ( Type.Bug, calcFor Type.Bug )
    , ( Type.Rock, calcFor Type.Rock )
    , ( Type.Ghost, calcFor Type.Ghost )
    , ( Type.Dragon, calcFor Type.Dragon )
    , ( Type.Dark, calcFor Type.Dark )
    , ( Type.Steel, calcFor Type.Steel )
    , ( Type.Fairy, calcFor Type.Fairy )
    ]


attackEffectiveness : Type -> Type -> Float
attackEffectiveness attacker defender =
    case ( attacker, defender ) of
        ( Normal, Rock ) ->
            0.5

        ( Normal, Ghost ) ->
            0

        ( Normal, Steel ) ->
            0.5

        ( Fire, Fire ) ->
            0.5

        ( Fire, Water ) ->
            0.5

        ( Fire, Grass ) ->
            2

        ( Fire, Ice ) ->
            2

        ( Fire, Bug ) ->
            2

        ( Fire, Rock ) ->
            0.5

        ( Fire, Dragon ) ->
            0.5

        ( Fire, Steel ) ->
            2

        ( Water, Fire ) ->
            2

        ( Water, Water ) ->
            0.5

        ( Water, Grass ) ->
            0.5

        ( Water, Ground ) ->
            2

        ( Water, Rock ) ->
            2

        ( Water, Dragon ) ->
            0.5

        ( Electric, Water ) ->
            2

        ( Electric, Electric ) ->
            0.5

        ( Electric, Grass ) ->
            0.5

        ( Electric, Ground ) ->
            0

        ( Electric, Flying ) ->
            2

        ( Electric, Dragon ) ->
            0.5

        ( Grass, Fire ) ->
            0.5

        ( Grass, Water ) ->
            2

        ( Grass, Grass ) ->
            0.5

        ( Grass, Poison ) ->
            0.5

        ( Grass, Ground ) ->
            2

        ( Grass, Flying ) ->
            0.5

        ( Grass, Bug ) ->
            0.5

        ( Grass, Rock ) ->
            2

        ( Grass, Dragon ) ->
            0.5

        ( Grass, Steel ) ->
            0.5

        ( Ice, Fire ) ->
            0.5

        ( Ice, Water ) ->
            0.5

        ( Ice, Grass ) ->
            2

        ( Ice, Ice ) ->
            0.5

        ( Ice, Ground ) ->
            2

        ( Ice, Flying ) ->
            2

        ( Ice, Dragon ) ->
            2

        ( Ice, Steel ) ->
            0.5

        ( Fighting, Normal ) ->
            2

        ( Fighting, Ice ) ->
            2

        ( Fighting, Poison ) ->
            0.5

        ( Fighting, Flying ) ->
            0.5

        ( Fighting, Psychic ) ->
            0.5

        ( Fighting, Bug ) ->
            0.5

        ( Fighting, Rock ) ->
            2

        ( Fighting, Ghost ) ->
            0

        ( Fighting, Dark ) ->
            2

        ( Fighting, Steel ) ->
            2

        ( Fighting, Fairy ) ->
            0.5

        ( Poison, Grass ) ->
            2

        ( Poison, Poison ) ->
            0.5

        ( Poison, Ground ) ->
            0.5

        ( Poison, Rock ) ->
            0.5

        ( Poison, Ghost ) ->
            0.5

        ( Poison, Steel ) ->
            0

        ( Poison, Fairy ) ->
            0.5

        ( Ground, Fire ) ->
            2

        ( Ground, Electric ) ->
            2

        ( Ground, Grass ) ->
            0.5

        ( Ground, Poison ) ->
            2

        ( Ground, Flying ) ->
            0

        ( Ground, Bug ) ->
            0.5

        ( Ground, Rock ) ->
            2

        ( Ground, Steel ) ->
            2

        ( Flying, Electric ) ->
            0.5

        ( Flying, Grass ) ->
            2

        ( Flying, Fighting ) ->
            2

        ( Flying, Bug ) ->
            2

        ( Flying, Rock ) ->
            0.5

        ( Flying, Steel ) ->
            0.5

        ( Psychic, Fighting ) ->
            2

        ( Psychic, Poison ) ->
            2

        ( Psychic, Psychic ) ->
            0.5

        ( Psychic, Dark ) ->
            0

        ( Psychic, Steel ) ->
            0.5

        --To continue here with BUG
        ( _, _ ) ->
            1


abilityEffectivenessMultiplier : Type -> Ability -> Float
abilityEffectivenessMultiplier attacker defenderAbility =
    case ( attacker, defenderAbility ) of
        ( _, _ ) ->
            1
