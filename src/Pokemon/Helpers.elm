module Pokemon.Helpers exposing (..)

import List.Extra as List
import Pokemon exposing (..)
import Pokemon.Data


shareTransformGroup : Pokemon -> Pokemon -> Bool
shareTransformGroup p1 p2 =
    case ( p1.transformationData, p2.transformationData ) of
        ( Transforms id1 _, Transforms id2 _ ) ->
            id1 == id2

        _ ->
            False


evolvesFrom : Pokemon -> Pokemon -> Bool
evolvesFrom evolution base =
    case base.evolutionData of
        DoesNotEvolve ->
            False

        EvolvesFrom ids _ ->
            List.member evolution.id ids


withID : Int -> Maybe Pokemon
withID id =
    List.find (\p -> p.id == id) Pokemon.Data.all
