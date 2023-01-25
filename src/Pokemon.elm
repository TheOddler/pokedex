module Pokemon exposing (..)

import Ability exposing (Ability)
import Css exposing (..)
import List.Extra as List
import Type exposing (Type(..), Typing(..))


type EvolutionData
    = DoesNotEvolve
    | EvolvesFrom (List Int) String


type TransformationData
    = DoesNotTransform
    | Transforms Int String


type alias Pokemon =
    { id : Int
    , nationalDexNumber : Int
    , originalPokemonID : Maybe Int
    , fullName : String
    , typing : Typing
    , ability : Maybe Ability
    , imageUrl : String
    , evolutionData : EvolutionData
    , transformationData : TransformationData
    }


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


withID : List Pokemon -> Int -> Maybe Pokemon
withID all id =
    List.find (\p -> p.id == id) all
