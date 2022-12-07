module Pokemon exposing (..)

import Ability exposing (Ability)
import Css exposing (..)
import Type exposing (Type(..), Typing(..))


type EvolutionData
    = DoesNotEvolve
    | EvolvesFrom (List Int) String


type TransformationData
    = DoesNotTransform
    | Transforms Int String


type alias Pokemon =
    { id : Int
    , originalPokemonID : Maybe Int
    , fullName : String
    , typing : Typing
    , ability : Maybe Ability
    , imageUrl : String
    , evolutionData : EvolutionData
    , transformationData : TransformationData
    }
