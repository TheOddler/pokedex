module Pokemon exposing (..)

import Ability exposing (Ability)
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (alt, attribute, class, src)
import List exposing (append)
import List.Extra as List
import Type exposing (Type(..), Typing(..))


type EvolutionData
    = IsNotEvolved
    | EvolvesFrom PokemonID String
    | EvolvesFromMultiple (List PokemonID) String


type TransformationData
    = DoesNotTransform
    | Transforms PokemonID String


type alias PokemonID =
    Int


type alias NationDexNumber =
    Int


type alias Pokemon =
    { id : PokemonID
    , nationalDexNumber : NationDexNumber
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
        IsNotEvolved ->
            False

        EvolvesFrom id _ ->
            evolution.id == id

        EvolvesFromMultiple ids _ ->
            List.member evolution.id ids


withID : List Pokemon -> PokemonID -> Maybe Pokemon
withID all id =
    List.find (\p -> p.id == id) all


viewBadge : Pokemon -> List (Html.Attribute msg) -> Html msg
viewBadge pkm additionalAttributes =
    div
        (append
            [ Type.backgroundFor pkm.typing
            , class "pokemonBadge"
            ]
            additionalAttributes
        )
        [ img
            [ src pkm.imageUrl
            , alt pkm.fullName
            , attribute "loading" "lazy"
            ]
            []
        , text pkm.fullName
        ]
