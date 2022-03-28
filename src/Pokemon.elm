module Pokemon exposing (Pokemon, decoder, view, viewDetail)

import Csv.Decode as Decode exposing (Decoder)
import Html exposing (Html, div, figcaption, figure, img, text)
import Html.Attributes exposing (class, src)
import Maybe.Extra as Maybe
import PokemonCSVRow exposing (PokemonCSVRow)
import Type exposing (Type, backgroundFor, viewBadge)


type alias Pokemon =
    { fullName : String
    , types : List Type
    , superEffective : List ( Type, Float )
    , notVeryEffective : List ( Type, Float )
    , imageUrl : String
    }


view : Pokemon -> Html msg
view pkm =
    figure
        [ backgroundFor <| pkm.types
        , class "pokemon"
        ]
        [ img
            [ src <| pkm.imageUrl
            ]
            []
        , figcaption
            [ class "name"
            ]
            [ text <| pkm.fullName
            ]
        ]


viewDetail : Pokemon -> Html msg
viewDetail pkm =
    let
        viewBadgeWE ( t, e ) =
            viewBadge t (Just e)

        viewBadgeWoE t =
            viewBadge t Nothing
    in
    div
        [ class "details"
        , backgroundFor pkm.types
        ]
        [ figure
            [ class "pokemon"
            ]
            [ img
                [ src <| pkm.imageUrl
                ]
                []
            , figcaption
                [ class "name"
                ]
                [ text <| pkm.fullName
                ]
            ]
        , div [ class "typeChart" ] <|
            List.map viewBadgeWoE pkm.types
        , div [ class "effectivenessChartTitle" ] [ text ("Super effective against " ++ pkm.fullName ++ ":") ]
        , div [ class "superEffectiveChart" ] <|
            List.map viewBadgeWE pkm.superEffective
        , div [ class "effectivenessChartTitle" ] [ text ("Not very effective against " ++ pkm.fullName ++ ":") ]
        , div [ class "notVeryEffectiveChart" ] <|
            List.map viewBadgeWE pkm.notVeryEffective
        ]


decoder : Decoder Pokemon
decoder =
    Decode.map pokemonFromCSVRow PokemonCSVRow.decoder


pokemonFromCSVRow : PokemonCSVRow -> Pokemon
pokemonFromCSVRow pkm =
    let
        effectivenessList =
            PokemonCSVRow.effectivenessAgainst pkm
    in
    { fullName =
        case pkm.alternateFormName of
            Just alternateFormName ->
                pkm.name ++ " (" ++ alternateFormName ++ ")"

            Nothing ->
                pkm.name
    , types = pkm.primaryType :: Maybe.toList pkm.secondaryType
    , superEffective = List.filter isSuperEffective effectivenessList
    , notVeryEffective = List.filter isNotVeryEffective effectivenessList
    , imageUrl = "https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/" ++ pkm.image ++ ".png"
    }


isSuperEffective : ( Type, Float ) -> Bool
isSuperEffective ( _, eff ) =
    eff >= 1.5


isNotVeryEffective : ( Type, Float ) -> Bool
isNotVeryEffective ( _, eff ) =
    eff < 0.75
