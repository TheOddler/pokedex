module Pokemon exposing (Pokemon, fromCSVRows, view, viewDetail)

import Dict exposing (Dict)
import Html exposing (Html, div, figcaption, figure, img, text)
import Html.Attributes exposing (class, src)
import Maybe.Extra as Maybe
import PokemonCSVRow exposing (PokemonCSVRow)
import Type exposing (Type, backgroundFor, viewBadge)


type alias Pokemon =
    { id : Int
    , fullName : String
    , types : List Type
    , superEffective : List ( Type, Float )
    , notVeryEffective : List ( Type, Float )
    , imageUrl : String
    , evolvesFromID : Maybe Int
    , evolvesFromDetails : Maybe String
    , evolvesIntoIDs : List Int
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


viewDetail : Dict Int Pokemon -> Pokemon -> Html msg
viewDetail allPkm pkm =
    let
        viewBadgeWE ( t, e ) =
            viewBadge t (Just e)

        viewBadgeWoE t =
            viewBadge t Nothing

        maybeEvolvesFrom =
            case pkm.evolvesFromID of
                Just evolvesFromID ->
                    Dict.get evolvesFromID allPkm

                Nothing ->
                    Nothing

        evolvesInto =
            Maybe.values <| List.map (\i -> Dict.get i allPkm) pkm.evolvesIntoIDs

        viewPkmPortrait p class_ caption =
            figure
                [ class class_
                ]
                [ img
                    [ src <| p.imageUrl
                    ]
                    []
                , figcaption
                    [ class "name"
                    ]
                    [ text <| caption
                    ]
                ]
    in
    div
        [ class "details"
        , backgroundFor pkm.types
        ]
        [ div [ class "evolutions " ] <|
            Maybe.values
                [ case maybeEvolvesFrom of
                    Nothing ->
                        Nothing

                    Just evolvesFrom ->
                        Just <| viewPkmPortrait evolvesFrom "evolvesFrom" (Maybe.withDefault "" pkm.evolvesFromDetails)
                , Just <| viewPkmPortrait pkm "pokemon" pkm.fullName
                , case evolvesInto of
                    [] ->
                        Nothing

                    evolvesInto_ ->
                        Just <| div [ class "evolvesInto" ] <| List.map (\p -> viewPkmPortrait p "evolvesIntoItem" (Maybe.withDefault "" p.evolvesFromDetails)) evolvesInto
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


fromCSVRows : List PokemonCSVRow -> List Pokemon
fromCSVRows pkmCSVRows =
    List.map (fromCSVRow pkmCSVRows) pkmCSVRows


fromCSVRow : List PokemonCSVRow -> PokemonCSVRow -> Pokemon
fromCSVRow pkmCSVRows pkm =
    let
        effectivenessList =
            PokemonCSVRow.effectivenessAgainst pkm
    in
    { id = pkm.id
    , fullName =
        case pkm.alternateFormName of
            Just alternateFormName ->
                pkm.name ++ " (" ++ alternateFormName ++ ")"

            Nothing ->
                pkm.name
    , types = pkm.primaryType :: Maybe.toList pkm.secondaryType
    , superEffective = List.filter isSuperEffective effectivenessList
    , notVeryEffective = List.filter isNotVeryEffective effectivenessList
    , imageUrl = "https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/" ++ pkm.image ++ ".png"
    , evolvesFromID = pkm.evolvesFromID
    , evolvesFromDetails = pkm.evolutionDetails
    , evolvesIntoIDs = List.map .id <| List.filter (\p -> p.evolvesFromID == Just pkm.id) pkmCSVRows
    }


isSuperEffective : ( Type, Float ) -> Bool
isSuperEffective ( _, eff ) =
    eff >= 1.5


isNotVeryEffective : ( Type, Float ) -> Bool
isNotVeryEffective ( _, eff ) =
    eff < 0.75
