module Pokemon exposing (Pokemon, fromCSVRows, view, viewDetail)

import Css exposing (..)
import Css.Transitions as Transitions exposing (cubicBezier, transition)
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html, div, figcaption, figure, img, text)
import Html.Styled.Attributes exposing (css, src)
import Html.Styled.Events exposing (onClick)
import Maybe.Extra as Maybe
import PokemonCSVRow exposing (PokemonCSVRow)
import String exposing (toLower)
import Type exposing (Type(..), Typing(..), backgroundFor, viewBadge)


type alias Pokemon =
    { id : Int
    , fullName : String
    , typing : Typing
    , superEffective : List ( Type, Float )
    , notVeryEffective : List ( Type, Float )
    , imageUrl : String
    , evolvesFromID : Maybe Int
    , evolvesFromDetails : Maybe String
    , evolvesIntoIDs : List Int
    }


view : (Pokemon -> msg) -> String -> Pokemon -> Html msg
view selectMsg searchString pkm =
    div
        [ onClick <| selectMsg pkm
        , css <|
            if String.contains (toLower searchString) (toLower pkm.fullName) then
                [ backgroundFor pkm.typing, viewBaseStyle ]

            else
                [ backgroundFor pkm.typing, viewBaseStyle, display none ]
        ]
        [ img
            [ src pkm.imageUrl
            , css
                [ width (rem 6)
                , height (rem 6)
                ]
            ]
            []
        , div [] [ text pkm.fullName ]
        ]


viewBaseStyle : Style
viewBaseStyle =
    Css.batch
        [ margin (em 0.2)
        , borderRadius (rem 1)
        , overflow hidden
        , boxSizing borderBox
        , property "box-shadow" "inset 0 -2px 2px rgba(0, 0, 0, 0.2), inset 0 2px 2px rgba(255, 255, 255, 0.2);"
        , cursor pointer
        , width (rem 7)
        , padding4 (em 0) (em 0.2) (em 0.5) (em 0.2)
        , position relative -- to make zIndex work
        , transition
            [ Transitions.transform3 500 0 (cubicBezier 0 1 0.5 1.5)
            , Transitions.boxShadow3 500 0 (cubicBezier 0 1 0.5 1.5)
            ]
        , hover
            [ transform <| scale 1.5
            , zIndex (int 100)
            , property "box-shadow" "inset 0 -2px 2px rgba(0, 0, 0, 0.2), inset 0 2px 2px rgba(255, 255, 255, 0.2), 1px 3px 3px 3px rgba(0, 0, 0, .3);"
            ]
        ]


viewDetail : Bool -> Dict Int Pokemon -> Pokemon -> Html msg
viewDetail visible allPkm pkm =
    let
        viewBadgeWithEff ( t, e ) =
            viewBadge t (Just e)

        maybeEvolvesFrom =
            Maybe.andThen (\i -> Dict.get i allPkm) pkm.evolvesFromID

        evolvesInto =
            Maybe.values <| List.map (\i -> Dict.get i allPkm) pkm.evolvesIntoIDs

        viewPkmPortrait p =
            figure
                []
                [ img
                    [ src <| p.imageUrl
                    , css
                        [ width (px 192)
                        , height auto
                        ]
                    ]
                    []
                , figcaption
                    [ css
                        [ fontSize (em 1.5)
                        ]
                    ]
                    [ text p.fullName
                    ]
                ]
    in
    div
        [ css
            [ backgroundFor pkm.typing
            , pointerEventsAll
            , borderRadius (px 16)
            , overflow hidden
            , cursor zoomOut
            , property "box-shadow" "inset 0 -2px 2px rgba(0, 0, 0, 0.2), inset 0 2px 2px rgba(255, 255, 255, 0.2), 2px 6px 6px 6px rgba(0, 0, 0, .3);"
            , property "transition" "all 0.3s ease-in-out"
            , Css.batch <|
                if visible then
                    []

                else
                    [ opacity (int 0)
                    , transform <| scale2 0 0
                    ]
            ]
        ]
        [ viewPkmPortrait pkm
        , div [] <|
            case pkm.typing of
                Single type_ ->
                    [ viewBadge type_ Nothing ]

                Double first second ->
                    [ viewBadge first Nothing, viewBadge second Nothing ]
        , div [ css [ effectivenessChartTitleStyle ] ]
            [ text ("Super effective against " ++ pkm.fullName ++ ":") ]
        , div [ css [ effectivenessChartStyle ] ] <|
            List.map viewBadgeWithEff pkm.superEffective
        , div [ css [ effectivenessChartTitleStyle ] ]
            [ text ("Not very effective against " ++ pkm.fullName ++ ":") ]
        , div [ css [ effectivenessChartStyle ] ] <|
            List.map viewBadgeWithEff pkm.notVeryEffective
        ]


effectivenessChartTitleStyle : Style
effectivenessChartTitleStyle =
    Css.batch
        [ fontSize (pct 120)
        , display block
        , padding (em 0.5)
        ]


effectivenessChartStyle : Style
effectivenessChartStyle =
    Css.batch
        [ margin (em 0)
        , padding (em 0.5)
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
    , typing =
        case pkm.secondaryType of
            Just second ->
                Double pkm.primaryType second

            Nothing ->
                Single pkm.primaryType
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
