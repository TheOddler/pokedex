module Pokemon exposing (Mode(..), Msg(..), Pokemon, fromCSVRows, view, viewDetail)

import Css exposing (..)
import Css.Media as Media exposing (canHover, withMedia)
import Css.Transitions as Transitions exposing (cubicBezier, transition)
import Dict exposing (Dict)
import Helpers exposing (stopPropagationOnClick)
import Html exposing (p)
import Html.Styled as Html exposing (Html, button, div, figcaption, figure, img, text)
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


type Mode
    = TypeEffectiveness
    | Evolution


type Msg
    = Select Pokemon
    | Deselect
    | ChangeMode Mode


view : String -> Pokemon -> Html Msg
view searchString pkm =
    div
        [ onClick <| Select pkm
        , css <|
            [ backgroundFor pkm.typing
            , viewBadgeStyle
            , if String.contains (toLower searchString) (toLower pkm.fullName) then
                Css.batch []

              else
                display none
            , withMedia [ Media.all [ Media.hover canHover ] ]
                [ hover
                    [ transform <| scale 1.5
                    , zIndex (int 100)
                    , property "box-shadow" "inset 0 -2px 2px rgba(0, 0, 0, 0.2), inset 0 2px 2px rgba(255, 255, 255, 0.2), 1px 3px 3px 3px rgba(0, 0, 0, .3);"
                    ]
                ]
            , transition
                [ Transitions.transform3 500 0 (cubicBezier 0 1 0.5 1.5)
                , Transitions.boxShadow3 500 0 (cubicBezier 0 1 0.5 1.5)
                ]
            ]
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


viewBadgeStyle : Style
viewBadgeStyle =
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
        ]


viewDetail : Bool -> Mode -> Dict Int Pokemon -> Pokemon -> Html Msg
viewDetail visible mode allPkm pkm =
    let
        viewBadgeWithEff ( t, e ) =
            viewBadge t (Just e)

        maybeEvolvesFrom =
            Maybe.andThen (\i -> Dict.get i allPkm) pkm.evolvesFromID

        evolvesInto =
            Maybe.values <| List.map (\i -> Dict.get i allPkm) pkm.evolvesIntoIDs

        mainView =
            figure
                []
                [ img
                    [ src <| pkm.imageUrl
                    , css
                        [ width (px 192)
                        , height auto
                        ]
                    ]
                    []
                , figcaption [ css [ fontSize (em 2) ] ] [ text pkm.fullName ]
                ]

        viewEvolition info p =
            div
                [ stopPropagationOnClick <| Select p
                , css
                    [ backgroundFor p.typing
                    , viewBadgeStyle
                    , display inlineBlock
                    , whiteSpace normal
                    , property "box-shadow" "inset 0 -2px 2px rgba(0, 0, 0, 0.2), inset 0 2px 2px rgba(255, 255, 255, 0.2), 0px 1px 1px 1px rgba(0, 0, 0, 0.15);"
                    ]
                ]
                [ img
                    [ src p.imageUrl
                    , css
                        [ width (px 96)
                        , height auto
                        ]
                    ]
                    []
                , div [ css [ fontSize (em 1) ] ] [ text p.fullName ]
                , div [ css [ fontSize (em 0.9) ] ] [ text info ]
                ]

        typeEffectivenessView =
            div []
                [ div [] <|
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

        wrapEvolutionListView =
            div
                [ css
                    [ overflowX auto
                    , width (pct 100)
                    , whiteSpace noWrap
                    , textAlign center
                    ]
                ]

        modeButton text_ toMode =
            button
                [ stopPropagationOnClick (ChangeMode toMode)
                , css
                    [ marginTop (em 1)
                    , fontSize (em 1)
                    , textAlign center
                    , padding2 (em 0.5) (em 1)
                    , borderRadius (em 5)
                    , border (px 0)
                    , backgroundColor (rgba 255 255 255 0.4)
                    , cursor pointer
                    ]
                ]
                [ text text_ ]
    in
    div
        [ onClick Deselect
        , css
            [ backgroundFor pkm.typing
            , position fixed
            , left (pct 50)
            , maxWidth (pct 95)
            , top (pct 50)
            , height auto
            , zIndex (int 200)
            , pointerEventsAll
            , borderRadius (px 16)
            , overflow hidden
            , cursor zoomOut
            , property "box-shadow" "inset 0 -2px 2px rgba(0, 0, 0, 0.2), inset 0 2px 2px rgba(255, 255, 255, 0.2), 2px 6px 6px 6px rgba(0, 0, 0, .3);"
            , paddingTop (em 0.8)
            , paddingBottom (em 0.8)
            , transition
                [ Transitions.transform 300
                , Transitions.opacity 300
                ]
            , Css.batch <|
                if visible then
                    [ transforms
                        [ translate2 (pct -50) (pct -50)
                        , scale2 1 1 -- must be set explicitly for the transition to work
                        ]
                    ]

                else
                    [ transforms
                        [ translate2 (pct -50) (pct -50)
                        , scale2 0 0
                        ]
                    , opacity (int 0)
                    ]
            ]
        ]
    <|
        case mode of
            TypeEffectiveness ->
                [ mainView
                , typeEffectivenessView
                , modeButton "show Evolutions" Evolution
                ]

            Evolution ->
                [ wrapEvolutionListView <| Maybe.values [ Maybe.map (viewEvolition <| Maybe.withDefault "" pkm.evolvesFromDetails) maybeEvolvesFrom ]
                , mainView
                , wrapEvolutionListView <| List.map (\p -> viewEvolition (Maybe.withDefault "" p.evolvesFromDetails) p) evolvesInto
                , modeButton "show Type Effectiveness" TypeEffectiveness
                ]


effectivenessChartTitleStyle : Style
effectivenessChartTitleStyle =
    Css.batch
        [ fontSize (pct 120)
        , display block
        , margin (em 0.5)
        ]


effectivenessChartStyle : Style
effectivenessChartStyle =
    Css.batch
        [ margin (em 0)
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
