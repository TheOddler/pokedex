module Pokemon.Details exposing (..)

import Css exposing (..)
import Css.Transitions as Transitions exposing (transition)
import Helpers exposing (stopPropagationOnClick)
import Html.Styled exposing (Html, button, div, figcaption, figure, img, text)
import Html.Styled.Attributes exposing (alt, css, src)
import LocalStorage
import Maybe.Extra as Maybe
import Pokemon exposing (EvolutionData(..), Pokemon, TransformationData(..), shareTransformGroup, withID)
import Pokemon.Mode as Mode exposing (Mode(..))
import Pokemon.SharedStyles as SharedStyles
import Type exposing (Typing(..))
import TypeEffectiveness


type Msg
    = Select Pokemon
    | Deselect
    | ChangeMode Mode


type alias Model =
    { pokemon : Pokemon
    , mode : Mode
    , visible : Bool -- this allows us to always draw the view for nicer animations
    }


init : Maybe Mode -> Pokemon -> Model
init mode first =
    { pokemon = first
    , mode = Maybe.withDefault Mode.Evolutions mode
    , visible = False
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Select p ->
            if model.visible == True && model.pokemon == p then
                update Deselect model

            else
                ( { model | pokemon = p, visible = True }, Cmd.none )

        Deselect ->
            ( { model | visible = False }, Cmd.none )

        ChangeMode mode ->
            ( { model | mode = mode }, saveMode mode )


saveMode : Mode -> Cmd Msg
saveMode mode =
    LocalStorage.save LocalStorage.modeKey <| Mode.toString mode


view : List Pokemon -> Model -> Html Msg
view allPkm model =
    let
        pkm =
            model.pokemon

        viewBadgeWithEff ( t, e ) =
            Type.viewBadge t (Just e)

        evolvesFrom =
            case pkm.evolutionData of
                DoesNotEvolve ->
                    []

                EvolvesFrom ids _ ->
                    Maybe.values <| List.map (withID allPkm) ids

        evolvesIntoIDs =
            List.map .id <| List.filter (Pokemon.evolvesFrom pkm) allPkm

        otherIDsInTransformGroup =
            List.map .id <| List.filter (\p -> shareTransformGroup pkm p && p.id /= pkm.id) allPkm

        -- Maybe.andThen (\i -> Dict.get i allPkm) pkm.evolvesFromID
        evolvesInto =
            Maybe.values <| List.map (withID allPkm) evolvesIntoIDs

        transformsInto =
            Maybe.values <| List.map (withID allPkm) otherIDsInTransformGroup

        mainView =
            figure
                []
                [ img
                    [ src <| pkm.imageUrl
                    , alt pkm.fullName
                    , css
                        [ width (rem 8)
                        , height (rem 8)
                        , property "object-fit" "contain"
                        ]
                    ]
                    []
                , figcaption [ css [ fontSize (Css.em 2) ] ] [ text pkm.fullName ]
                ]

        viewBadge p =
            div
                [ stopPropagationOnClick <| Select p
                , css
                    [ Type.backgroundFor p.typing
                    , SharedStyles.badgeStyle
                    , property "box-shadow" "inset 0 -2px 2px rgba(0, 0, 0, 0.2), inset 0 2px 2px rgba(255, 255, 255, 0.2), 0px 1px 1px 1px rgba(0, 0, 0, 0.15);"
                    , paddingTop (rem 0.5)
                    ]
                ]
                [ img
                    [ src p.imageUrl
                    , alt p.fullName
                    , css
                        [ width (rem 6)
                        , height (rem 6)
                        , property "object-fit" "contain"
                        ]
                    ]
                    []
                , div [ css [ fontSize (em 1) ] ] [ text p.fullName ]
                ]

        evolutionDetailsToString p =
            case p.evolutionData of
                DoesNotEvolve ->
                    ""

                EvolvesFrom _ details ->
                    details

        transformationDetailsToString p =
            case p.transformationData of
                DoesNotTransform ->
                    ""

                Transforms _ details ->
                    details

        viewEvolution isPrevolution info p =
            let
                children =
                    [ viewBadge p
                    , div [ css [ fontSize (em 0.9) ] ] [ text info ]
                    ]
            in
            div
                [ css
                    [ display inlineBlock
                    , whiteSpace normal
                    , width (rem 7)
                    , margin (em 0.2)
                    ]
                ]
            <|
                if isPrevolution then
                    children

                else
                    List.reverse children

        effectivenessList =
            TypeEffectiveness.getAll pkm.typing pkm.ability

        isSuperEffective ( _, eff ) =
            eff > 1.1

        isNotVeryEffective ( _, eff ) =
            eff < 0.9

        superEffective =
            List.filter isSuperEffective effectivenessList

        notVeryEffective =
            List.filter isNotVeryEffective effectivenessList

        typeEffectivenessView =
            div []
                [ div [] <|
                    case pkm.typing of
                        Single type_ ->
                            [ Type.viewBadge type_ Nothing ]

                        Double first second ->
                            [ Type.viewBadge first Nothing, Type.viewBadge second Nothing ]
                , div [ css [ effectivenessChartTitleStyle ] ]
                    [ text ("Super effective against " ++ pkm.fullName ++ ":") ]
                , div [ css [ effectivenessChartStyle ] ] <|
                    List.map viewBadgeWithEff superEffective
                , div [ css [ effectivenessChartTitleStyle ] ]
                    [ text ("Not very effective against " ++ pkm.fullName ++ ":") ]
                , div [ css [ effectivenessChartStyle ] ] <|
                    List.map viewBadgeWithEff notVeryEffective
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

        typeEffectivenessButton =
            modeButton "show Type Effectiveness" TypeEffectiveness

        evolutionsButton =
            let
                label =
                    case ( evolvesInto, transformsInto ) of
                        ( [], [] ) ->
                            "show Evolutions"

                        ( _, [] ) ->
                            "show Evolutions"

                        ( [], _ ) ->
                            "show Transformations"

                        ( _, _ ) ->
                            "show Transformations & Evolutions"
            in
            modeButton label Evolutions

        -- The button is fake because you can close by clicking anywhere really.
        fakeCloseButton =
            let
                size =
                    1.7

                thickness =
                    0.2

                barStyle degrees =
                    [ position absolute
                    , left (em <| size / 2)
                    , height (em size)
                    , width (em thickness)
                    , property "content" "' '"
                    , backgroundColor <| rgb 0 0 0
                    , transform <| rotate (deg degrees)
                    ]
            in
            div
                [ css
                    [ position absolute
                    , top (em 0.5)
                    , right (em 0.5)
                    , opacity (num 0.3)
                    , width (em size)
                    , height (em size)
                    , before <| barStyle 45
                    , after <| barStyle -45
                    , hover [ opacity (num 1) ]
                    , cursor pointer
                    ]
                ]
                []
    in
    div
        [ stopPropagationOnClick Deselect
        , css
            [ Type.backgroundFor pkm.typing
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
                if model.visible then
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
        case model.mode of
            TypeEffectiveness ->
                [ mainView
                , typeEffectivenessView
                , evolutionsButton
                , fakeCloseButton
                ]

            Evolutions ->
                [ wrapEvolutionListView <|
                    List.map (viewEvolution True <| evolutionDetailsToString pkm) evolvesFrom
                , mainView
                , wrapEvolutionListView <|
                    if evolvesFrom == [] && evolvesInto == [] && transformsInto == [] then
                        [ text "Does not evolve" ]

                    else
                        List.concat
                            [ List.map (\p -> viewEvolution False (transformationDetailsToString p) p) transformsInto
                            , List.map (\p -> viewEvolution False (evolutionDetailsToString p) p) evolvesInto
                            ]
                , typeEffectivenessButton
                , fakeCloseButton
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
