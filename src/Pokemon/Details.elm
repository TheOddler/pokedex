module Pokemon.Details exposing (..)

import Css exposing (..)
import Css.Transitions as Transitions exposing (transition)
import Dict exposing (Dict)
import Helpers exposing (stopPropagationOnClick)
import Html.Styled exposing (Html, button, div, figcaption, figure, img, text)
import Html.Styled.Attributes exposing (css, src)
import LocalStorage exposing (LocalStorage)
import Maybe.Extra as Maybe
import Pokemon exposing (Pokemon)
import Pokemon.Mode as Mode exposing (Mode(..))
import Pokemon.SharedStyles as SharedStyles
import Type exposing (Typing(..))


type Msg
    = Select Pokemon
    | Deselect
    | ChangeMode Mode


type alias Model =
    { pokemon : Pokemon
    , mode : Mode
    , visible : Bool -- this allows us to always draw the view for nicer animations
    }


init : LocalStorage -> Pokemon -> Model
init localStorage first =
    { pokemon = first
    , mode = Maybe.withDefault Mode.Evolutions (Maybe.map Mode.fromString localStorage.mode)
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


view : Dict Int Pokemon -> Model -> Html Msg
view allPkm model =
    let
        pkm =
            model.pokemon

        viewBadgeWithEff ( t, e ) =
            Type.viewBadge t (Just e)

        evolvesFrom =
            Maybe.values <| List.map (\i -> Dict.get i allPkm) pkm.evolvesFromIDs

        -- Maybe.andThen (\i -> Dict.get i allPkm) pkm.evolvesFromID
        evolvesInto =
            Maybe.values <| List.map (\i -> Dict.get i allPkm) pkm.evolvesIntoIDs

        transformsInto =
            Maybe.values <| List.map (\i -> Dict.get i allPkm) pkm.othersInTransformGroup

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
                , figcaption [ css [ fontSize (Css.em 2) ] ] [ text pkm.fullName ]
                ]

        viewBadge p =
            div
                [ stopPropagationOnClick <| Select p
                , css
                    [ Type.backgroundFor p.typing
                    , SharedStyles.badgeStyle
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
                ]

        viewEvolition isPrevolution info p =
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
                ]

            Evolutions ->
                [ wrapEvolutionListView <| List.map (\p -> viewEvolition True (Maybe.withDefault "" pkm.evolvesFromDetails) p) evolvesFrom
                , mainView
                , wrapEvolutionListView <|
                    List.concat
                        [ List.map (\p -> viewEvolition False (Maybe.withDefault "" p.transformGroupDetails) p) transformsInto
                        , List.map (\p -> viewEvolition False (Maybe.withDefault "" p.evolvesFromDetails) p) evolvesInto
                        ]
                , typeEffectivenessButton
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
