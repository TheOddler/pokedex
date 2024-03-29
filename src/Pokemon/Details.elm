module Pokemon.Details exposing (Model, Msg(..), init, update, view)

import Helpers exposing (stopPropagationOnClick)
import Html exposing (Html, button, div, figcaption, figure, img, text)
import Html.Attributes exposing (alt, class, src)
import List exposing (append)
import LocalStorage
import Maybe.Extra as Maybe
import Pokemon exposing (EvolutionData(..), Pokemon, TransformationData(..), shareTransformGroup, withID)
import Pokemon.Mode as Mode exposing (Mode(..))
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

        modeButton text_ toMode =
            button
                [ stopPropagationOnClick (ChangeMode toMode)
                , class "modeButton"
                ]
                [ text text_ ]

        typeEffectivenessButton =
            modeButton "show Type Effectiveness" TypeEffectiveness

        evolutionsButton =
            let
                label =
                    case ( pkm.evolutionData, pkm.transformationData ) of
                        ( IsNotEvolved, DoesNotTransform ) ->
                            "show Evolutions"

                        ( _, DoesNotTransform ) ->
                            "show Evolutions"

                        ( IsNotEvolved, _ ) ->
                            "show Transformations"

                        ( _, _ ) ->
                            "show Transformations & Evolutions"
            in
            modeButton label Evolutions

        -- The button is fake because you can close by clicking anywhere really.
        fakeCloseButton =
            div [ class "closeButton" ] []
    in
    div
        [ stopPropagationOnClick Deselect
        , class "details"
        , if model.visible then
            class "visible"

          else
            class "hidden"
        , Type.backgroundFor pkm.typing
        ]
    <|
        case model.mode of
            TypeEffectiveness ->
                append (typeEffectivenessView pkm)
                    [ evolutionsButton
                    , fakeCloseButton
                    ]

            Evolutions ->
                append (evolutionsView pkm allPkm)
                    [ typeEffectivenessButton
                    , fakeCloseButton
                    ]


mainView : Pokemon -> Html Msg
mainView pkm =
    figure
        []
        [ img
            [ src <| pkm.imageUrl
            , alt pkm.fullName
            , class "mainImage"
            ]
            []
        , figcaption
            [ class "mainName"
            ]
            [ text pkm.fullName
            ]
        ]


typeEffectivenessView : Pokemon -> List (Html Msg)
typeEffectivenessView pkm =
    let
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

        viewBadgeWithEff ( t, e ) =
            Type.viewBadge t (Just e)
    in
    [ mainView pkm
    , case pkm.typing of
        Single type_ ->
            Type.viewBadge type_ Nothing

        Double first second ->
            div
                [ class "typeList"
                ]
                [ Type.viewBadge first Nothing
                , Type.viewBadge second Nothing
                ]
    , div [ class "effectivenessChartTitle" ]
        [ text ("Super effective against " ++ pkm.fullName ++ ":") ]
    , div [ class "typeList" ] <|
        List.map viewBadgeWithEff superEffective
    , div [ class "effectivenessChartTitle" ]
        [ text ("Not very effective against " ++ pkm.fullName ++ ":") ]
    , div [ class "typeList" ] <|
        List.map viewBadgeWithEff notVeryEffective
    ]


evolutionsView : Pokemon -> List Pokemon -> List (Html Msg)
evolutionsView pkm allPkm =
    let
        evolvesFrom =
            case pkm.evolutionData of
                IsNotEvolved ->
                    []

                EvolvesFrom id _ ->
                    Maybe.toList <| withID allPkm id

                EvolvesFromMultiple ids _ ->
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

        evolutionDetailsToString p =
            case p.evolutionData of
                IsNotEvolved ->
                    ""

                EvolvesFrom _ details ->
                    details

                EvolvesFromMultiple _ details ->
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
                    [ Pokemon.viewBadge p [ stopPropagationOnClick <| Select p ]
                    , div [ class "info" ] [ text info ]
                    ]
            in
            if isPrevolution then
                children

            else
                List.reverse children

        wrapEvolutionListView =
            div [ class "evolutions" ]
    in
    [ wrapEvolutionListView <|
        List.concatMap (viewEvolution True <| evolutionDetailsToString pkm) evolvesFrom
    , mainView pkm
    , wrapEvolutionListView <|
        if evolvesFrom == [] && evolvesInto == [] && transformsInto == [] then
            [ text "Does not evolve" ]

        else
            List.concat
                [ List.concatMap (\p -> viewEvolution False (transformationDetailsToString p) p) transformsInto
                , List.concatMap (\p -> viewEvolution False (evolutionDetailsToString p) p) evolvesInto
                ]
    ]
