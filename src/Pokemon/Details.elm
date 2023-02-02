module Pokemon.Details exposing (..)

import Helpers exposing (stopPropagationOnClick)
import Html exposing (Html, button, div, figcaption, figure, img, text)
import Html.Attributes exposing (alt, class, src)
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

        viewBadgeWithEff ( t, e ) =
            Type.viewBadge t (Just e)

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

        mainView =
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
                    , div [] [ text info ]
                    ]
            in
            div
                [ class "evolution"
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
                , div [ class "effectivenessChartTitle" ]
                    [ text ("Super effective against " ++ pkm.fullName ++ ":") ]
                , div [ class "effectivenessChart" ] <|
                    List.map viewBadgeWithEff superEffective
                , div [ class "effectivenessChartTitle" ]
                    [ text ("Not very effective against " ++ pkm.fullName ++ ":") ]
                , div [ class "effectivenessChart" ] <|
                    List.map viewBadgeWithEff notVeryEffective
                ]

        wrapEvolutionListView =
            div [ class "evolutions" ]

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
