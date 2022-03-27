module Pokemon exposing (Pokemon, decoder, view, viewDetail)

import Csv.Decode as Decode exposing (Decoder)
import Html exposing (Html, div, figcaption, figure, img, text)
import Html.Attributes exposing (class, src)
import Maybe.Extra exposing (values)
import Type exposing (Type, backgroundFor, viewBadge)


type alias Pokemon =
    { id : Int
    , speciesID : Int
    , name : String
    , alternateFormName : Maybe String
    , originalPokemonID : Maybe Int
    , primaryType : Type
    , secondaryType : Maybe Type
    , evolvesFromID : Maybe Int
    , evolutionDetails : Maybe String
    , normal : Float
    , fire : Float
    , water : Float
    , electric : Float
    , grass : Float
    , ice : Float
    , fighting : Float
    , poison : Float
    , ground : Float
    , flying : Float
    , psychic : Float
    , bug : Float
    , rock : Float
    , ghost : Float
    , dragon : Float
    , dark : Float
    , steel : Float
    , fairy : Float
    }


view : Pokemon -> Html msg
view pkm =
    figure
        [ backgroundFor <| allTypes pkm
        , class "pokemon"
        ]
        [ img
            [ src <| imageUrl pkm
            ]
            []
        , figcaption
            [ class "name"
            ]
            [ text <| fullName pkm
            ]
        ]


viewDetail : Pokemon -> Html msg
viewDetail pkm =
    let
        viewBadgeWE ( t, e ) =
            viewBadge t (Just e)

        viewBadgeWoE t =
            viewBadge t Nothing

        types =
            allTypes pkm

        effectivenessList =
            effectivenessAgainst pkm
    in
    div
        [ class "details"
        , backgroundFor types
        ]
        [ figure
            [ class "pokemon"
            ]
            [ img
                [ src <| imageUrl pkm
                ]
                []
            , figcaption
                [ class "name"
                ]
                [ text <| fullName pkm
                ]
            ]
        , div [ class "typeChart" ] <|
            List.map viewBadgeWoE types
        , div [ class "effectivenessChartTitle" ] [ text ("Super effective against " ++ fullName pkm ++ ":") ]
        , div [ class "superEffectiveChart" ] <|
            List.map viewBadgeWE <|
                List.filter isSuperEffective effectivenessList
        , div [ class "effectivenessChartTitle" ] [ text ("Not very effective against " ++ fullName pkm ++ ":") ]
        , div [ class "notVeryEffectiveChart" ] <|
            List.map viewBadgeWE <|
                List.filter isNotVeryEffective effectivenessList
        ]



-- Helper functions


fullName : Pokemon -> String
fullName pkm =
    case pkm.alternateFormName of
        Just alternateFormName ->
            pkm.name ++ " (" ++ alternateFormName ++ ")"

        Nothing ->
            pkm.name


allTypes : Pokemon -> List Type
allTypes pkm =
    values [ Just pkm.primaryType, pkm.secondaryType ]


effectivenessAgainst : Pokemon -> List ( Type, Float )
effectivenessAgainst pkm =
    [ ( Type.Normal, pkm.normal )
    , ( Type.Fire, pkm.fire )
    , ( Type.Water, pkm.water )
    , ( Type.Electric, pkm.electric )
    , ( Type.Grass, pkm.grass )
    , ( Type.Ice, pkm.ice )
    , ( Type.Fighting, pkm.fighting )
    , ( Type.Poison, pkm.poison )
    , ( Type.Ground, pkm.ground )
    , ( Type.Flying, pkm.flying )
    , ( Type.Psychic, pkm.psychic )
    , ( Type.Bug, pkm.bug )
    , ( Type.Rock, pkm.rock )
    , ( Type.Ghost, pkm.ghost )
    , ( Type.Dragon, pkm.dragon )
    , ( Type.Dark, pkm.dark )
    , ( Type.Steel, pkm.steel )
    , ( Type.Fairy, pkm.fairy )
    ]


isSuperEffective : ( Type, Float ) -> Bool
isSuperEffective ( _, eff ) =
    eff >= 1.5


isNotVeryEffective : ( Type, Float ) -> Bool
isNotVeryEffective ( _, eff ) =
    eff < 0.75


decoder : Decoder Pokemon
decoder =
    Decode.into Pokemon
        |> Decode.pipeline (Decode.field "ID" Decode.int)
        |> Decode.pipeline (Decode.field "Species ID" Decode.int)
        |> Decode.pipeline (Decode.field "Name" Decode.string)
        |> Decode.pipeline (Decode.field "Alternate Form Name" (Decode.blank Decode.string))
        |> Decode.pipeline (Decode.field "Original Pokemon" (Decode.blank Decode.int))
        |> Decode.pipeline (Decode.field "Primary Type" Type.decoder)
        |> Decode.pipeline (Decode.field "Secondary Type" (Decode.blank Type.decoder))
        |> Decode.pipeline (Decode.field "Evolves From" (Decode.blank Decode.int))
        |> Decode.pipeline (Decode.field "Evolution Details" (Decode.blank Decode.string))
        |> Decode.pipeline (Decode.field "Normal" Decode.float)
        |> Decode.pipeline (Decode.field "Fire" Decode.float)
        |> Decode.pipeline (Decode.field "Water" Decode.float)
        |> Decode.pipeline (Decode.field "Electric" Decode.float)
        |> Decode.pipeline (Decode.field "Grass" Decode.float)
        |> Decode.pipeline (Decode.field "Ice" Decode.float)
        |> Decode.pipeline (Decode.field "Fighting" Decode.float)
        |> Decode.pipeline (Decode.field "Poison" Decode.float)
        |> Decode.pipeline (Decode.field "Ground" Decode.float)
        |> Decode.pipeline (Decode.field "Flying" Decode.float)
        |> Decode.pipeline (Decode.field "Psychic" Decode.float)
        |> Decode.pipeline (Decode.field "Bug" Decode.float)
        |> Decode.pipeline (Decode.field "Rock" Decode.float)
        |> Decode.pipeline (Decode.field "Ghost" Decode.float)
        |> Decode.pipeline (Decode.field "Dragon" Decode.float)
        |> Decode.pipeline (Decode.field "Dark" Decode.float)
        |> Decode.pipeline (Decode.field "Steel" Decode.float)
        |> Decode.pipeline (Decode.field "Fairy" Decode.float)


imageUrl : Pokemon -> String
imageUrl pkm =
    let
        base =
            "https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/"

        suffix =
            case pkm.alternateFormName of
                Just alternateFormName ->
                    "-" ++ String.toLower alternateFormName

                Nothing ->
                    ""
    in
    base ++ String.fromInt pkm.speciesID ++ suffix ++ ".png"
