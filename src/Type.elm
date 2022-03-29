module Type exposing (Type(..), Typing(..), backgroundFor, decoder, viewBadge)

import Css exposing (..)
import Csv.Decode as Decode exposing (Decoder)
import Html.Styled exposing (Html, div, text)
import Html.Styled.Attributes exposing (css)


type Typing
    = Single Type
    | Double Type Type


type Type
    = Normal
    | Fire
    | Water
    | Electric
    | Grass
    | Ice
    | Fighting
    | Poison
    | Ground
    | Flying
    | Psychic
    | Bug
    | Rock
    | Ghost
    | Dragon
    | Dark
    | Steel
    | Fairy


viewBadge : Type -> Maybe Float -> Html msg
viewBadge type_ effectivenesss =
    let
        beautifyEffectiveness f =
            if abs (f - 0.5) < 0.001 then
                "½"

            else if abs (f - 0.25) < 0.001 then
                "¼"

            else
                String.fromFloat f

        typeHtml =
            div
                [ css
                    [ textTransform uppercase
                    , display inlineBlock
                    ]
                ]
                [ text (typeName type_) ]

        allHtml =
            case effectivenesss of
                Just e ->
                    [ typeHtml
                    , div
                        [ css [ display inlineBlock, paddingLeft (em 0.5) ] ]
                        [ text <| beautifyEffectiveness e ]
                    ]

                Nothing ->
                    [ typeHtml ]
    in
    div
        [ css
            [ backgroundColor (typeColor type_)
            , badgeStyle
            ]
        ]
        allHtml


badgeStyle : Style
badgeStyle =
    Css.batch
        [ margin (em 0.2)
        , padding2 (em 0.2) (em 0.4)
        , borderRadius (em 0.4)
        , property "box-shadow" "inset 0 -2px 0 rgba(0, 0, 0, 0.2), inset 0 2px 0 rgba(255, 255, 255, 0.2);"
        , display inlineBlock
        ]


decoder : Decoder Type
decoder =
    Decode.andThen
        (\value ->
            Decode.fromMaybe (value ++ " is not a valid Type") (parseType value)
        )
        Decode.string


backgroundFor : Typing -> Style
backgroundFor typing =
    backgroundImage <|
        case typing of
            Single type_ ->
                linearGradient2 toRight (stop <| typeColor type_) (stop <| typeColor type_) []

            Double first second ->
                linearGradient2 toRight (stop <| typeColor first) (stop <| typeColor first) [ stop <| typeColor second, stop <| typeColor second ]


typeName : Type -> String
typeName type_ =
    case type_ of
        Normal ->
            "Normal"

        Fire ->
            "Fire"

        Water ->
            "Water"

        Electric ->
            "Electric"

        Grass ->
            "Grass"

        Ice ->
            "Ice"

        Fighting ->
            "Fighting"

        Poison ->
            "Poison"

        Ground ->
            "Ground"

        Flying ->
            "Flying"

        Psychic ->
            "Psychic"

        Bug ->
            "Bug"

        Rock ->
            "Rock"

        Ghost ->
            "Ghost"

        Dragon ->
            "Dragon"

        Dark ->
            "Dark"

        Steel ->
            "Steel"

        Fairy ->
            "Fairy"


parseType : String -> Maybe Type
parseType typeStr =
    case typeStr of
        "Normal" ->
            Just Normal

        "Fire" ->
            Just Fire

        "Water" ->
            Just Water

        "Electric" ->
            Just Electric

        "Grass" ->
            Just Grass

        "Ice" ->
            Just Ice

        "Fighting" ->
            Just Fighting

        "Poison" ->
            Just Poison

        "Ground" ->
            Just Ground

        "Flying" ->
            Just Flying

        "Psychic" ->
            Just Psychic

        "Bug" ->
            Just Bug

        "Rock" ->
            Just Rock

        "Ghost" ->
            Just Ghost

        "Dragon" ->
            Just Dragon

        "Dark" ->
            Just Dark

        "Steel" ->
            Just Steel

        "Fairy" ->
            Just Fairy

        _ ->
            Nothing


typeColor : Type -> Color
typeColor type_ =
    case type_ of
        Normal ->
            rgb 168 167 122

        Fire ->
            rgb 238 129 48

        Water ->
            rgb 99 144 240

        Electric ->
            rgb 247 208 44

        Grass ->
            rgb 122 199 76

        Ice ->
            rgb 150 217 214

        Fighting ->
            rgb 194 46 40

        Poison ->
            rgb 163 62 161

        Ground ->
            rgb 226 191 101

        Flying ->
            rgb 169 143 243

        Psychic ->
            rgb 249 85 135

        Bug ->
            rgb 166 185 26

        Rock ->
            rgb 182 161 54

        Ghost ->
            rgb 115 87 151

        Dragon ->
            rgb 111 53 252

        Dark ->
            rgb 112 87 70

        Steel ->
            rgb 183 183 206

        Fairy ->
            rgb 214 133 173
