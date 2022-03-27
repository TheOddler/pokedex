module Type exposing (Type(..), backgroundFor, decoder, viewBadge)

import Csv.Decode as Decode exposing (Decoder)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)


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
            div [ class "type" ] [ text (typeName type_) ]

        allHtml =
            case effectivenesss of
                Just e ->
                    [ typeHtml, div [ class "effectiveness" ] [ text <| beautifyEffectiveness e ] ]

                Nothing ->
                    [ typeHtml ]
    in
    div
        [ style "background-color" (typeColor type_)
        , class "typeBadge"
        ]
        allHtml


backgroundFor : List Type -> Html.Attribute msg
backgroundFor types =
    let
        duplicate c =
            [ c, c ]
    in
    style "background" <| "linear-gradient(to right, " ++ String.join "," (List.concatMap (typeColor >> duplicate) types) ++ ")"


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


decoder : Decoder Type
decoder =
    Decode.andThen
        (\value ->
            Decode.fromMaybe (value ++ " is not a valid Type") (parseType value)
        )
        Decode.string


typeColor : Type -> String
typeColor type_ =
    case type_ of
        Normal ->
            rgb255 168 167 122

        Fire ->
            rgb255 238 129 48

        Water ->
            rgb255 99 144 240

        Electric ->
            rgb255 247 208 44

        Grass ->
            rgb255 122 199 76

        Ice ->
            rgb255 150 217 214

        Fighting ->
            rgb255 194 46 40

        Poison ->
            rgb255 163 62 161

        Ground ->
            rgb255 226 191 101

        Flying ->
            rgb255 169 143 243

        Psychic ->
            rgb255 249 85 135

        Bug ->
            rgb255 166 185 26

        Rock ->
            rgb255 182 161 54

        Ghost ->
            rgb255 115 87 151

        Dragon ->
            rgb255 111 53 252

        Dark ->
            rgb255 112 87 70

        Steel ->
            rgb255 183 183 206

        Fairy ->
            rgb255 214 133 173


rgb255 : Int -> Int -> Int -> String
rgb255 r g b =
    "rgb(" ++ String.fromInt r ++ "," ++ String.fromInt g ++ "," ++ String.fromInt b ++ ")"
