module Type exposing (Type(..), Typing(..), backgroundFor, viewBadge)

import Html exposing (Attribute, Html, div, text)
import Html.Attributes exposing (class, style)


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
                [ class "type"
                ]
                [ text (toString type_) ]

        allHtml =
            case effectivenesss of
                Just e ->
                    [ typeHtml
                    , div
                        [ class "effectiveness" ]
                        [ text <| beautifyEffectiveness e ]
                    ]

                Nothing ->
                    [ typeHtml ]
    in
    div
        [ class "typeBadge"
        , style "background-color" (toColor type_)
        ]
        allHtml


backgroundFor : Typing -> Attribute msg
backgroundFor typing =
    case typing of
        Single type_ ->
            style "background-color" <| toColor type_

        Double first second ->
            style "background-image" <|
                "linear-gradient(to right, "
                    ++ toColor first
                    ++ ", "
                    ++ toColor first
                    ++ ", "
                    ++ toColor second
                    ++ ", "
                    ++ toColor second
                    ++ ")"


toString : Type -> String
toString type_ =
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


toColor : Type -> String
toColor type_ =
    case type_ of
        Normal ->
            "rgb(168, 167, 122)"

        Fire ->
            "rgb(238, 129, 48)"

        Water ->
            "rgb(99, 144, 240)"

        Electric ->
            "rgb(247, 208, 44)"

        Grass ->
            "rgb(122, 199, 76)"

        Ice ->
            "rgb(150, 217, 214)"

        Fighting ->
            "rgb(194, 46, 40)"

        Poison ->
            "rgb(163, 62, 161)"

        Ground ->
            "rgb(226, 191, 101)"

        Flying ->
            "rgb(169, 143, 243)"

        Psychic ->
            "rgb(249, 85, 135)"

        Bug ->
            "rgb(166, 185, 26)"

        Rock ->
            "rgb(182, 161, 54)"

        Ghost ->
            "rgb(115, 87, 151)"

        Dragon ->
            "rgb(111, 53, 252)"

        Dark ->
            "rgb(112, 87, 70)"

        Steel ->
            "rgb(183, 183, 206)"

        Fairy ->
            "rgb(214, 133, 173)"
