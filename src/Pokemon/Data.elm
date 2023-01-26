module Pokemon.Data exposing (all, first)

import Ability exposing (Ability(..))
import Pokemon exposing (..)
import String.Extra as String
import StringHelpers as String
import Type exposing (..)


type Form
    = Mega
    | MegaX
    | MegaY
    | Alolan
    | Galarian
    | Hisuian
    | Paldean
    | Unique String String


nameWithForm : String -> Form -> String
nameWithForm name form =
    case form of
        Mega ->
            "Mega " ++ name

        MegaX ->
            "Mega " ++ name ++ " X"

        MegaY ->
            "Mega " ++ name ++ " Y"

        Alolan ->
            "Alolan " ++ name

        Galarian ->
            "Galarian " ++ name

        Hisuian ->
            "Hisuian " ++ name

        Paldean ->
            "Paldean " ++ name

        Unique prefix suffix ->
            prefix ++ " " ++ name ++ " " ++ suffix


imageNameCleanup : String -> String
imageNameCleanup =
    String.removeAll [ ":", "%", "♀", "♂", "'" ]
        << String.replaceAll [ ( " ", "_" ) ]
        << String.removeAccents


imageUrl : Int -> String -> String
imageUrl natDexNum name =
    "images/"
        ++ String.right 4 ("000" ++ String.fromInt natDexNum)
        ++ imageNameCleanup name
        ++ ".webp"


imageUrlWithForm : Int -> String -> Form -> String
imageUrlWithForm natDexNum name form =
    "images/"
        ++ String.right 4 ("000" ++ String.fromInt natDexNum)
        ++ imageNameCleanup name
        ++ "-"
        ++ (case form of
                Mega ->
                    "Mega"

                MegaX ->
                    "Mega_X"

                MegaY ->
                    "Mega_Y"

                Alolan ->
                    "Alola"

                Galarian ->
                    "Galar"

                Hisuian ->
                    "Hisui"

                Paldean ->
                    "Paldea"

                Unique _ suffix ->
                    suffix
           )
        ++ ".webp"


first : Pokemon
first =
    { id = 1
    , nationalDexNumber = 1
    , originalPokemonID = Nothing
    , fullName = "Bulbasaur"
    , typing = Double Grass Poison
    , ability = Nothing
    , imageUrl = imageUrl 1 "Bulbasaur"
    , evolutionData = DoesNotEvolve
    , transformationData = DoesNotTransform
    }


all : List Pokemon
all =
    [ first
    , { id = 2
      , nationalDexNumber = 2
      , originalPokemonID = Nothing
      , fullName = "Ivysaur"
      , typing = Double Grass Poison
      , ability = Nothing
      , imageUrl = imageUrl 2 "Ivysaur"
      , evolutionData = EvolvesFrom [ 1 ] "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 3
      , nationalDexNumber = 3
      , originalPokemonID = Nothing
      , fullName = "Venusaur"
      , typing = Double Grass Poison
      , ability = Nothing
      , imageUrl = imageUrl 3 "Venusaur"
      , evolutionData = EvolvesFrom [ 2 ] "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 4
      , nationalDexNumber = 3
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Venusaur" Mega
      , typing = Double Grass Poison
      , ability = Just ThickFat
      , imageUrl = imageUrlWithForm 3 "Venusaur" Mega
      , evolutionData = EvolvesFrom [ 3 ] "Holding Venusaurite"
      , transformationData = DoesNotTransform
      }
    , { id = 5
      , nationalDexNumber = 4
      , originalPokemonID = Nothing
      , fullName = "Charmander"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrl 4 "Charmander"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 6
      , nationalDexNumber = 5
      , originalPokemonID = Nothing
      , fullName = "Charmeleon"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrl 5 "Charmeleon"
      , evolutionData = EvolvesFrom [ 5 ] "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 7
      , nationalDexNumber = 6
      , originalPokemonID = Nothing
      , fullName = "Charizard"
      , typing = Double Fire Flying
      , ability = Nothing
      , imageUrl = imageUrl 6 "Charizard"
      , evolutionData = EvolvesFrom [ 6 ] "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 8
      , nationalDexNumber = 6
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Charizard" MegaX
      , typing = Double Fire Dragon
      , ability = Nothing
      , imageUrl = imageUrlWithForm 6 "Charizard" MegaX
      , evolutionData = EvolvesFrom [ 7 ] "Holding Charizardite X"
      , transformationData = DoesNotTransform
      }
    , { id = 9
      , nationalDexNumber = 6
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Charizard" MegaY
      , typing = Double Fire Flying
      , ability = Nothing
      , imageUrl = imageUrlWithForm 6 "Charizard" MegaY
      , evolutionData = EvolvesFrom [ 7 ] "Holding Charizardite Y"
      , transformationData = DoesNotTransform
      }
    , { id = 10
      , nationalDexNumber = 7
      , originalPokemonID = Nothing
      , fullName = "Squirtle"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 7 "Squirtle"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 11
      , nationalDexNumber = 8
      , originalPokemonID = Nothing
      , fullName = "Wartortle"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 8 "Wartortle"
      , evolutionData = EvolvesFrom [ 10 ] "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 12
      , nationalDexNumber = 9
      , originalPokemonID = Nothing
      , fullName = "Blastoise"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 9 "Blastoise"
      , evolutionData = EvolvesFrom [ 11 ] "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 13
      , nationalDexNumber = 9
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Blastoise" Mega
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlWithForm 9 "Blastoise" Mega
      , evolutionData = EvolvesFrom [ 12 ] "Holding Blastoisinite"
      , transformationData = DoesNotTransform
      }
    , { id = 32
      , nationalDexNumber = 10
      , originalPokemonID = Nothing
      , fullName = "Caterpie"
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imageUrl 10 "Caterpie"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 23
      , nationalDexNumber = 11
      , originalPokemonID = Nothing
      , fullName = "Metapod"
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imageUrl 11 "Metapod"
      , evolutionData = EvolvesFrom [ 32 ] "Level 7"
      , transformationData = DoesNotTransform
      }
    , { id = 24
      , nationalDexNumber = 12
      , originalPokemonID = Nothing
      , fullName = "Butterfree"
      , typing = Double Bug Flying
      , ability = Nothing
      , imageUrl = imageUrl 12 "Butterfree"
      , evolutionData = EvolvesFrom [ 23 ] "Level 10"
      , transformationData = DoesNotTransform
      }
    , { id = 31
      , nationalDexNumber = 13
      , originalPokemonID = Nothing
      , fullName = "Weedle"
      , typing = Double Bug Poison
      , ability = Nothing
      , imageUrl = imageUrl 13 "Weedle"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 30
      , nationalDexNumber = 14
      , originalPokemonID = Nothing
      , fullName = "Kakuna"
      , typing = Double Bug Poison
      , ability = Nothing
      , imageUrl = imageUrl 14 "Kakuna"
      , evolutionData = EvolvesFrom [ 31 ] "Level 7"
      , transformationData = DoesNotTransform
      }
    , { id = 27
      , nationalDexNumber = 15
      , originalPokemonID = Nothing
      , fullName = "Beedrill"
      , typing = Double Bug Poison
      , ability = Nothing
      , imageUrl = imageUrl 15 "Beedrill"
      , evolutionData = EvolvesFrom [ 30 ] "Level 10"
      , transformationData = DoesNotTransform
      }
    , { id = 28
      , nationalDexNumber = 15
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Beedrill" Mega
      , typing = Double Bug Poison
      , ability = Nothing
      , imageUrl = imageUrlWithForm 15 "Beedrill" Mega
      , evolutionData = EvolvesFrom [ 27 ] "Holding Beedrillite"
      , transformationData = DoesNotTransform
      }
    , { id = 76
      , nationalDexNumber = 16
      , originalPokemonID = Nothing
      , fullName = "Pidgey"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrl 16 "Pidgey"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 77
      , nationalDexNumber = 17
      , originalPokemonID = Nothing
      , fullName = "Pidgeotto"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrl 17 "Pidgeotto"
      , evolutionData = EvolvesFrom [ 76 ] "Level 18"
      , transformationData = DoesNotTransform
      }
    , { id = 78
      , nationalDexNumber = 18
      , originalPokemonID = Nothing
      , fullName = "Pidgeot"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrl 18 "Pidgeot"
      , evolutionData = EvolvesFrom [ 77 ] "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 79
      , nationalDexNumber = 18
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Pidgeot" Mega
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrlWithForm 18 "Pidgeot" Mega
      , evolutionData = EvolvesFrom [ 78 ] "Holding Pidgeotite"
      , transformationData = DoesNotTransform
      }
    , { id = 80
      , nationalDexNumber = 19
      , originalPokemonID = Nothing
      , fullName = "Rattata"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 19 "Rattata"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 82
      , nationalDexNumber = 20
      , originalPokemonID = Nothing
      , fullName = "Raticate"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 20 "Raticate"
      , evolutionData = EvolvesFrom [ 80 ] "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 81
      , nationalDexNumber = 19
      , originalPokemonID = Just 80
      , fullName = nameWithForm "Rattata" Alolan
      , typing = Double Dark Normal
      , ability = Nothing
      , imageUrl = imageUrlWithForm 19 "Rattata" Alolan
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 83
      , nationalDexNumber = 20
      , originalPokemonID = Just 82
      , fullName = nameWithForm "Raticate" Alolan
      , typing = Double Dark Normal
      , ability = Nothing
      , imageUrl = imageUrlWithForm 20 "Raticate" Alolan
      , evolutionData = EvolvesFrom [ 81 ] "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 84
      , nationalDexNumber = 21
      , originalPokemonID = Nothing
      , fullName = "Spearow"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrl 21 "Spearow"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 85
      , nationalDexNumber = 22
      , originalPokemonID = Nothing
      , fullName = "Fearow"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrl 22 "Fearow"
      , evolutionData = EvolvesFrom [ 84 ] "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 86
      , nationalDexNumber = 23
      , originalPokemonID = Nothing
      , fullName = "Ekans"
      , typing = Single Poison
      , ability = Nothing
      , imageUrl = imageUrl 23 "Ekans"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 87
      , nationalDexNumber = 24
      , originalPokemonID = Nothing
      , fullName = "Arbok"
      , typing = Single Poison
      , ability = Nothing
      , imageUrl = imageUrl 24 "Arbok"
      , evolutionData = EvolvesFrom [ 86 ] "Level 22"
      , transformationData = DoesNotTransform
      }
    , { id = 88
      , nationalDexNumber = 172
      , originalPokemonID = Nothing
      , fullName = "Pichu"
      , typing = Single Electric
      , ability = Nothing
      , imageUrl = imageUrl 172 "Pichu"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 89
      , nationalDexNumber = 25
      , originalPokemonID = Nothing
      , fullName = "Pikachu"
      , typing = Single Electric
      , ability = Nothing
      , imageUrl = imageUrl 25 "Pikachu"
      , evolutionData = EvolvesFrom [ 88 ] "Level with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 90
      , nationalDexNumber = 26
      , originalPokemonID = Nothing
      , fullName = "Raichu"
      , typing = Single Electric
      , ability = Nothing
      , imageUrl = imageUrl 26 "Raichu"
      , evolutionData = EvolvesFrom [ 89 ] "Use Thunder Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 91
      , nationalDexNumber = 26
      , originalPokemonID = Just 90
      , fullName = nameWithForm "Raichu" Alolan
      , typing = Double Electric Psychic
      , ability = Nothing
      , imageUrl = imageUrlWithForm 26 "Raichu" Alolan
      , evolutionData = EvolvesFrom [ 89 ] "Use Thunder Stone in Alola"
      , transformationData = DoesNotTransform
      }
    , { id = 92
      , nationalDexNumber = 27
      , originalPokemonID = Nothing
      , fullName = "Sandshrew"
      , typing = Single Ground
      , ability = Nothing
      , imageUrl = imageUrl 27 "Sandshrew"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 94
      , nationalDexNumber = 28
      , originalPokemonID = Nothing
      , fullName = "Sandslash"
      , typing = Single Ground
      , ability = Nothing
      , imageUrl = imageUrl 28 "Sandslash"
      , evolutionData = EvolvesFrom [ 92 ] "Level 22"
      , transformationData = DoesNotTransform
      }
    , { id = 93
      , nationalDexNumber = 27
      , originalPokemonID = Just 92
      , fullName = nameWithForm "Sandshrew" Alolan
      , typing = Double Ice Steel
      , ability = Nothing
      , imageUrl = imageUrlWithForm 27 "Sandshrew" Alolan
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 95
      , nationalDexNumber = 28
      , originalPokemonID = Just 94
      , fullName = nameWithForm "Sandslash" Alolan
      , typing = Double Ice Steel
      , ability = Nothing
      , imageUrl = imageUrlWithForm 28 "Sandslash" Alolan
      , evolutionData = EvolvesFrom [ 93 ] "Use Ice Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 33
      , nationalDexNumber = 29
      , originalPokemonID = Nothing
      , fullName = "Nidoran ♀"
      , typing = Single Poison
      , ability = Nothing
      , imageUrl = imageUrl 29 "Nidoran"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 17
      , nationalDexNumber = 30
      , originalPokemonID = Nothing
      , fullName = "Nidorina"
      , typing = Single Poison
      , ability = Nothing
      , imageUrl = imageUrl 30 "Nidorina"
      , evolutionData = EvolvesFrom [ 33 ] "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 18
      , nationalDexNumber = 31
      , originalPokemonID = Nothing
      , fullName = "Nidoqueen"
      , typing = Double Poison Ground
      , ability = Nothing
      , imageUrl = imageUrl 31 "Nidoqueen"
      , evolutionData = EvolvesFrom [ 17 ] "Use Moon Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 48
      , nationalDexNumber = 32
      , originalPokemonID = Nothing
      , fullName = "Nidoran ♂"
      , typing = Single Poison
      , ability = Nothing
      , imageUrl = imageUrl 32 "Nidoran"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 49
      , nationalDexNumber = 33
      , originalPokemonID = Nothing
      , fullName = "Nidorino"
      , typing = Single Poison
      , ability = Nothing
      , imageUrl = imageUrl 33 "Nidorino"
      , evolutionData = EvolvesFrom [ 48 ] "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 50
      , nationalDexNumber = 34
      , originalPokemonID = Nothing
      , fullName = "Nidoking"
      , typing = Double Poison Ground
      , ability = Nothing
      , imageUrl = imageUrl 34 "Nidoking"
      , evolutionData = EvolvesFrom [ 49 ] "Use Moon Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 51
      , nationalDexNumber = 173
      , originalPokemonID = Nothing
      , fullName = "Cleffa"
      , typing = Single Fairy
      , ability = Nothing
      , imageUrl = imageUrl 173 "Cleffa"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 52
      , nationalDexNumber = 35
      , originalPokemonID = Nothing
      , fullName = "Clefairy"
      , typing = Single Fairy
      , ability = Nothing
      , imageUrl = imageUrl 35 "Clefairy"
      , evolutionData = EvolvesFrom [ 51 ] "Level with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 53
      , nationalDexNumber = 36
      , originalPokemonID = Nothing
      , fullName = "Clefable"
      , typing = Single Fairy
      , ability = Nothing
      , imageUrl = imageUrl 36 "Clefable"
      , evolutionData = EvolvesFrom [ 52 ] "Use Moon Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 54
      , nationalDexNumber = 37
      , originalPokemonID = Nothing
      , fullName = "Vulpix"
      , typing = Single Fire
      , ability = Just FlashFire
      , imageUrl = imageUrl 37 "Vulpix"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 56
      , nationalDexNumber = 38
      , originalPokemonID = Nothing
      , fullName = "Ninetales"
      , typing = Single Fire
      , ability = Just FlashFire
      , imageUrl = imageUrl 38 "Ninetales"
      , evolutionData = EvolvesFrom [ 54 ] "Use Fire Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 55
      , nationalDexNumber = 37
      , originalPokemonID = Just 54
      , fullName = nameWithForm "Vulpix" Alolan
      , typing = Single Ice
      , ability = Nothing
      , imageUrl = imageUrlWithForm 37 "Vulpix" Alolan
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 57
      , nationalDexNumber = 38
      , originalPokemonID = Just 56
      , fullName = nameWithForm "Ninetales" Alolan
      , typing = Double Ice Fairy
      , ability = Nothing
      , imageUrl = imageUrlWithForm 38 "Ninetales" Alolan
      , evolutionData = EvolvesFrom [ 55 ] "Use Ice Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 58
      , nationalDexNumber = 174
      , originalPokemonID = Nothing
      , fullName = "Igglybuff"
      , typing = Double Normal Fairy
      , ability = Nothing
      , imageUrl = imageUrl 174 "Igglybuff"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 59
      , nationalDexNumber = 39
      , originalPokemonID = Nothing
      , fullName = "Jigglypuff"
      , typing = Double Normal Fairy
      , ability = Nothing
      , imageUrl = imageUrl 39 "Jigglypuff"
      , evolutionData = EvolvesFrom [ 58 ] "Level with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 60
      , nationalDexNumber = 40
      , originalPokemonID = Nothing
      , fullName = "Wigglytuff"
      , typing = Double Normal Fairy
      , ability = Nothing
      , imageUrl = imageUrl 40 "Wigglytuff"
      , evolutionData = EvolvesFrom [ 59 ] "Use Moon Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 61
      , nationalDexNumber = 41
      , originalPokemonID = Nothing
      , fullName = "Zubat"
      , typing = Double Poison Flying
      , ability = Nothing
      , imageUrl = imageUrl 41 "Zubat"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 62
      , nationalDexNumber = 42
      , originalPokemonID = Nothing
      , fullName = "Golbat"
      , typing = Double Poison Flying
      , ability = Nothing
      , imageUrl = imageUrl 42 "Golbat"
      , evolutionData = EvolvesFrom [ 61 ] "Level 22"
      , transformationData = DoesNotTransform
      }
    , { id = 63
      , nationalDexNumber = 169
      , originalPokemonID = Nothing
      , fullName = "Crobat"
      , typing = Double Poison Flying
      , ability = Nothing
      , imageUrl = imageUrl 169 "Crobat"
      , evolutionData = EvolvesFrom [ 62 ] "Level with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 64
      , nationalDexNumber = 43
      , originalPokemonID = Nothing
      , fullName = "Oddish"
      , typing = Double Grass Poison
      , ability = Nothing
      , imageUrl = imageUrl 43 "Oddish"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 65
      , nationalDexNumber = 44
      , originalPokemonID = Nothing
      , fullName = "Gloom"
      , typing = Double Grass Poison
      , ability = Nothing
      , imageUrl = imageUrl 44 "Gloom"
      , evolutionData = EvolvesFrom [ 64 ] "Level 21"
      , transformationData = DoesNotTransform
      }
    , { id = 66
      , nationalDexNumber = 45
      , originalPokemonID = Nothing
      , fullName = "Vileplume"
      , typing = Double Grass Poison
      , ability = Nothing
      , imageUrl = imageUrl 45 "Vileplume"
      , evolutionData = EvolvesFrom [ 65 ] "Use Leaf Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 67
      , nationalDexNumber = 182
      , originalPokemonID = Nothing
      , fullName = "Bellossom"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrl 182 "Bellossom"
      , evolutionData = EvolvesFrom [ 65 ] "Use Sun Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 68
      , nationalDexNumber = 46
      , originalPokemonID = Nothing
      , fullName = "Paras"
      , typing = Double Bug Grass
      , ability = Just DrySkin
      , imageUrl = imageUrl 46 "Paras"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 69
      , nationalDexNumber = 47
      , originalPokemonID = Nothing
      , fullName = "Parasect"
      , typing = Double Bug Grass
      , ability = Just DrySkin
      , imageUrl = imageUrl 47 "Parasect"
      , evolutionData = EvolvesFrom [ 68 ] "Level 24"
      , transformationData = DoesNotTransform
      }
    , { id = 70
      , nationalDexNumber = 48
      , originalPokemonID = Nothing
      , fullName = "Venonat"
      , typing = Double Bug Poison
      , ability = Nothing
      , imageUrl = imageUrl 48 "Venonat"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 71
      , nationalDexNumber = 49
      , originalPokemonID = Nothing
      , fullName = "Venomoth"
      , typing = Double Bug Poison
      , ability = Nothing
      , imageUrl = imageUrl 49 "Venomoth"
      , evolutionData = EvolvesFrom [ 70 ] "Level 31"
      , transformationData = DoesNotTransform
      }
    , { id = 72
      , nationalDexNumber = 50
      , originalPokemonID = Nothing
      , fullName = "Diglett"
      , typing = Single Ground
      , ability = Nothing
      , imageUrl = imageUrl 50 "Diglett"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 73
      , nationalDexNumber = 51
      , originalPokemonID = Nothing
      , fullName = "Dugtrio"
      , typing = Single Ground
      , ability = Nothing
      , imageUrl = imageUrl 51 "Dugtrio"
      , evolutionData = EvolvesFrom [ 72 ] "Level 26"
      , transformationData = DoesNotTransform
      }
    , { id = 74
      , nationalDexNumber = 50
      , originalPokemonID = Just 72
      , fullName = nameWithForm "Diglett" Alolan
      , typing = Double Ground Steel
      , ability = Nothing
      , imageUrl = imageUrlWithForm 50 "Diglett" Alolan
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 75
      , nationalDexNumber = 51
      , originalPokemonID = Just 73
      , fullName = nameWithForm "Dugtrio" Alolan
      , typing = Double Ground Steel
      , ability = Nothing
      , imageUrl = imageUrlWithForm 51 "Dugtrio" Alolan
      , evolutionData = EvolvesFrom [ 74 ] "Level 26 in Alola"
      , transformationData = DoesNotTransform
      }
    , { id = 96
      , nationalDexNumber = 52
      , originalPokemonID = Nothing
      , fullName = "Meowth"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 52 "Meowth"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 100
      , nationalDexNumber = 53
      , originalPokemonID = Nothing
      , fullName = "Persian"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 53 "Persian"
      , evolutionData = EvolvesFrom [ 96 ] "Level 28"
      , transformationData = DoesNotTransform
      }
    , { id = 97
      , nationalDexNumber = 52
      , originalPokemonID = Just 96
      , fullName = nameWithForm "Meowth" Alolan
      , typing = Single Dark
      , ability = Nothing
      , imageUrl = imageUrlWithForm 52 "Meowth" Alolan
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 101
      , nationalDexNumber = 53
      , originalPokemonID = Just 100
      , fullName = nameWithForm "Persian" Alolan
      , typing = Single Dark
      , ability = Nothing
      , imageUrl = imageUrlWithForm 53 "Persian" Alolan
      , evolutionData = EvolvesFrom [ 97 ] "Level with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 98
      , nationalDexNumber = 52
      , originalPokemonID = Just 96
      , fullName = nameWithForm "Meowth" Galarian
      , typing = Single Steel
      , ability = Nothing
      , imageUrl = imageUrlWithForm 52 "Meowth" Galarian
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 99
      , nationalDexNumber = 863
      , originalPokemonID = Nothing
      , fullName = "Perrserker"
      , typing = Single Steel
      , ability = Nothing
      , imageUrl = imageUrl 863 "Perrserker"
      , evolutionData = EvolvesFrom [ 98 ] "Level 28"
      , transformationData = DoesNotTransform
      }
    , { id = 102
      , nationalDexNumber = 54
      , originalPokemonID = Nothing
      , fullName = "Psyduck"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 54 "Psyduck"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 884
      , nationalDexNumber = 55
      , originalPokemonID = Nothing
      , fullName = "Golduck"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 55 "Golduck"
      , evolutionData = EvolvesFrom [ 102 ] "Level 33"
      , transformationData = DoesNotTransform
      }
    , { id = 895
      , nationalDexNumber = 56
      , originalPokemonID = Nothing
      , fullName = "Mankey"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrl 56 "Mankey"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 924
      , nationalDexNumber = 57
      , originalPokemonID = Nothing
      , fullName = "Primeape"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrl 57 "Primeape"
      , evolutionData = EvolvesFrom [ 895 ] "Level 28"
      , transformationData = DoesNotTransform
      }
    , { id = 1664
      , nationalDexNumber = 979
      , originalPokemonID = Nothing
      , fullName = "Annihilape"
      , typing = Double Fighting Ghost
      , ability = Nothing
      , imageUrl = imageUrl 979 "Annihilape"
      , evolutionData = EvolvesFrom [ 924 ] "After using Rage Fist 20 times"
      , transformationData = DoesNotTransform
      }
    , { id = 949
      , nationalDexNumber = 58
      , originalPokemonID = Nothing
      , fullName = "Growlithe"
      , typing = Single Fire
      , ability = Just FlashFire
      , imageUrl = imageUrl 58 "Growlithe"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 960
      , nationalDexNumber = 59
      , originalPokemonID = Nothing
      , fullName = "Arcanine"
      , typing = Single Fire
      , ability = Just FlashFire
      , imageUrl = imageUrl 59 "Arcanine"
      , evolutionData = EvolvesFrom [ 949 ] "Use Fire Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 1619
      , nationalDexNumber = 58
      , originalPokemonID = Just 949
      , fullName = nameWithForm "Growlithe" Hisuian
      , typing = Double Fire Rock
      , ability = Just FlashFire
      , imageUrl = imageUrlWithForm 58 "Growlithe" Hisuian
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1620
      , nationalDexNumber = 59
      , originalPokemonID = Just 960
      , fullName = nameWithForm "Arcanine" Hisuian
      , typing = Double Fire Rock
      , ability = Just FlashFire
      , imageUrl = imageUrlWithForm 59 "Arcanine" Hisuian
      , evolutionData = EvolvesFrom [ 1619 ] "Use Fire Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 971
      , nationalDexNumber = 60
      , originalPokemonID = Nothing
      , fullName = "Poliwag"
      , typing = Single Water
      , ability = Just WaterAbsorb
      , imageUrl = imageUrl 60 "Poliwag"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 982
      , nationalDexNumber = 61
      , originalPokemonID = Nothing
      , fullName = "Poliwhirl"
      , typing = Single Water
      , ability = Just WaterAbsorb
      , imageUrl = imageUrl 61 "Poliwhirl"
      , evolutionData = EvolvesFrom [ 971 ] "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 993
      , nationalDexNumber = 62
      , originalPokemonID = Nothing
      , fullName = "Poliwrath"
      , typing = Double Water Fighting
      , ability = Just WaterAbsorb
      , imageUrl = imageUrl 62 "Poliwrath"
      , evolutionData = EvolvesFrom [ 982 ] "Use Water Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 286
      , nationalDexNumber = 186
      , originalPokemonID = Nothing
      , fullName = "Politoed"
      , typing = Single Water
      , ability = Just WaterAbsorb
      , imageUrl = imageUrl 186 "Politoed"
      , evolutionData = EvolvesFrom [ 982 ] "Trade holding King's Rock"
      , transformationData = DoesNotTransform
      }
    , { id = 1004
      , nationalDexNumber = 63
      , originalPokemonID = Nothing
      , fullName = "Abra"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrl 63 "Abra"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1015
      , nationalDexNumber = 64
      , originalPokemonID = Nothing
      , fullName = "Kadabra"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrl 64 "Kadabra"
      , evolutionData = EvolvesFrom [ 1004 ] "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 1026
      , nationalDexNumber = 65
      , originalPokemonID = Nothing
      , fullName = "Alakazam"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrl 65 "Alakazam"
      , evolutionData = EvolvesFrom [ 1015 ] "Trade"
      , transformationData = DoesNotTransform
      }
    , { id = 1360
      , nationalDexNumber = 65
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Alakazam" Mega
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrlWithForm 65 "Alakazam" Mega
      , evolutionData = EvolvesFrom [ 1026 ] "Holding Alakazite"
      , transformationData = DoesNotTransform
      }
    , { id = 1076
      , nationalDexNumber = 66
      , originalPokemonID = Nothing
      , fullName = "Machop"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrl 66 "Machop"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1087
      , nationalDexNumber = 67
      , originalPokemonID = Nothing
      , fullName = "Machoke"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrl 67 "Machoke"
      , evolutionData = EvolvesFrom [ 1076 ] "Level 28"
      , transformationData = DoesNotTransform
      }
    , { id = 1097
      , nationalDexNumber = 68
      , originalPokemonID = Nothing
      , fullName = "Machamp"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrl 68 "Machamp"
      , evolutionData = EvolvesFrom [ 1087 ] "Trade"
      , transformationData = DoesNotTransform
      }
    , { id = 1109
      , nationalDexNumber = 69
      , originalPokemonID = Nothing
      , fullName = "Bellsprout"
      , typing = Double Grass Poison
      , ability = Nothing
      , imageUrl = imageUrl 69 "Bellsprout"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1128
      , nationalDexNumber = 70
      , originalPokemonID = Nothing
      , fullName = "Weepinbell"
      , typing = Double Grass Poison
      , ability = Nothing
      , imageUrl = imageUrl 70 "Weepinbell"
      , evolutionData = EvolvesFrom [ 1109 ] "Level 21"
      , transformationData = DoesNotTransform
      }
    , { id = 1139
      , nationalDexNumber = 71
      , originalPokemonID = Nothing
      , fullName = "Victreebel"
      , typing = Double Grass Poison
      , ability = Nothing
      , imageUrl = imageUrl 71 "Victreebel"
      , evolutionData = EvolvesFrom [ 1128 ] "Use Leaf Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 1150
      , nationalDexNumber = 72
      , originalPokemonID = Nothing
      , fullName = "Tentacool"
      , typing = Double Water Poison
      , ability = Nothing
      , imageUrl = imageUrl 72 "Tentacool"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1254
      , nationalDexNumber = 73
      , originalPokemonID = Nothing
      , fullName = "Tentacruel"
      , typing = Double Water Poison
      , ability = Nothing
      , imageUrl = imageUrl 73 "Tentacruel"
      , evolutionData = EvolvesFrom [ 1150 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 1265
      , nationalDexNumber = 74
      , originalPokemonID = Nothing
      , fullName = "Geodude"
      , typing = Double Rock Ground
      , ability = Nothing
      , imageUrl = imageUrl 74 "Geodude"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1276
      , nationalDexNumber = 75
      , originalPokemonID = Nothing
      , fullName = "Graveler"
      , typing = Double Rock Ground
      , ability = Nothing
      , imageUrl = imageUrl 75 "Graveler"
      , evolutionData = EvolvesFrom [ 1265 ] "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 1287
      , nationalDexNumber = 76
      , originalPokemonID = Nothing
      , fullName = "Golem"
      , typing = Double Rock Ground
      , ability = Nothing
      , imageUrl = imageUrl 76 "Golem"
      , evolutionData = EvolvesFrom [ 1276 ] "Trade"
      , transformationData = DoesNotTransform
      }
    , { id = 1361
      , nationalDexNumber = 74
      , originalPokemonID = Just 1265
      , fullName = nameWithForm "Geodude" Alolan
      , typing = Double Rock Electric
      , ability = Nothing
      , imageUrl = imageUrlWithForm 74 "Geodude" Alolan
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1362
      , nationalDexNumber = 75
      , originalPokemonID = Just 1276
      , fullName = nameWithForm "Graveler" Alolan
      , typing = Double Rock Electric
      , ability = Nothing
      , imageUrl = imageUrlWithForm 75 "Graveler" Alolan
      , evolutionData = EvolvesFrom [ 1361 ] "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 1363
      , nationalDexNumber = 76
      , originalPokemonID = Just 1287
      , fullName = nameWithForm "Golem" Alolan
      , typing = Double Rock Electric
      , ability = Nothing
      , imageUrl = imageUrlWithForm 76 "Golem" Alolan
      , evolutionData = EvolvesFrom [ 1362 ] "Trade"
      , transformationData = DoesNotTransform
      }
    , { id = 1297
      , nationalDexNumber = 77
      , originalPokemonID = Nothing
      , fullName = "Ponyta"
      , typing = Single Fire
      , ability = Just FlashFire
      , imageUrl = imageUrl 77 "Ponyta"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1308
      , nationalDexNumber = 78
      , originalPokemonID = Nothing
      , fullName = "Rapidash"
      , typing = Single Fire
      , ability = Just FlashFire
      , imageUrl = imageUrl 78 "Rapidash"
      , evolutionData = EvolvesFrom [ 1297 ] "Level 40"
      , transformationData = DoesNotTransform
      }
    , { id = 1364
      , nationalDexNumber = 77
      , originalPokemonID = Just 1297
      , fullName = nameWithForm "Ponyta" Galarian
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrlWithForm 77 "Ponyta" Galarian
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1365
      , nationalDexNumber = 78
      , originalPokemonID = Just 1308
      , fullName = nameWithForm "Rapidash" Galarian
      , typing = Double Psychic Fairy
      , ability = Nothing
      , imageUrl = imageUrlWithForm 78 "Rapidash" Galarian
      , evolutionData = EvolvesFrom [ 1364 ] "Level 40"
      , transformationData = DoesNotTransform
      }
    , { id = 1319
      , nationalDexNumber = 79
      , originalPokemonID = Nothing
      , fullName = "Slowpoke"
      , typing = Double Water Psychic
      , ability = Nothing
      , imageUrl = imageUrl 79 "Slowpoke"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1330
      , nationalDexNumber = 80
      , originalPokemonID = Nothing
      , fullName = "Slowbro"
      , typing = Double Water Psychic
      , ability = Nothing
      , imageUrl = imageUrl 80 "Slowbro"
      , evolutionData = EvolvesFrom [ 1319 ] "Level 37"
      , transformationData = DoesNotTransform
      }
    , { id = 1367
      , nationalDexNumber = 80
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Slowbro" Mega
      , typing = Double Water Psychic
      , ability = Nothing
      , imageUrl = imageUrlWithForm 80 "Slowbro" Mega
      , evolutionData = EvolvesFrom [ 1330 ] "Holding Slowbronite"
      , transformationData = DoesNotTransform
      }
    , { id = 299
      , nationalDexNumber = 199
      , originalPokemonID = Nothing
      , fullName = "Slowking"
      , typing = Double Water Psychic
      , ability = Nothing
      , imageUrl = imageUrl 199 "Slowking"
      , evolutionData = EvolvesFrom [ 1319 ] "Trade holding King's Rock"
      , transformationData = DoesNotTransform
      }
    , { id = 1366
      , nationalDexNumber = 79
      , originalPokemonID = Just 1319
      , fullName = nameWithForm "Slowpoke" Galarian
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrlWithForm 79 "Slowpoke" Galarian
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1368
      , nationalDexNumber = 80
      , originalPokemonID = Just 1330
      , fullName = nameWithForm "Slowbro" Galarian
      , typing = Double Poison Psychic
      , ability = Nothing
      , imageUrl = imageUrlWithForm 80 "Slowbro" Galarian
      , evolutionData = EvolvesFrom [ 1366 ] "Use Galarica Cuff"
      , transformationData = DoesNotTransform
      }
    , { id = 1388
      , nationalDexNumber = 199
      , originalPokemonID = Just 299
      , fullName = nameWithForm "Slowking" Galarian
      , typing = Double Poison Psychic
      , ability = Nothing
      , imageUrl = imageUrlWithForm 199 "Slowking" Galarian
      , evolutionData = EvolvesFrom [ 1366 ] "Use Galarica Wreath"
      , transformationData = DoesNotTransform
      }
    , { id = 46
      , nationalDexNumber = 81
      , originalPokemonID = Nothing
      , fullName = "Magnemite"
      , typing = Double Electric Steel
      , ability = Nothing
      , imageUrl = imageUrl 81 "Magnemite"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1341
      , nationalDexNumber = 82
      , originalPokemonID = Nothing
      , fullName = "Magneton"
      , typing = Double Electric Steel
      , ability = Nothing
      , imageUrl = imageUrl 82 "Magneton"
      , evolutionData = EvolvesFrom [ 46 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 684
      , nationalDexNumber = 462
      , originalPokemonID = Nothing
      , fullName = "Magnezone"
      , typing = Double Electric Steel
      , ability = Nothing
      , imageUrl = imageUrl 462 "Magnezone"
      , evolutionData = EvolvesFrom [ 1341 ] "Use Thunder Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 1342
      , nationalDexNumber = 83
      , originalPokemonID = Nothing
      , fullName = "Farfetch'd"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrl 83 "Farfetch'd"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1369
      , nationalDexNumber = 83
      , originalPokemonID = Just 1342
      , fullName = nameWithForm "Farfetch'd" Galarian
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrlWithForm 83 "Farfetch'd" Galarian
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1398
      , nationalDexNumber = 865
      , originalPokemonID = Nothing
      , fullName = "Sirfetch'd"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrl 865 "Sirfetch'd"
      , evolutionData = EvolvesFrom [ 1369 ] "Land three Critical Hits in one battle"
      , transformationData = DoesNotTransform
      }
    , { id = 1343
      , nationalDexNumber = 84
      , originalPokemonID = Nothing
      , fullName = "Doduo"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrl 84 "Doduo"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1344
      , nationalDexNumber = 85
      , originalPokemonID = Nothing
      , fullName = "Dodrio"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrl 85 "Dodrio"
      , evolutionData = EvolvesFrom [ 1343 ] "Level 31"
      , transformationData = DoesNotTransform
      }
    , { id = 1345
      , nationalDexNumber = 86
      , originalPokemonID = Nothing
      , fullName = "Seel"
      , typing = Single Water
      , ability = Just ThickFat
      , imageUrl = imageUrl 86 "Seel"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1346
      , nationalDexNumber = 87
      , originalPokemonID = Nothing
      , fullName = "Dewgong"
      , typing = Double Water Ice
      , ability = Just ThickFat
      , imageUrl = imageUrl 87 "Dewgong"
      , evolutionData = EvolvesFrom [ 1345 ] "Level 34"
      , transformationData = DoesNotTransform
      }
    , { id = 1347
      , nationalDexNumber = 88
      , originalPokemonID = Nothing
      , fullName = "Grimer"
      , typing = Single Poison
      , ability = Nothing
      , imageUrl = imageUrl 88 "Grimer"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1348
      , nationalDexNumber = 89
      , originalPokemonID = Nothing
      , fullName = "Muk"
      , typing = Single Poison
      , ability = Nothing
      , imageUrl = imageUrl 89 "Muk"
      , evolutionData = EvolvesFrom [ 1347 ] "Level 38"
      , transformationData = DoesNotTransform
      }
    , { id = 1372
      , nationalDexNumber = 88
      , originalPokemonID = Just 1347
      , fullName = nameWithForm "Grimer" Alolan
      , typing = Double Poison Dark
      , ability = Nothing
      , imageUrl = imageUrlWithForm 88 "Grimer" Alolan
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1373
      , nationalDexNumber = 89
      , originalPokemonID = Just 1348
      , fullName = nameWithForm "Muk" Alolan
      , typing = Double Poison Dark
      , ability = Nothing
      , imageUrl = imageUrlWithForm 89 "Muk" Alolan
      , evolutionData = EvolvesFrom [ 1372 ] "Level 38"
      , transformationData = DoesNotTransform
      }
    , { id = 1349
      , nationalDexNumber = 90
      , originalPokemonID = Nothing
      , fullName = "Shellder"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 90 "Shellder"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1350
      , nationalDexNumber = 91
      , originalPokemonID = Nothing
      , fullName = "Cloyster"
      , typing = Double Water Ice
      , ability = Nothing
      , imageUrl = imageUrl 91 "Cloyster"
      , evolutionData = EvolvesFrom [ 1349 ] "Use Water Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 1351
      , nationalDexNumber = 92
      , originalPokemonID = Nothing
      , fullName = "Gastly"
      , typing = Double Ghost Poison
      , ability = Just Levitate
      , imageUrl = imageUrl 92 "Gastly"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1352
      , nationalDexNumber = 93
      , originalPokemonID = Nothing
      , fullName = "Haunter"
      , typing = Double Ghost Poison
      , ability = Just Levitate
      , imageUrl = imageUrl 93 "Haunter"
      , evolutionData = EvolvesFrom [ 1351 ] "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 1353
      , nationalDexNumber = 94
      , originalPokemonID = Nothing
      , fullName = "Gengar"
      , typing = Double Ghost Poison
      , ability = Nothing
      , imageUrl = imageUrl 94 "Gengar"
      , evolutionData = EvolvesFrom [ 1352 ] "Trade"
      , transformationData = DoesNotTransform
      }
    , { id = 1371
      , nationalDexNumber = 94
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Gengar" Mega
      , typing = Double Ghost Poison
      , ability = Nothing
      , imageUrl = imageUrlWithForm 94 "Gengar" Mega
      , evolutionData = EvolvesFrom [ 1353 ] "Holding Gengarite"
      , transformationData = DoesNotTransform
      }
    , { id = 1354
      , nationalDexNumber = 95
      , originalPokemonID = Nothing
      , fullName = "Onix"
      , typing = Double Rock Ground
      , ability = Nothing
      , imageUrl = imageUrl 95 "Onix"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 332
      , nationalDexNumber = 208
      , originalPokemonID = Nothing
      , fullName = "Steelix"
      , typing = Double Steel Ground
      , ability = Nothing
      , imageUrl = imageUrl 208 "Steelix"
      , evolutionData = EvolvesFrom [ 1354 ] "Trade holding Metal Coat"
      , transformationData = DoesNotTransform
      }
    , { id = 1383
      , nationalDexNumber = 208
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Steelix" Mega
      , typing = Double Steel Ground
      , ability = Nothing
      , imageUrl = imageUrlWithForm 208 "Steelix" Mega
      , evolutionData = EvolvesFrom [ 332 ] "Holding Steelixite"
      , transformationData = DoesNotTransform
      }
    , { id = 1355
      , nationalDexNumber = 96
      , originalPokemonID = Nothing
      , fullName = "Drowzee"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrl 96 "Drowzee"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1356
      , nationalDexNumber = 97
      , originalPokemonID = Nothing
      , fullName = "Hypno"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrl 97 "Hypno"
      , evolutionData = EvolvesFrom [ 1355 ] "Level 26"
      , transformationData = DoesNotTransform
      }
    , { id = 1357
      , nationalDexNumber = 98
      , originalPokemonID = Nothing
      , fullName = "Krabby"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 98 "Krabby"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1358
      , nationalDexNumber = 99
      , originalPokemonID = Nothing
      , fullName = "Kingler"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 99 "Kingler"
      , evolutionData = EvolvesFrom [ 1357 ] "Level 28"
      , transformationData = DoesNotTransform
      }
    , { id = 47
      , nationalDexNumber = 100
      , originalPokemonID = Nothing
      , fullName = "Voltorb"
      , typing = Single Electric
      , ability = Nothing
      , imageUrl = imageUrl 100 "Voltorb"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 105
      , nationalDexNumber = 101
      , originalPokemonID = Nothing
      , fullName = "Electrode"
      , typing = Single Electric
      , ability = Nothing
      , imageUrl = imageUrl 101 "Electrode"
      , evolutionData = EvolvesFrom [ 47 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 1621
      , nationalDexNumber = 100
      , originalPokemonID = Just 47
      , fullName = nameWithForm "Voltorb" Hisuian
      , typing = Double Electric Grass
      , ability = Nothing
      , imageUrl = imageUrlWithForm 100 "Voltorb" Hisuian
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1622
      , nationalDexNumber = 101
      , originalPokemonID = Just 105
      , fullName = nameWithForm "Electrode" Hisuian
      , typing = Double Electric Grass
      , ability = Nothing
      , imageUrl = imageUrlWithForm 101 "Electrode" Hisuian
      , evolutionData = EvolvesFrom [ 1621 ] "Use Leaf Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 136
      , nationalDexNumber = 102
      , originalPokemonID = Nothing
      , fullName = "Exeggcute"
      , typing = Double Grass Psychic
      , ability = Nothing
      , imageUrl = imageUrl 102 "Exeggcute"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 137
      , nationalDexNumber = 103
      , originalPokemonID = Nothing
      , fullName = "Exeggutor"
      , typing = Double Grass Psychic
      , ability = Nothing
      , imageUrl = imageUrl 103 "Exeggutor"
      , evolutionData = EvolvesFrom [ 136 ] "Use Leaf Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 1359
      , nationalDexNumber = 103
      , originalPokemonID = Just 137
      , fullName = nameWithForm "Exeggutor" Alolan
      , typing = Double Grass Dragon
      , ability = Nothing
      , imageUrl = imageUrlWithForm 103 "Exeggutor" Alolan
      , evolutionData = EvolvesFrom [ 136 ] "Use Leaf Stone in Alola"
      , transformationData = DoesNotTransform
      }
    , { id = 138
      , nationalDexNumber = 104
      , originalPokemonID = Nothing
      , fullName = "Cubone"
      , typing = Single Ground
      , ability = Just LightningRod
      , imageUrl = imageUrl 104 "Cubone"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 139
      , nationalDexNumber = 105
      , originalPokemonID = Nothing
      , fullName = "Marowak"
      , typing = Single Ground
      , ability = Just LightningRod
      , imageUrl = imageUrl 105 "Marowak"
      , evolutionData = EvolvesFrom [ 138 ] "Level 28"
      , transformationData = DoesNotTransform
      }
    , { id = 1374
      , nationalDexNumber = 105
      , originalPokemonID = Just 139
      , fullName = nameWithForm "Marowak" Alolan
      , typing = Double Ghost Fire
      , ability = Just LightningRod
      , imageUrl = imageUrlWithForm 105 "Marowak" Alolan
      , evolutionData = EvolvesFrom [ 138 ] "Level 28 at night"
      , transformationData = DoesNotTransform
      }
    , { id = 360
      , nationalDexNumber = 236
      , originalPokemonID = Nothing
      , fullName = "Tyrogue"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrl 236 "Tyrogue"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 140
      , nationalDexNumber = 106
      , originalPokemonID = Nothing
      , fullName = "Hitmonlee"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrl 106 "Hitmonlee"
      , evolutionData = EvolvesFrom [ 360 ] "Level 20 With Attack > Defense"
      , transformationData = DoesNotTransform
      }
    , { id = 141
      , nationalDexNumber = 107
      , originalPokemonID = Nothing
      , fullName = "Hitmonchan"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrl 107 "Hitmonchan"
      , evolutionData = EvolvesFrom [ 360 ] "Level 20 With Attack < Defense"
      , transformationData = DoesNotTransform
      }
    , { id = 361
      , nationalDexNumber = 237
      , originalPokemonID = Nothing
      , fullName = "Hitmontop"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrl 237 "Hitmontop"
      , evolutionData = EvolvesFrom [ 360 ] "Level 20 With Attack = Defense"
      , transformationData = DoesNotTransform
      }
    , { id = 142
      , nationalDexNumber = 108
      , originalPokemonID = Nothing
      , fullName = "Lickitung"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 108 "Lickitung"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 685
      , nationalDexNumber = 463
      , originalPokemonID = Nothing
      , fullName = "Lickilicky"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 463 "Lickilicky"
      , evolutionData = EvolvesFrom [ 142 ] "Level while knowing Rollout"
      , transformationData = DoesNotTransform
      }
    , { id = 143
      , nationalDexNumber = 109
      , originalPokemonID = Nothing
      , fullName = "Koffing"
      , typing = Single Poison
      , ability = Just Levitate
      , imageUrl = imageUrl 109 "Koffing"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 144
      , nationalDexNumber = 110
      , originalPokemonID = Nothing
      , fullName = "Weezing"
      , typing = Single Poison
      , ability = Just Levitate
      , imageUrl = imageUrl 110 "Weezing"
      , evolutionData = EvolvesFrom [ 143 ] "Level 35"
      , transformationData = DoesNotTransform
      }
    , { id = 1375
      , nationalDexNumber = 110
      , originalPokemonID = Just 144
      , fullName = nameWithForm "Weezing" Galarian
      , typing = Double Poison Fairy
      , ability = Just Levitate
      , imageUrl = imageUrlWithForm 110 "Weezing" Galarian
      , evolutionData = EvolvesFrom [ 143 ] "Level 35 In Galar"
      , transformationData = DoesNotTransform
      }
    , { id = 145
      , nationalDexNumber = 111
      , originalPokemonID = Nothing
      , fullName = "Rhyhorn"
      , typing = Double Ground Rock
      , ability = Just LightningRod
      , imageUrl = imageUrl 111 "Rhyhorn"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 146
      , nationalDexNumber = 112
      , originalPokemonID = Nothing
      , fullName = "Rhydon"
      , typing = Double Ground Rock
      , ability = Just LightningRod
      , imageUrl = imageUrl 112 "Rhydon"
      , evolutionData = EvolvesFrom [ 145 ] "Level 42"
      , transformationData = DoesNotTransform
      }
    , { id = 686
      , nationalDexNumber = 464
      , originalPokemonID = Nothing
      , fullName = "Rhyperior"
      , typing = Double Ground Rock
      , ability = Just LightningRod
      , imageUrl = imageUrl 464 "Rhyperior"
      , evolutionData = EvolvesFrom [ 146 ] "Trade holding Protector"
      , transformationData = DoesNotTransform
      }
    , { id = 638
      , nationalDexNumber = 440
      , originalPokemonID = Nothing
      , fullName = "Happiny"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 440 "Happiny"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 147
      , nationalDexNumber = 113
      , originalPokemonID = Nothing
      , fullName = "Chansey"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 113 "Chansey"
      , evolutionData = EvolvesFrom [ 638 ] "Level while holding an Oval Stone during the day"
      , transformationData = DoesNotTransform
      }
    , { id = 366
      , nationalDexNumber = 242
      , originalPokemonID = Nothing
      , fullName = "Blissey"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 242 "Blissey"
      , evolutionData = EvolvesFrom [ 147 ] "Level with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 148
      , nationalDexNumber = 114
      , originalPokemonID = Nothing
      , fullName = "Tangela"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrl 114 "Tangela"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 687
      , nationalDexNumber = 465
      , originalPokemonID = Nothing
      , fullName = "Tangrowth"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrl 465 "Tangrowth"
      , evolutionData = EvolvesFrom [ 148 ] "Level while knowing Ancient Power"
      , transformationData = DoesNotTransform
      }
    , { id = 211
      , nationalDexNumber = 115
      , originalPokemonID = Nothing
      , fullName = "Kangaskhan"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 115 "Kangaskhan"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1379
      , nationalDexNumber = 115
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Kangaskhan" Mega
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlWithForm 115 "Kangaskhan" Mega
      , evolutionData = EvolvesFrom [ 211 ] "Holding Kangaskhanite"
      , transformationData = DoesNotTransform
      }
    , { id = 212
      , nationalDexNumber = 116
      , originalPokemonID = Nothing
      , fullName = "Horsea"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 116 "Horsea"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 213
      , nationalDexNumber = 117
      , originalPokemonID = Nothing
      , fullName = "Seadra"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 117 "Seadra"
      , evolutionData = EvolvesFrom [ 212 ] "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 354
      , nationalDexNumber = 230
      , originalPokemonID = Nothing
      , fullName = "Kingdra"
      , typing = Double Water Dragon
      , ability = Nothing
      , imageUrl = imageUrl 230 "Kingdra"
      , evolutionData = EvolvesFrom [ 213 ] "Trade holding Dragon Scale"
      , transformationData = DoesNotTransform
      }
    , { id = 214
      , nationalDexNumber = 118
      , originalPokemonID = Nothing
      , fullName = "Goldeen"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 118 "Goldeen"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 215
      , nationalDexNumber = 119
      , originalPokemonID = Nothing
      , fullName = "Seaking"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 119 "Seaking"
      , evolutionData = EvolvesFrom [ 214 ] "Level 33"
      , transformationData = DoesNotTransform
      }
    , { id = 216
      , nationalDexNumber = 120
      , originalPokemonID = Nothing
      , fullName = "Staryu"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 120 "Staryu"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 217
      , nationalDexNumber = 121
      , originalPokemonID = Nothing
      , fullName = "Starmie"
      , typing = Double Water Psychic
      , ability = Nothing
      , imageUrl = imageUrl 121 "Starmie"
      , evolutionData = EvolvesFrom [ 216 ] "Use Water Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 637
      , nationalDexNumber = 439
      , originalPokemonID = Nothing
      , fullName = "Mime Jr."
      , typing = Double Psychic Fairy
      , ability = Nothing
      , imageUrl = imageUrl 439 "Mime Jr"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 218
      , nationalDexNumber = 122
      , originalPokemonID = Nothing
      , fullName = "Mr. Mime"
      , typing = Double Psychic Fairy
      , ability = Nothing
      , imageUrl = imageUrl 122 "Mr. Mime"
      , evolutionData = EvolvesFrom [ 637 ] "Level while knowing Mimic"
      , transformationData = DoesNotTransform
      }
    , { id = 1377
      , nationalDexNumber = 122
      , originalPokemonID = Just 218
      , fullName = nameWithForm "Mr. Mime" Galarian
      , typing = Double Ice Psychic
      , ability = Nothing
      , imageUrl = imageUrlWithForm 122 "Mr. Mime" Galarian
      , evolutionData = EvolvesFrom [ 637 ] "Level while knowing Mimic in Galar"
      , transformationData = DoesNotTransform
      }
    , { id = 1376
      , nationalDexNumber = 866
      , originalPokemonID = Nothing
      , fullName = "Mr. Rime"
      , typing = Double Ice Psychic
      , ability = Nothing
      , imageUrl = imageUrl 866 "Mr. Rime"
      , evolutionData = EvolvesFrom [ 1377 ] "Level 42"
      , transformationData = DoesNotTransform
      }
    , { id = 219
      , nationalDexNumber = 123
      , originalPokemonID = Nothing
      , fullName = "Scyther"
      , typing = Double Bug Flying
      , ability = Nothing
      , imageUrl = imageUrl 123 "Scyther"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 336
      , nationalDexNumber = 212
      , originalPokemonID = Nothing
      , fullName = "Scizor"
      , typing = Double Bug Steel
      , ability = Nothing
      , imageUrl = imageUrl 212 "Scizor"
      , evolutionData = EvolvesFrom [ 219 ] "Trade holding Metal Coat"
      , transformationData = DoesNotTransform
      }
    , { id = 1384
      , nationalDexNumber = 212
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Scizor" Mega
      , typing = Double Bug Steel
      , ability = Nothing
      , imageUrl = imageUrlWithForm 212 "Scizor" Mega
      , evolutionData = EvolvesFrom [ 336 ] "Holding Scizorite"
      , transformationData = DoesNotTransform
      }
    , { id = 1610
      , nationalDexNumber = 900
      , originalPokemonID = Nothing
      , fullName = "Kleavor"
      , typing = Double Bug Rock
      , ability = Nothing
      , imageUrl = imageUrl 900 "Kleavor"
      , evolutionData = EvolvesFrom [ 219 ] "Use Black Augurite"
      , transformationData = DoesNotTransform
      }
    , { id = 362
      , nationalDexNumber = 238
      , originalPokemonID = Nothing
      , fullName = "Smoochum"
      , typing = Double Ice Psychic
      , ability = Nothing
      , imageUrl = imageUrl 238 "Smoochum"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 220
      , nationalDexNumber = 124
      , originalPokemonID = Nothing
      , fullName = "Jynx"
      , typing = Double Ice Psychic
      , ability = Nothing
      , imageUrl = imageUrl 124 "Jynx"
      , evolutionData = EvolvesFrom [ 362 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 363
      , nationalDexNumber = 239
      , originalPokemonID = Nothing
      , fullName = "Elekid"
      , typing = Single Electric
      , ability = Nothing
      , imageUrl = imageUrl 239 "Elekid"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 221
      , nationalDexNumber = 125
      , originalPokemonID = Nothing
      , fullName = "Electabuzz"
      , typing = Single Electric
      , ability = Nothing
      , imageUrl = imageUrl 125 "Electabuzz"
      , evolutionData = EvolvesFrom [ 363 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 688
      , nationalDexNumber = 466
      , originalPokemonID = Nothing
      , fullName = "Electivire"
      , typing = Single Electric
      , ability = Just MotorDrive
      , imageUrl = imageUrl 466 "Electivire"
      , evolutionData = EvolvesFrom [ 221 ] "Trade holding Electirizer"
      , transformationData = DoesNotTransform
      }
    , { id = 364
      , nationalDexNumber = 240
      , originalPokemonID = Nothing
      , fullName = "Magby"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrl 240 "Magby"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 222
      , nationalDexNumber = 126
      , originalPokemonID = Nothing
      , fullName = "Magmar"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrl 126 "Magmar"
      , evolutionData = EvolvesFrom [ 364 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 699
      , nationalDexNumber = 467
      , originalPokemonID = Nothing
      , fullName = "Magmortar"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrl 467 "Magmortar"
      , evolutionData = EvolvesFrom [ 222 ] "Trade holding Magmarizer"
      , transformationData = DoesNotTransform
      }
    , { id = 223
      , nationalDexNumber = 127
      , originalPokemonID = Nothing
      , fullName = "Pinsir"
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imageUrl 127 "Pinsir"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1380
      , nationalDexNumber = 127
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Pinsir" Mega
      , typing = Double Bug Flying
      , ability = Nothing
      , imageUrl = imageUrlWithForm 127 "Pinsir" Mega
      , evolutionData = EvolvesFrom [ 223 ] "Holding Pinsirite"
      , transformationData = DoesNotTransform
      }
    , { id = 224
      , nationalDexNumber = 128
      , originalPokemonID = Nothing
      , fullName = "Tauros"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 128 "Tauros"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1678
      , nationalDexNumber = 128
      , originalPokemonID = Just 224
      , fullName = nameWithForm "Tauros" Paldean
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrlWithForm 128 "Tauros" <| Unique "" "Paldea_Combat"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1679
      , nationalDexNumber = 128
      , originalPokemonID = Just 224
      , fullName = nameWithForm "Tauros" <| Unique "Paldean" "Aqua"
      , typing = Double Fighting Fire
      , ability = Nothing
      , imageUrl = imageUrlWithForm 128 "Tauros" <| Unique "" "Paldea_Aqua"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1680
      , nationalDexNumber = 128
      , originalPokemonID = Just 224
      , fullName = nameWithForm "Tauros" <| Unique "Paldean" "Blaze"
      , typing = Double Fighting Water
      , ability = Nothing
      , imageUrl = imageUrlWithForm 128 "Tauros" <| Unique "" "Paldea_Blaze"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 225
      , nationalDexNumber = 129
      , originalPokemonID = Nothing
      , fullName = "Magikarp"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 129 "Magikarp"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 226
      , nationalDexNumber = 130
      , originalPokemonID = Nothing
      , fullName = "Gyarados"
      , typing = Double Water Flying
      , ability = Nothing
      , imageUrl = imageUrl 130 "Gyarados"
      , evolutionData = EvolvesFrom [ 225 ] "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 1381
      , nationalDexNumber = 130
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Gyarados" Mega
      , typing = Double Water Dark
      , ability = Nothing
      , imageUrl = imageUrlWithForm 130 "Gyarados" Mega
      , evolutionData = EvolvesFrom [ 226 ] "Holding Gyaradosite"
      , transformationData = DoesNotTransform
      }
    , { id = 227
      , nationalDexNumber = 131
      , originalPokemonID = Nothing
      , fullName = "Lapras"
      , typing = Double Water Ice
      , ability = Just WaterAbsorb
      , imageUrl = imageUrl 131 "Lapras"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 29
      , nationalDexNumber = 132
      , originalPokemonID = Nothing
      , fullName = "Ditto"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 132 "Ditto"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 228
      , nationalDexNumber = 133
      , originalPokemonID = Nothing
      , fullName = "Eevee"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 133 "Eevee"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 229
      , nationalDexNumber = 134
      , originalPokemonID = Nothing
      , fullName = "Vaporeon"
      , typing = Single Water
      , ability = Just WaterAbsorb
      , imageUrl = imageUrl 134 "Vaporeon"
      , evolutionData = EvolvesFrom [ 228 ] "Use Water Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 230
      , nationalDexNumber = 135
      , originalPokemonID = Nothing
      , fullName = "Jolteon"
      , typing = Single Electric
      , ability = Just VoltAbsorb
      , imageUrl = imageUrl 135 "Jolteon"
      , evolutionData = EvolvesFrom [ 228 ] "Use Thunder Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 231
      , nationalDexNumber = 136
      , originalPokemonID = Nothing
      , fullName = "Flareon"
      , typing = Single Fire
      , ability = Just FlashFire
      , imageUrl = imageUrl 136 "Flareon"
      , evolutionData = EvolvesFrom [ 228 ] "Use Fire Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 296
      , nationalDexNumber = 196
      , originalPokemonID = Nothing
      , fullName = "Espeon"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrl 196 "Espeon"
      , evolutionData = EvolvesFrom [ 228 ] "Level during the day with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 297
      , nationalDexNumber = 197
      , originalPokemonID = Nothing
      , fullName = "Umbreon"
      , typing = Single Dark
      , ability = Nothing
      , imageUrl = imageUrl 197 "Umbreon"
      , evolutionData = EvolvesFrom [ 228 ] "Level during the night with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 715
      , nationalDexNumber = 470
      , originalPokemonID = Nothing
      , fullName = "Leafeon"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrl 470 "Leafeon"
      , evolutionData = EvolvesFrom [ 228 ] "Use Leaf Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 716
      , nationalDexNumber = 471
      , originalPokemonID = Nothing
      , fullName = "Glaceon"
      , typing = Single Ice
      , ability = Nothing
      , imageUrl = imageUrl 471 "Glaceon"
      , evolutionData = EvolvesFrom [ 228 ] "Use Ice Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 1129
      , nationalDexNumber = 700
      , originalPokemonID = Nothing
      , fullName = "Sylveon"
      , typing = Single Fairy
      , ability = Nothing
      , imageUrl = imageUrl 700 "Sylveon"
      , evolutionData = EvolvesFrom [ 228 ] "Level while knowing a Fairy move with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 232
      , nationalDexNumber = 137
      , originalPokemonID = Nothing
      , fullName = "Porygon"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 137 "Porygon"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 357
      , nationalDexNumber = 233
      , originalPokemonID = Nothing
      , fullName = "Porygon2"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 233 "Porygon2"
      , evolutionData = EvolvesFrom [ 232 ] "Trade holding Upgrade"
      , transformationData = DoesNotTransform
      }
    , { id = 719
      , nationalDexNumber = 474
      , originalPokemonID = Nothing
      , fullName = "Porygon-Z"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 474 "Porygon-Z"
      , evolutionData = EvolvesFrom [ 357 ] "Trade holding Dubious Disc"
      , transformationData = DoesNotTransform
      }
    , { id = 233
      , nationalDexNumber = 138
      , originalPokemonID = Nothing
      , fullName = "Omanyte"
      , typing = Double Rock Water
      , ability = Nothing
      , imageUrl = imageUrl 138 "Omanyte"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 234
      , nationalDexNumber = 139
      , originalPokemonID = Nothing
      , fullName = "Omastar"
      , typing = Double Rock Water
      , ability = Nothing
      , imageUrl = imageUrl 139 "Omastar"
      , evolutionData = EvolvesFrom [ 233 ] "Level 40"
      , transformationData = DoesNotTransform
      }
    , { id = 235
      , nationalDexNumber = 140
      , originalPokemonID = Nothing
      , fullName = "Kabuto"
      , typing = Double Rock Water
      , ability = Nothing
      , imageUrl = imageUrl 140 "Kabuto"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 236
      , nationalDexNumber = 141
      , originalPokemonID = Nothing
      , fullName = "Kabutops"
      , typing = Double Rock Water
      , ability = Nothing
      , imageUrl = imageUrl 141 "Kabutops"
      , evolutionData = EvolvesFrom [ 235 ] "Level 40"
      , transformationData = DoesNotTransform
      }
    , { id = 237
      , nationalDexNumber = 142
      , originalPokemonID = Nothing
      , fullName = "Aerodactyl"
      , typing = Double Rock Flying
      , ability = Nothing
      , imageUrl = imageUrl 142 "Aerodactyl"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1382
      , nationalDexNumber = 142
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Aerodactyl" Mega
      , typing = Double Rock Flying
      , ability = Nothing
      , imageUrl = imageUrlWithForm 142 "Aerodactyl" Mega
      , evolutionData = EvolvesFrom [ 237 ] "Holding Aerodactylite"
      , transformationData = DoesNotTransform
      }
    , { id = 644
      , nationalDexNumber = 446
      , originalPokemonID = Nothing
      , fullName = "Munchlax"
      , typing = Single Normal
      , ability = Just ThickFat
      , imageUrl = imageUrl 446 "Munchlax"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 238
      , nationalDexNumber = 143
      , originalPokemonID = Nothing
      , fullName = "Snorlax"
      , typing = Single Normal
      , ability = Just ThickFat
      , imageUrl = imageUrl 143 "Snorlax"
      , evolutionData = EvolvesFrom [ 644 ] "Level with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 239
      , nationalDexNumber = 144
      , originalPokemonID = Nothing
      , fullName = "Articuno"
      , typing = Double Ice Flying
      , ability = Nothing
      , imageUrl = imageUrl 144 "Articuno"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1385
      , nationalDexNumber = 144
      , originalPokemonID = Just 239
      , fullName = nameWithForm "Articuno" Galarian
      , typing = Double Psychic Flying
      , ability = Nothing
      , imageUrl = imageUrlWithForm 144 "Articuno" Galarian
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 240
      , nationalDexNumber = 145
      , originalPokemonID = Nothing
      , fullName = "Zapdos"
      , typing = Double Electric Flying
      , ability = Nothing
      , imageUrl = imageUrl 145 "Zapdos"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1386
      , nationalDexNumber = 145
      , originalPokemonID = Just 240
      , fullName = nameWithForm "Zapdos" Galarian
      , typing = Double Fighting Flying
      , ability = Nothing
      , imageUrl = imageUrlWithForm 145 "Zapdos" Galarian
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 241
      , nationalDexNumber = 146
      , originalPokemonID = Nothing
      , fullName = "Moltres"
      , typing = Double Fire Flying
      , ability = Nothing
      , imageUrl = imageUrl 146 "Moltres"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1387
      , nationalDexNumber = 146
      , originalPokemonID = Just 241
      , fullName = nameWithForm "Moltres" Galarian
      , typing = Double Dark Flying
      , ability = Nothing
      , imageUrl = imageUrlWithForm 146 "Moltres" Galarian
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 242
      , nationalDexNumber = 147
      , originalPokemonID = Nothing
      , fullName = "Dratini"
      , typing = Single Dragon
      , ability = Nothing
      , imageUrl = imageUrl 147 "Dratini"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 243
      , nationalDexNumber = 148
      , originalPokemonID = Nothing
      , fullName = "Dragonair"
      , typing = Single Dragon
      , ability = Nothing
      , imageUrl = imageUrl 148 "Dragonair"
      , evolutionData = EvolvesFrom [ 242 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 244
      , nationalDexNumber = 149
      , originalPokemonID = Nothing
      , fullName = "Dragonite"
      , typing = Double Dragon Flying
      , ability = Nothing
      , imageUrl = imageUrl 149 "Dragonite"
      , evolutionData = EvolvesFrom [ 243 ] "Level 55"
      , transformationData = DoesNotTransform
      }
    , { id = 43
      , nationalDexNumber = 150
      , originalPokemonID = Nothing
      , fullName = "Mewtwo"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrl 150 "Mewtwo"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 44
      , nationalDexNumber = 150
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Mewtwo" MegaX
      , typing = Double Psychic Fighting
      , ability = Nothing
      , imageUrl = imageUrlWithForm 150 "Mewtwo" MegaX
      , evolutionData = EvolvesFrom [ 43 ] "Holding Mewtwonite X"
      , transformationData = DoesNotTransform
      }
    , { id = 45
      , nationalDexNumber = 150
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Mewtwo" MegaY
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrlWithForm 150 "Mewtwo" MegaY
      , evolutionData = EvolvesFrom [ 43 ] "Holding Mewtwonite Y"
      , transformationData = DoesNotTransform
      }
    , { id = 42
      , nationalDexNumber = 151
      , originalPokemonID = Nothing
      , fullName = "Mew"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrl 151 "Mew"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 245
      , nationalDexNumber = 152
      , originalPokemonID = Nothing
      , fullName = "Chikorita"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrl 152 "Chikorita"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 246
      , nationalDexNumber = 153
      , originalPokemonID = Nothing
      , fullName = "Bayleef"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrl 153 "Bayleef"
      , evolutionData = EvolvesFrom [ 245 ] "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 247
      , nationalDexNumber = 154
      , originalPokemonID = Nothing
      , fullName = "Meganium"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrl 154 "Meganium"
      , evolutionData = EvolvesFrom [ 246 ] "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 248
      , nationalDexNumber = 155
      , originalPokemonID = Nothing
      , fullName = "Cyndaquil"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrl 155 "Cyndaquil"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 249
      , nationalDexNumber = 156
      , originalPokemonID = Nothing
      , fullName = "Quilava"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrl 156 "Quilava"
      , evolutionData = EvolvesFrom [ 248 ] "Level 14 (17 in Legends: Arceus)"
      , transformationData = DoesNotTransform
      }
    , { id = 250
      , nationalDexNumber = 157
      , originalPokemonID = Nothing
      , fullName = "Typhlosion"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrl 157 "Typhlosion"
      , evolutionData = EvolvesFrom [ 249 ] "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 1623
      , nationalDexNumber = 157
      , originalPokemonID = Just 250
      , fullName = nameWithForm "Typhlosion" Hisuian
      , typing = Double Fire Ghost
      , ability = Nothing
      , imageUrl = imageUrlWithForm 157 "Typhlosion" Hisuian
      , evolutionData = EvolvesFrom [ 249 ] "Level 36 in Legends: Arceus"
      , transformationData = DoesNotTransform
      }
    , { id = 251
      , nationalDexNumber = 158
      , originalPokemonID = Nothing
      , fullName = "Totodile"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 158 "Totodile"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 252
      , nationalDexNumber = 159
      , originalPokemonID = Nothing
      , fullName = "Croconaw"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 159 "Croconaw"
      , evolutionData = EvolvesFrom [ 251 ] "Level 18"
      , transformationData = DoesNotTransform
      }
    , { id = 253
      , nationalDexNumber = 160
      , originalPokemonID = Nothing
      , fullName = "Feraligatr"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 160 "Feraligatr"
      , evolutionData = EvolvesFrom [ 252 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 254
      , nationalDexNumber = 161
      , originalPokemonID = Nothing
      , fullName = "Sentret"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 161 "Sentret"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 255
      , nationalDexNumber = 162
      , originalPokemonID = Nothing
      , fullName = "Furret"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 162 "Furret"
      , evolutionData = EvolvesFrom [ 254 ] "Level 15"
      , transformationData = DoesNotTransform
      }
    , { id = 256
      , nationalDexNumber = 163
      , originalPokemonID = Nothing
      , fullName = "Hoothoot"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrl 163 "Hoothoot"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 257
      , nationalDexNumber = 164
      , originalPokemonID = Nothing
      , fullName = "Noctowl"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrl 164 "Noctowl"
      , evolutionData = EvolvesFrom [ 256 ] "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 258
      , nationalDexNumber = 165
      , originalPokemonID = Nothing
      , fullName = "Ledyba"
      , typing = Double Bug Flying
      , ability = Nothing
      , imageUrl = imageUrl 165 "Ledyba"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 259
      , nationalDexNumber = 166
      , originalPokemonID = Nothing
      , fullName = "Ledian"
      , typing = Double Bug Flying
      , ability = Nothing
      , imageUrl = imageUrl 166 "Ledian"
      , evolutionData = EvolvesFrom [ 258 ] "Level 18"
      , transformationData = DoesNotTransform
      }
    , { id = 260
      , nationalDexNumber = 167
      , originalPokemonID = Nothing
      , fullName = "Spinarak"
      , typing = Double Bug Poison
      , ability = Nothing
      , imageUrl = imageUrl 167 "Spinarak"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 261
      , nationalDexNumber = 168
      , originalPokemonID = Nothing
      , fullName = "Ariados"
      , typing = Double Bug Poison
      , ability = Nothing
      , imageUrl = imageUrl 168 "Ariados"
      , evolutionData = EvolvesFrom [ 260 ] "Level 22"
      , transformationData = DoesNotTransform
      }
    , { id = 262
      , nationalDexNumber = 170
      , originalPokemonID = Nothing
      , fullName = "Chinchou"
      , typing = Double Water Electric
      , ability = Just VoltAbsorb
      , imageUrl = imageUrl 170 "Chinchou"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 263
      , nationalDexNumber = 171
      , originalPokemonID = Nothing
      , fullName = "Lanturn"
      , typing = Double Water Electric
      , ability = Just VoltAbsorb
      , imageUrl = imageUrl 171 "Lanturn"
      , evolutionData = EvolvesFrom [ 262 ] "Level 27"
      , transformationData = DoesNotTransform
      }
    , { id = 264
      , nationalDexNumber = 175
      , originalPokemonID = Nothing
      , fullName = "Togepi"
      , typing = Single Fairy
      , ability = Nothing
      , imageUrl = imageUrl 175 "Togepi"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 265
      , nationalDexNumber = 176
      , originalPokemonID = Nothing
      , fullName = "Togetic"
      , typing = Double Fairy Flying
      , ability = Nothing
      , imageUrl = imageUrl 176 "Togetic"
      , evolutionData = EvolvesFrom [ 264 ] "Level with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 713
      , nationalDexNumber = 468
      , originalPokemonID = Nothing
      , fullName = "Togekiss"
      , typing = Double Fairy Flying
      , ability = Nothing
      , imageUrl = imageUrl 468 "Togekiss"
      , evolutionData = EvolvesFrom [ 265 ] "Use Shiny Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 266
      , nationalDexNumber = 177
      , originalPokemonID = Nothing
      , fullName = "Natu"
      , typing = Double Psychic Flying
      , ability = Nothing
      , imageUrl = imageUrl 177 "Natu"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 267
      , nationalDexNumber = 178
      , originalPokemonID = Nothing
      , fullName = "Xatu"
      , typing = Double Psychic Flying
      , ability = Nothing
      , imageUrl = imageUrl 178 "Xatu"
      , evolutionData = EvolvesFrom [ 266 ] "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 278
      , nationalDexNumber = 179
      , originalPokemonID = Nothing
      , fullName = "Mareep"
      , typing = Single Electric
      , ability = Nothing
      , imageUrl = imageUrl 179 "Mareep"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 279
      , nationalDexNumber = 180
      , originalPokemonID = Nothing
      , fullName = "Flaaffy"
      , typing = Single Electric
      , ability = Nothing
      , imageUrl = imageUrl 180 "Flaaffy"
      , evolutionData = EvolvesFrom [ 278 ] "Level 15"
      , transformationData = DoesNotTransform
      }
    , { id = 280
      , nationalDexNumber = 181
      , originalPokemonID = Nothing
      , fullName = "Ampharos"
      , typing = Single Electric
      , ability = Nothing
      , imageUrl = imageUrl 181 "Ampharos"
      , evolutionData = EvolvesFrom [ 279 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 1394
      , nationalDexNumber = 181
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Ampharos" Mega
      , typing = Double Electric Dragon
      , ability = Nothing
      , imageUrl = imageUrlWithForm 181 "Ampharos" Mega
      , evolutionData = EvolvesFrom [ 280 ] "Holding Ampharosite"
      , transformationData = DoesNotTransform
      }
    , { id = 426
      , nationalDexNumber = 298
      , originalPokemonID = Nothing
      , fullName = "Azurill"
      , typing = Double Normal Fairy
      , ability = Just ThickFat
      , imageUrl = imageUrl 298 "Azurill"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 283
      , nationalDexNumber = 183
      , originalPokemonID = Nothing
      , fullName = "Marill"
      , typing = Double Water Fairy
      , ability = Just ThickFat
      , imageUrl = imageUrl 183 "Marill"
      , evolutionData = EvolvesFrom [ 426 ] "Level with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 284
      , nationalDexNumber = 184
      , originalPokemonID = Nothing
      , fullName = "Azumarill"
      , typing = Double Water Fairy
      , ability = Just ThickFat
      , imageUrl = imageUrl 184 "Azumarill"
      , evolutionData = EvolvesFrom [ 283 ] "Level 18"
      , transformationData = DoesNotTransform
      }
    , { id = 636
      , nationalDexNumber = 438
      , originalPokemonID = Nothing
      , fullName = "Bonsly"
      , typing = Single Rock
      , ability = Nothing
      , imageUrl = imageUrl 438 "Bonsly"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 285
      , nationalDexNumber = 185
      , originalPokemonID = Nothing
      , fullName = "Sudowoodo"
      , typing = Single Rock
      , ability = Nothing
      , imageUrl = imageUrl 185 "Sudowoodo"
      , evolutionData = EvolvesFrom [ 636 ] "Level while knowing Mimic"
      , transformationData = DoesNotTransform
      }
    , { id = 287
      , nationalDexNumber = 187
      , originalPokemonID = Nothing
      , fullName = "Hoppip"
      , typing = Double Grass Flying
      , ability = Nothing
      , imageUrl = imageUrl 187 "Hoppip"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 288
      , nationalDexNumber = 188
      , originalPokemonID = Nothing
      , fullName = "Skiploom"
      , typing = Double Grass Flying
      , ability = Nothing
      , imageUrl = imageUrl 188 "Skiploom"
      , evolutionData = EvolvesFrom [ 287 ] "Level 18"
      , transformationData = DoesNotTransform
      }
    , { id = 289
      , nationalDexNumber = 189
      , originalPokemonID = Nothing
      , fullName = "Jumpluff"
      , typing = Double Grass Flying
      , ability = Nothing
      , imageUrl = imageUrl 189 "Jumpluff"
      , evolutionData = EvolvesFrom [ 288 ] "Level 27"
      , transformationData = DoesNotTransform
      }
    , { id = 290
      , nationalDexNumber = 190
      , originalPokemonID = Nothing
      , fullName = "Aipom"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 190 "Aipom"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 622
      , nationalDexNumber = 424
      , originalPokemonID = Nothing
      , fullName = "Ambipom"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 424 "Ambipom"
      , evolutionData = EvolvesFrom [ 290 ] "Level while knowing Double Hit"
      , transformationData = DoesNotTransform
      }
    , { id = 291
      , nationalDexNumber = 191
      , originalPokemonID = Nothing
      , fullName = "Sunkern"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrl 191 "Sunkern"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 292
      , nationalDexNumber = 192
      , originalPokemonID = Nothing
      , fullName = "Sunflora"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrl 192 "Sunflora"
      , evolutionData = EvolvesFrom [ 291 ] "Use Sun Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 293
      , nationalDexNumber = 193
      , originalPokemonID = Nothing
      , fullName = "Yanma"
      , typing = Double Bug Flying
      , ability = Nothing
      , imageUrl = imageUrl 193 "Yanma"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 714
      , nationalDexNumber = 469
      , originalPokemonID = Nothing
      , fullName = "Yanmega"
      , typing = Double Bug Flying
      , ability = Nothing
      , imageUrl = imageUrl 469 "Yanmega"
      , evolutionData = EvolvesFrom [ 293 ] "Level while knowing Ancient Power"
      , transformationData = DoesNotTransform
      }
    , { id = 294
      , nationalDexNumber = 194
      , originalPokemonID = Nothing
      , fullName = "Wooper"
      , typing = Double Water Ground
      , ability = Just WaterAbsorb
      , imageUrl = imageUrl 194 "Wooper"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 295
      , nationalDexNumber = 195
      , originalPokemonID = Nothing
      , fullName = "Quagsire"
      , typing = Double Water Ground
      , ability = Just WaterAbsorb
      , imageUrl = imageUrl 195 "Quagsire"
      , evolutionData = EvolvesFrom [ 294 ] "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 1651
      , nationalDexNumber = 194
      , originalPokemonID = Just 294
      , fullName = nameWithForm "Wooper" Paldean
      , typing = Double Poison Ground
      , ability = Just WaterAbsorb
      , imageUrl = imageUrlWithForm 194 "Wooper" Paldean
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1652
      , nationalDexNumber = 980
      , originalPokemonID = Nothing
      , fullName = "Clodsire"
      , typing = Double Poison Ground
      , ability = Just WaterAbsorb
      , imageUrl = imageUrl 980 "Clodsire"
      , evolutionData = EvolvesFrom [ 1651 ] "Level 28"
      , transformationData = DoesNotTransform
      }
    , { id = 298
      , nationalDexNumber = 198
      , originalPokemonID = Nothing
      , fullName = "Murkrow"
      , typing = Double Dark Flying
      , ability = Nothing
      , imageUrl = imageUrl 198 "Murkrow"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 628
      , nationalDexNumber = 430
      , originalPokemonID = Nothing
      , fullName = "Honchkrow"
      , typing = Double Dark Flying
      , ability = Nothing
      , imageUrl = imageUrl 430 "Honchkrow"
      , evolutionData = EvolvesFrom [ 298 ] "Use Dusk Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 324
      , nationalDexNumber = 200
      , originalPokemonID = Nothing
      , fullName = "Misdreavus"
      , typing = Single Ghost
      , ability = Just Levitate
      , imageUrl = imageUrl 200 "Misdreavus"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 627
      , nationalDexNumber = 429
      , originalPokemonID = Nothing
      , fullName = "Mismagius"
      , typing = Single Ghost
      , ability = Just Levitate
      , imageUrl = imageUrl 429 "Mismagius"
      , evolutionData = EvolvesFrom [ 324 ] "Use Dusk Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 325
      , nationalDexNumber = 201
      , originalPokemonID = Nothing
      , fullName = "Unown"
      , typing = Single Psychic
      , ability = Just Levitate
      , imageUrl = imageUrl 201 "Unown"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 488
      , nationalDexNumber = 360
      , originalPokemonID = Nothing
      , fullName = "Wynaut"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrl 360 "Wynaut"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 326
      , nationalDexNumber = 202
      , originalPokemonID = Nothing
      , fullName = "Wobbuffet"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrl 202 "Wobbuffet"
      , evolutionData = EvolvesFrom [ 488 ] "Level 15"
      , transformationData = DoesNotTransform
      }
    , { id = 327
      , nationalDexNumber = 203
      , originalPokemonID = Nothing
      , fullName = "Girafarig"
      , typing = Double Normal Psychic
      , ability = Nothing
      , imageUrl = imageUrl 203 "Girafarig"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1673
      , nationalDexNumber = 981
      , originalPokemonID = Nothing
      , fullName = "Farigiraf"
      , typing = Double Normal Psychic
      , ability = Nothing
      , imageUrl = imageUrl 981 "Farigiraf"
      , evolutionData = EvolvesFrom [ 327 ] "Level while knowing Twin Beam"
      , transformationData = DoesNotTransform
      }
    , { id = 328
      , nationalDexNumber = 204
      , originalPokemonID = Nothing
      , fullName = "Pineco"
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imageUrl 204 "Pineco"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 329
      , nationalDexNumber = 205
      , originalPokemonID = Nothing
      , fullName = "Forretress"
      , typing = Double Bug Steel
      , ability = Nothing
      , imageUrl = imageUrl 205 "Forretress"
      , evolutionData = EvolvesFrom [ 328 ] "Level 31"
      , transformationData = DoesNotTransform
      }
    , { id = 330
      , nationalDexNumber = 206
      , originalPokemonID = Nothing
      , fullName = "Dunsparce"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 206 "Dunsparce"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1672
      , nationalDexNumber = 982
      , originalPokemonID = Nothing
      , fullName = "Dudunsparce"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 982 "Dudunsparce"
      , evolutionData = EvolvesFrom [ 330 ] "Level while knowing Hyper Drill"
      , transformationData = DoesNotTransform
      }
    , { id = 331
      , nationalDexNumber = 207
      , originalPokemonID = Nothing
      , fullName = "Gligar"
      , typing = Double Ground Flying
      , ability = Nothing
      , imageUrl = imageUrl 207 "Gligar"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 717
      , nationalDexNumber = 472
      , originalPokemonID = Nothing
      , fullName = "Gliscor"
      , typing = Double Ground Flying
      , ability = Nothing
      , imageUrl = imageUrl 472 "Gliscor"
      , evolutionData = EvolvesFrom [ 331 ] "Level while holding Razor Fang at night"
      , transformationData = DoesNotTransform
      }
    , { id = 333
      , nationalDexNumber = 209
      , originalPokemonID = Nothing
      , fullName = "Snubbull"
      , typing = Single Fairy
      , ability = Nothing
      , imageUrl = imageUrl 209 "Snubbull"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 334
      , nationalDexNumber = 210
      , originalPokemonID = Nothing
      , fullName = "Granbull"
      , typing = Single Fairy
      , ability = Nothing
      , imageUrl = imageUrl 210 "Granbull"
      , evolutionData = EvolvesFrom [ 333 ] "Level 23"
      , transformationData = DoesNotTransform
      }
    , { id = 335
      , nationalDexNumber = 211
      , originalPokemonID = Nothing
      , fullName = "Qwilfish"
      , typing = Double Water Poison
      , ability = Nothing
      , imageUrl = imageUrl 211 "Qwilfish"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1615
      , nationalDexNumber = 211
      , originalPokemonID = Just 335
      , fullName = nameWithForm "Qwilfish" Hisuian
      , typing = Double Dark Poison
      , ability = Nothing
      , imageUrl = imageUrlWithForm 211 "Qwilfish" Hisuian
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1616
      , nationalDexNumber = 904
      , originalPokemonID = Nothing
      , fullName = "Overqwil"
      , typing = Double Dark Poison
      , ability = Nothing
      , imageUrl = imageUrl 904 "Overqwil"
      , evolutionData = EvolvesFrom [ 1615 ] "Use Barb Barrage in Strong Style 20 times"
      , transformationData = DoesNotTransform
      }
    , { id = 337
      , nationalDexNumber = 213
      , originalPokemonID = Nothing
      , fullName = "Shuckle"
      , typing = Double Bug Rock
      , ability = Nothing
      , imageUrl = imageUrl 213 "Shuckle"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 338
      , nationalDexNumber = 214
      , originalPokemonID = Nothing
      , fullName = "Heracross"
      , typing = Double Bug Fighting
      , ability = Nothing
      , imageUrl = imageUrl 214 "Heracross"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1395
      , nationalDexNumber = 214
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Heracross" Mega
      , typing = Double Bug Fighting
      , ability = Nothing
      , imageUrl = imageUrlWithForm 214 "Heracross" Mega
      , evolutionData = EvolvesFrom [ 338 ] "Holding Heracronite"
      , transformationData = DoesNotTransform
      }
    , { id = 339
      , nationalDexNumber = 215
      , originalPokemonID = Nothing
      , fullName = "Sneasel"
      , typing = Double Dark Ice
      , ability = Nothing
      , imageUrl = imageUrl 215 "Sneasel"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 683
      , nationalDexNumber = 461
      , originalPokemonID = Nothing
      , fullName = "Weavile"
      , typing = Double Dark Ice
      , ability = Nothing
      , imageUrl = imageUrl 461 "Weavile"
      , evolutionData = EvolvesFrom [ 339 ] "Level while holding Razor Claw at night"
      , transformationData = DoesNotTransform
      }
    , { id = 1613
      , nationalDexNumber = 215
      , originalPokemonID = Just 339
      , fullName = nameWithForm "Sneasel" Hisuian
      , typing = Double Fighting Poison
      , ability = Nothing
      , imageUrl = imageUrlWithForm 215 "Sneasel" Hisuian
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1614
      , nationalDexNumber = 903
      , originalPokemonID = Nothing
      , fullName = "Sneasler"
      , typing = Double Fighting Poison
      , ability = Nothing
      , imageUrl = imageUrl 903 "Sneasler"
      , evolutionData = EvolvesFrom [ 1613 ] "Use Razor Claw during the day"
      , transformationData = DoesNotTransform
      }
    , { id = 340
      , nationalDexNumber = 216
      , originalPokemonID = Nothing
      , fullName = "Teddiursa"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 216 "Teddiursa"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 341
      , nationalDexNumber = 217
      , originalPokemonID = Nothing
      , fullName = "Ursaring"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 217 "Ursaring"
      , evolutionData = EvolvesFrom [ 340 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 1611
      , nationalDexNumber = 901
      , originalPokemonID = Nothing
      , fullName = "Ursaluna"
      , typing = Double Normal Ground
      , ability = Nothing
      , imageUrl = imageUrl 901 "Ursaluna"
      , evolutionData = EvolvesFrom [ 341 ] "Use Peat Block under a full moon"
      , transformationData = DoesNotTransform
      }
    , { id = 342
      , nationalDexNumber = 218
      , originalPokemonID = Nothing
      , fullName = "Slugma"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrl 218 "Slugma"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 343
      , nationalDexNumber = 219
      , originalPokemonID = Nothing
      , fullName = "Magcargo"
      , typing = Double Fire Rock
      , ability = Nothing
      , imageUrl = imageUrl 219 "Magcargo"
      , evolutionData = EvolvesFrom [ 342 ] "Level 38"
      , transformationData = DoesNotTransform
      }
    , { id = 344
      , nationalDexNumber = 220
      , originalPokemonID = Nothing
      , fullName = "Swinub"
      , typing = Double Ice Ground
      , ability = Nothing
      , imageUrl = imageUrl 220 "Swinub"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 345
      , nationalDexNumber = 221
      , originalPokemonID = Nothing
      , fullName = "Piloswine"
      , typing = Double Ice Ground
      , ability = Nothing
      , imageUrl = imageUrl 221 "Piloswine"
      , evolutionData = EvolvesFrom [ 344 ] "Level 33"
      , transformationData = DoesNotTransform
      }
    , { id = 718
      , nationalDexNumber = 473
      , originalPokemonID = Nothing
      , fullName = "Mamoswine"
      , typing = Double Ice Ground
      , ability = Nothing
      , imageUrl = imageUrl 473 "Mamoswine"
      , evolutionData = EvolvesFrom [ 345 ] "Level while knowing Ancient Power"
      , transformationData = DoesNotTransform
      }
    , { id = 346
      , nationalDexNumber = 222
      , originalPokemonID = Nothing
      , fullName = "Corsola"
      , typing = Double Water Rock
      , ability = Nothing
      , imageUrl = imageUrl 222 "Corsola"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1389
      , nationalDexNumber = 222
      , originalPokemonID = Just 346
      , fullName = nameWithForm "Corsola" Galarian
      , typing = Single Ghost
      , ability = Nothing
      , imageUrl = imageUrlWithForm 222 "Corsola" Galarian
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1390
      , nationalDexNumber = 864
      , originalPokemonID = Nothing
      , fullName = "Cursola"
      , typing = Single Ghost
      , ability = Nothing
      , imageUrl = imageUrl 864 "Cursola"
      , evolutionData = EvolvesFrom [ 1389 ] "Level 38"
      , transformationData = DoesNotTransform
      }
    , { id = 347
      , nationalDexNumber = 223
      , originalPokemonID = Nothing
      , fullName = "Remoraid"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 223 "Remoraid"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 348
      , nationalDexNumber = 224
      , originalPokemonID = Nothing
      , fullName = "Octillery"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 224 "Octillery"
      , evolutionData = EvolvesFrom [ 347 ] "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 349
      , nationalDexNumber = 225
      , originalPokemonID = Nothing
      , fullName = "Delibird"
      , typing = Double Ice Flying
      , ability = Nothing
      , imageUrl = imageUrl 225 "Delibird"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 656
      , nationalDexNumber = 458
      , originalPokemonID = Nothing
      , fullName = "Mantyke"
      , typing = Double Water Flying
      , ability = Just WaterAbsorb
      , imageUrl = imageUrl 458 "Mantyke"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 350
      , nationalDexNumber = 226
      , originalPokemonID = Nothing
      , fullName = "Mantine"
      , typing = Double Water Flying
      , ability = Just WaterAbsorb
      , imageUrl = imageUrl 226 "Mantine"
      , evolutionData = EvolvesFrom [ 656 ] "Level with Remoraid in party"
      , transformationData = DoesNotTransform
      }
    , { id = 351
      , nationalDexNumber = 227
      , originalPokemonID = Nothing
      , fullName = "Skarmory"
      , typing = Double Steel Flying
      , ability = Nothing
      , imageUrl = imageUrl 227 "Skarmory"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 352
      , nationalDexNumber = 228
      , originalPokemonID = Nothing
      , fullName = "Houndour"
      , typing = Double Dark Fire
      , ability = Just FlashFire
      , imageUrl = imageUrl 228 "Houndour"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 353
      , nationalDexNumber = 229
      , originalPokemonID = Nothing
      , fullName = "Houndoom"
      , typing = Double Dark Fire
      , ability = Just FlashFire
      , imageUrl = imageUrl 229 "Houndoom"
      , evolutionData = EvolvesFrom [ 352 ] "Level 24"
      , transformationData = DoesNotTransform
      }
    , { id = 1396
      , nationalDexNumber = 229
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Houndoom" Mega
      , typing = Double Dark Fire
      , ability = Just FlashFire
      , imageUrl = imageUrlWithForm 229 "Houndoom" Mega
      , evolutionData = EvolvesFrom [ 353 ] "Holding Houndoominite"
      , transformationData = DoesNotTransform
      }
    , { id = 355
      , nationalDexNumber = 231
      , originalPokemonID = Nothing
      , fullName = "Phanpy"
      , typing = Single Ground
      , ability = Nothing
      , imageUrl = imageUrl 231 "Phanpy"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 356
      , nationalDexNumber = 232
      , originalPokemonID = Nothing
      , fullName = "Donphan"
      , typing = Single Ground
      , ability = Nothing
      , imageUrl = imageUrl 232 "Donphan"
      , evolutionData = EvolvesFrom [ 355 ] "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 358
      , nationalDexNumber = 234
      , originalPokemonID = Nothing
      , fullName = "Stantler"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 234 "Stantler"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1609
      , nationalDexNumber = 899
      , originalPokemonID = Nothing
      , fullName = "Wyrdeer"
      , typing = Double Normal Psychic
      , ability = Nothing
      , imageUrl = imageUrl 899 "Wyrdeer"
      , evolutionData = EvolvesFrom [ 358 ] "Use Psyshield Bash 20 times in Agile Style"
      , transformationData = DoesNotTransform
      }
    , { id = 359
      , nationalDexNumber = 235
      , originalPokemonID = Nothing
      , fullName = "Smeargle"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 235 "Smeargle"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 365
      , nationalDexNumber = 241
      , originalPokemonID = Nothing
      , fullName = "Miltank"
      , typing = Single Normal
      , ability = Just ThickFat
      , imageUrl = imageUrl 241 "Miltank"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 367
      , nationalDexNumber = 243
      , originalPokemonID = Nothing
      , fullName = "Raikou"
      , typing = Single Electric
      , ability = Nothing
      , imageUrl = imageUrl 243 "Raikou"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 368
      , nationalDexNumber = 244
      , originalPokemonID = Nothing
      , fullName = "Entei"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrl 244 "Entei"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 369
      , nationalDexNumber = 245
      , originalPokemonID = Nothing
      , fullName = "Suicune"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 245 "Suicune"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 370
      , nationalDexNumber = 246
      , originalPokemonID = Nothing
      , fullName = "Larvitar"
      , typing = Double Rock Ground
      , ability = Nothing
      , imageUrl = imageUrl 246 "Larvitar"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 371
      , nationalDexNumber = 247
      , originalPokemonID = Nothing
      , fullName = "Pupitar"
      , typing = Double Rock Ground
      , ability = Nothing
      , imageUrl = imageUrl 247 "Pupitar"
      , evolutionData = EvolvesFrom [ 370 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 372
      , nationalDexNumber = 248
      , originalPokemonID = Nothing
      , fullName = "Tyranitar"
      , typing = Double Rock Dark
      , ability = Nothing
      , imageUrl = imageUrl 248 "Tyranitar"
      , evolutionData = EvolvesFrom [ 371 ] "Level 55"
      , transformationData = DoesNotTransform
      }
    , { id = 1397
      , nationalDexNumber = 248
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Tyranitar" Mega
      , typing = Double Rock Dark
      , ability = Nothing
      , imageUrl = imageUrlWithForm 248 "Tyranitar" Mega
      , evolutionData = EvolvesFrom [ 372 ] "Holding Tyranitarite"
      , transformationData = DoesNotTransform
      }
    , { id = 373
      , nationalDexNumber = 249
      , originalPokemonID = Nothing
      , fullName = "Lugia"
      , typing = Double Psychic Flying
      , ability = Nothing
      , imageUrl = imageUrl 249 "Lugia"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 374
      , nationalDexNumber = 250
      , originalPokemonID = Nothing
      , fullName = "Ho-Oh"
      , typing = Double Fire Flying
      , ability = Nothing
      , imageUrl = imageUrl 250 "Ho-Oh"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 375
      , nationalDexNumber = 251
      , originalPokemonID = Nothing
      , fullName = "Celebi"
      , typing = Double Psychic Grass
      , ability = Nothing
      , imageUrl = imageUrl 251 "Celebi"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 380
      , nationalDexNumber = 252
      , originalPokemonID = Nothing
      , fullName = "Treecko"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrl 252 "Treecko"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 381
      , nationalDexNumber = 253
      , originalPokemonID = Nothing
      , fullName = "Grovyle"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrl 253 "Grovyle"
      , evolutionData = EvolvesFrom [ 380 ] "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 382
      , nationalDexNumber = 254
      , originalPokemonID = Nothing
      , fullName = "Sceptile"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrl 254 "Sceptile"
      , evolutionData = EvolvesFrom [ 381 ] "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 1428
      , nationalDexNumber = 254
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Sceptile" Mega
      , typing = Double Grass Dragon
      , ability = Just LightningRod
      , imageUrl = imageUrlWithForm 254 "Sceptile" Mega
      , evolutionData = EvolvesFrom [ 382 ] "Holding Sceptilite"
      , transformationData = DoesNotTransform
      }
    , { id = 383
      , nationalDexNumber = 255
      , originalPokemonID = Nothing
      , fullName = "Torchic"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrl 255 "Torchic"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 384
      , nationalDexNumber = 256
      , originalPokemonID = Nothing
      , fullName = "Combusken"
      , typing = Double Fire Fighting
      , ability = Nothing
      , imageUrl = imageUrl 256 "Combusken"
      , evolutionData = EvolvesFrom [ 383 ] "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 385
      , nationalDexNumber = 257
      , originalPokemonID = Nothing
      , fullName = "Blaziken"
      , typing = Double Fire Fighting
      , ability = Nothing
      , imageUrl = imageUrl 257 "Blaziken"
      , evolutionData = EvolvesFrom [ 384 ] "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 1429
      , nationalDexNumber = 257
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Blaziken" Mega
      , typing = Double Fire Fighting
      , ability = Nothing
      , imageUrl = imageUrlWithForm 257 "Blaziken" Mega
      , evolutionData = EvolvesFrom [ 385 ] "Holding Blazikenite"
      , transformationData = DoesNotTransform
      }
    , { id = 386
      , nationalDexNumber = 258
      , originalPokemonID = Nothing
      , fullName = "Mudkip"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 258 "Mudkip"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 387
      , nationalDexNumber = 259
      , originalPokemonID = Nothing
      , fullName = "Marshtomp"
      , typing = Double Water Ground
      , ability = Nothing
      , imageUrl = imageUrl 259 "Marshtomp"
      , evolutionData = EvolvesFrom [ 386 ] "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 388
      , nationalDexNumber = 260
      , originalPokemonID = Nothing
      , fullName = "Swampert"
      , typing = Double Water Ground
      , ability = Nothing
      , imageUrl = imageUrl 260 "Swampert"
      , evolutionData = EvolvesFrom [ 387 ] "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 1430
      , nationalDexNumber = 260
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Swampert" Mega
      , typing = Double Water Ground
      , ability = Nothing
      , imageUrl = imageUrlWithForm 260 "Swampert" Mega
      , evolutionData = EvolvesFrom [ 388 ] "Holding Swampertite"
      , transformationData = DoesNotTransform
      }
    , { id = 389
      , nationalDexNumber = 261
      , originalPokemonID = Nothing
      , fullName = "Poochyena"
      , typing = Single Dark
      , ability = Nothing
      , imageUrl = imageUrl 261 "Poochyena"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 390
      , nationalDexNumber = 262
      , originalPokemonID = Nothing
      , fullName = "Mightyena"
      , typing = Single Dark
      , ability = Nothing
      , imageUrl = imageUrl 262 "Mightyena"
      , evolutionData = EvolvesFrom [ 389 ] "Level 18"
      , transformationData = DoesNotTransform
      }
    , { id = 391
      , nationalDexNumber = 263
      , originalPokemonID = Nothing
      , fullName = "Zigzagoon"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 263 "Zigzagoon"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 392
      , nationalDexNumber = 264
      , originalPokemonID = Nothing
      , fullName = "Linoone"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 264 "Linoone"
      , evolutionData = EvolvesFrom [ 391 ] "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 1466
      , nationalDexNumber = 263
      , originalPokemonID = Just 391
      , fullName = nameWithForm "Zigzagoon" Galarian
      , typing = Double Dark Normal
      , ability = Nothing
      , imageUrl = imageUrlWithForm 263 "Zigzagoon" Galarian
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1467
      , nationalDexNumber = 264
      , originalPokemonID = Just 392
      , fullName = nameWithForm "Linoone" Galarian
      , typing = Double Dark Normal
      , ability = Nothing
      , imageUrl = imageUrlWithForm 264 "Linoone" Galarian
      , evolutionData = EvolvesFrom [ 1466 ] "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 1468
      , nationalDexNumber = 862
      , originalPokemonID = Nothing
      , fullName = "Obstagoon"
      , typing = Double Dark Normal
      , ability = Nothing
      , imageUrl = imageUrl 862 "Obstagoon"
      , evolutionData = EvolvesFrom [ 1467 ] "Level 35 at night"
      , transformationData = DoesNotTransform
      }
    , { id = 393
      , nationalDexNumber = 265
      , originalPokemonID = Nothing
      , fullName = "Wurmple"
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imageUrl 265 "Wurmple"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 394
      , nationalDexNumber = 266
      , originalPokemonID = Nothing
      , fullName = "Silcoon"
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imageUrl 266 "Silcoon"
      , evolutionData = EvolvesFrom [ 393 ] "Level 7 (random)"
      , transformationData = DoesNotTransform
      }
    , { id = 395
      , nationalDexNumber = 267
      , originalPokemonID = Nothing
      , fullName = "Beautifly"
      , typing = Double Bug Flying
      , ability = Nothing
      , imageUrl = imageUrl 267 "Beautifly"
      , evolutionData = EvolvesFrom [ 394 ] "Level 10"
      , transformationData = DoesNotTransform
      }
    , { id = 396
      , nationalDexNumber = 268
      , originalPokemonID = Nothing
      , fullName = "Cascoon"
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imageUrl 268 "Cascoon"
      , evolutionData = EvolvesFrom [ 393 ] "Level 7 (random)"
      , transformationData = DoesNotTransform
      }
    , { id = 397
      , nationalDexNumber = 269
      , originalPokemonID = Nothing
      , fullName = "Dustox"
      , typing = Double Bug Poison
      , ability = Nothing
      , imageUrl = imageUrl 269 "Dustox"
      , evolutionData = EvolvesFrom [ 396 ] "Level 10"
      , transformationData = DoesNotTransform
      }
    , { id = 398
      , nationalDexNumber = 270
      , originalPokemonID = Nothing
      , fullName = "Lotad"
      , typing = Double Water Grass
      , ability = Nothing
      , imageUrl = imageUrl 270 "Lotad"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 399
      , nationalDexNumber = 271
      , originalPokemonID = Nothing
      , fullName = "Lombre"
      , typing = Double Water Grass
      , ability = Nothing
      , imageUrl = imageUrl 271 "Lombre"
      , evolutionData = EvolvesFrom [ 398 ] "Level 14"
      , transformationData = DoesNotTransform
      }
    , { id = 400
      , nationalDexNumber = 272
      , originalPokemonID = Nothing
      , fullName = "Ludicolo"
      , typing = Double Water Grass
      , ability = Nothing
      , imageUrl = imageUrl 272 "Ludicolo"
      , evolutionData = EvolvesFrom [ 399 ] "Use Water Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 401
      , nationalDexNumber = 273
      , originalPokemonID = Nothing
      , fullName = "Seedot"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrl 273 "Seedot"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 402
      , nationalDexNumber = 274
      , originalPokemonID = Nothing
      , fullName = "Nuzleaf"
      , typing = Double Grass Dark
      , ability = Nothing
      , imageUrl = imageUrl 274 "Nuzleaf"
      , evolutionData = EvolvesFrom [ 401 ] "Level 14"
      , transformationData = DoesNotTransform
      }
    , { id = 403
      , nationalDexNumber = 275
      , originalPokemonID = Nothing
      , fullName = "Shiftry"
      , typing = Double Grass Dark
      , ability = Nothing
      , imageUrl = imageUrl 275 "Shiftry"
      , evolutionData = EvolvesFrom [ 402 ] "Use Leaf Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 404
      , nationalDexNumber = 276
      , originalPokemonID = Nothing
      , fullName = "Taillow"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrl 276 "Taillow"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 405
      , nationalDexNumber = 277
      , originalPokemonID = Nothing
      , fullName = "Swellow"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrl 277 "Swellow"
      , evolutionData = EvolvesFrom [ 404 ] "Level 22"
      , transformationData = DoesNotTransform
      }
    , { id = 406
      , nationalDexNumber = 278
      , originalPokemonID = Nothing
      , fullName = "Wingull"
      , typing = Double Water Flying
      , ability = Nothing
      , imageUrl = imageUrl 278 "Wingull"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 407
      , nationalDexNumber = 279
      , originalPokemonID = Nothing
      , fullName = "Pelipper"
      , typing = Double Water Flying
      , ability = Nothing
      , imageUrl = imageUrl 279 "Pelipper"
      , evolutionData = EvolvesFrom [ 406 ] "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 408
      , nationalDexNumber = 280
      , originalPokemonID = Nothing
      , fullName = "Ralts"
      , typing = Double Psychic Fairy
      , ability = Nothing
      , imageUrl = imageUrl 280 "Ralts"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 409
      , nationalDexNumber = 281
      , originalPokemonID = Nothing
      , fullName = "Kirlia"
      , typing = Double Psychic Fairy
      , ability = Nothing
      , imageUrl = imageUrl 281 "Kirlia"
      , evolutionData = EvolvesFrom [ 408 ] "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 410
      , nationalDexNumber = 282
      , originalPokemonID = Nothing
      , fullName = "Gardevoir"
      , typing = Double Psychic Fairy
      , ability = Nothing
      , imageUrl = imageUrl 282 "Gardevoir"
      , evolutionData = EvolvesFrom [ 409 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 1435
      , nationalDexNumber = 282
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Gardevoir" Mega
      , typing = Double Psychic Fairy
      , ability = Nothing
      , imageUrl = imageUrlWithForm 282 "Gardevoir" Mega
      , evolutionData = EvolvesFrom [ 410 ] "Holding Gardevoirite"
      , transformationData = DoesNotTransform
      }
    , { id = 720
      , nationalDexNumber = 475
      , originalPokemonID = Nothing
      , fullName = "Gallade"
      , typing = Double Psychic Fighting
      , ability = Nothing
      , imageUrl = imageUrl 475 "Gallade"
      , evolutionData = EvolvesFrom [ 409 ] "Use Dawn Stone when male"
      , transformationData = DoesNotTransform
      }
    , { id = 1436
      , nationalDexNumber = 475
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Gallade" Mega
      , typing = Double Psychic Fighting
      , ability = Nothing
      , imageUrl = imageUrlWithForm 475 "Gallade" Mega
      , evolutionData = EvolvesFrom [ 720 ] "Holding Galladite"
      , transformationData = DoesNotTransform
      }
    , { id = 411
      , nationalDexNumber = 283
      , originalPokemonID = Nothing
      , fullName = "Surskit"
      , typing = Double Bug Water
      , ability = Nothing
      , imageUrl = imageUrl 283 "Surskit"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 412
      , nationalDexNumber = 284
      , originalPokemonID = Nothing
      , fullName = "Masquerain"
      , typing = Double Bug Flying
      , ability = Nothing
      , imageUrl = imageUrl 284 "Masquerain"
      , evolutionData = EvolvesFrom [ 411 ] "Level 22"
      , transformationData = DoesNotTransform
      }
    , { id = 413
      , nationalDexNumber = 285
      , originalPokemonID = Nothing
      , fullName = "Shroomish"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrl 285 "Shroomish"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 414
      , nationalDexNumber = 286
      , originalPokemonID = Nothing
      , fullName = "Breloom"
      , typing = Double Grass Fighting
      , ability = Nothing
      , imageUrl = imageUrl 286 "Breloom"
      , evolutionData = EvolvesFrom [ 413 ] "Level 23"
      , transformationData = DoesNotTransform
      }
    , { id = 415
      , nationalDexNumber = 287
      , originalPokemonID = Nothing
      , fullName = "Slakoth"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 287 "Slakoth"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 416
      , nationalDexNumber = 288
      , originalPokemonID = Nothing
      , fullName = "Vigoroth"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 288 "Vigoroth"
      , evolutionData = EvolvesFrom [ 415 ] "Level 18"
      , transformationData = DoesNotTransform
      }
    , { id = 417
      , nationalDexNumber = 289
      , originalPokemonID = Nothing
      , fullName = "Slaking"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 289 "Slaking"
      , evolutionData = EvolvesFrom [ 416 ] "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 418
      , nationalDexNumber = 290
      , originalPokemonID = Nothing
      , fullName = "Nincada"
      , typing = Double Bug Ground
      , ability = Nothing
      , imageUrl = imageUrl 290 "Nincada"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 419
      , nationalDexNumber = 291
      , originalPokemonID = Nothing
      , fullName = "Ninjask"
      , typing = Double Bug Flying
      , ability = Nothing
      , imageUrl = imageUrl 291 "Ninjask"
      , evolutionData = EvolvesFrom [ 418 ] "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 420
      , nationalDexNumber = 292
      , originalPokemonID = Nothing
      , fullName = "Shedinja"
      , typing = Double Bug Ghost
      , ability = Just WonderGuard
      , imageUrl = imageUrl 292 "Shedinja"
      , evolutionData = EvolvesFrom [ 418 ] "Evolve while having a Pokeball and an empty space in party"
      , transformationData = DoesNotTransform
      }
    , { id = 421
      , nationalDexNumber = 293
      , originalPokemonID = Nothing
      , fullName = "Whismur"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 293 "Whismur"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 422
      , nationalDexNumber = 294
      , originalPokemonID = Nothing
      , fullName = "Loudred"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 294 "Loudred"
      , evolutionData = EvolvesFrom [ 421 ] "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 423
      , nationalDexNumber = 295
      , originalPokemonID = Nothing
      , fullName = "Exploud"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 295 "Exploud"
      , evolutionData = EvolvesFrom [ 422 ] "Level 40"
      , transformationData = DoesNotTransform
      }
    , { id = 424
      , nationalDexNumber = 296
      , originalPokemonID = Nothing
      , fullName = "Makuhita"
      , typing = Single Fighting
      , ability = Just ThickFat
      , imageUrl = imageUrl 296 "Makuhita"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 425
      , nationalDexNumber = 297
      , originalPokemonID = Nothing
      , fullName = "Hariyama"
      , typing = Single Fighting
      , ability = Just ThickFat
      , imageUrl = imageUrl 297 "Hariyama"
      , evolutionData = EvolvesFrom [ 424 ] "Level 24"
      , transformationData = DoesNotTransform
      }
    , { id = 427
      , nationalDexNumber = 299
      , originalPokemonID = Nothing
      , fullName = "Nosepass"
      , typing = Single Rock
      , ability = Nothing
      , imageUrl = imageUrl 299 "Nosepass"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 726
      , nationalDexNumber = 476
      , originalPokemonID = Nothing
      , fullName = "Probopass"
      , typing = Double Rock Steel
      , ability = Nothing
      , imageUrl = imageUrl 476 "Probopass"
      , evolutionData = EvolvesFrom [ 427 ] "Level near a Special Magnetic Field"
      , transformationData = DoesNotTransform
      }
    , { id = 428
      , nationalDexNumber = 300
      , originalPokemonID = Nothing
      , fullName = "Skitty"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 300 "Skitty"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 429
      , nationalDexNumber = 301
      , originalPokemonID = Nothing
      , fullName = "Delcatty"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 301 "Delcatty"
      , evolutionData = EvolvesFrom [ 428 ] "Use Moon Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 430
      , nationalDexNumber = 302
      , originalPokemonID = Nothing
      , fullName = "Sableye"
      , typing = Double Dark Ghost
      , ability = Nothing
      , imageUrl = imageUrl 302 "Sableye"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1558
      , nationalDexNumber = 302
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Sableye" Mega
      , typing = Double Dark Ghost
      , ability = Nothing
      , imageUrl = imageUrlWithForm 302 "Sableye" Mega
      , evolutionData = EvolvesFrom [ 430 ] "Holding Sablenite"
      , transformationData = DoesNotTransform
      }
    , { id = 431
      , nationalDexNumber = 303
      , originalPokemonID = Nothing
      , fullName = "Mawile"
      , typing = Double Steel Fairy
      , ability = Nothing
      , imageUrl = imageUrl 303 "Mawile"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1559
      , nationalDexNumber = 303
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Mawile" Mega
      , typing = Double Steel Fairy
      , ability = Nothing
      , imageUrl = imageUrlWithForm 303 "Mawile" Mega
      , evolutionData = EvolvesFrom [ 431 ] "Holding Mawilite"
      , transformationData = DoesNotTransform
      }
    , { id = 432
      , nationalDexNumber = 304
      , originalPokemonID = Nothing
      , fullName = "Aron"
      , typing = Double Steel Rock
      , ability = Nothing
      , imageUrl = imageUrl 304 "Aron"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 433
      , nationalDexNumber = 305
      , originalPokemonID = Nothing
      , fullName = "Lairon"
      , typing = Double Steel Rock
      , ability = Nothing
      , imageUrl = imageUrl 305 "Lairon"
      , evolutionData = EvolvesFrom [ 432 ] "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 434
      , nationalDexNumber = 306
      , originalPokemonID = Nothing
      , fullName = "Aggron"
      , typing = Double Steel Rock
      , ability = Nothing
      , imageUrl = imageUrl 306 "Aggron"
      , evolutionData = EvolvesFrom [ 433 ] "Level 42"
      , transformationData = DoesNotTransform
      }
    , { id = 1434
      , nationalDexNumber = 306
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Aggron" Mega
      , typing = Single Steel
      , ability = Nothing
      , imageUrl = imageUrlWithForm 306 "Aggron" Mega
      , evolutionData = EvolvesFrom [ 434 ] "Holding Aggronite"
      , transformationData = DoesNotTransform
      }
    , { id = 435
      , nationalDexNumber = 307
      , originalPokemonID = Nothing
      , fullName = "Meditite"
      , typing = Double Fighting Psychic
      , ability = Nothing
      , imageUrl = imageUrl 307 "Meditite"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 436
      , nationalDexNumber = 308
      , originalPokemonID = Nothing
      , fullName = "Medicham"
      , typing = Double Fighting Psychic
      , ability = Nothing
      , imageUrl = imageUrl 308 "Medicham"
      , evolutionData = EvolvesFrom [ 435 ] "Level 37"
      , transformationData = DoesNotTransform
      }
    , { id = 1590
      , nationalDexNumber = 308
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Medicham" Mega
      , typing = Double Fighting Psychic
      , ability = Nothing
      , imageUrl = imageUrlWithForm 308 "Medicham" Mega
      , evolutionData = EvolvesFrom [ 436 ] "Holding Medichamite"
      , transformationData = DoesNotTransform
      }
    , { id = 437
      , nationalDexNumber = 309
      , originalPokemonID = Nothing
      , fullName = "Electrike"
      , typing = Single Electric
      , ability = Just LightningRod
      , imageUrl = imageUrl 309 "Electrike"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 438
      , nationalDexNumber = 310
      , originalPokemonID = Nothing
      , fullName = "Manectric"
      , typing = Single Electric
      , ability = Just LightningRod
      , imageUrl = imageUrl 310 "Manectric"
      , evolutionData = EvolvesFrom [ 437 ] "Level 26"
      , transformationData = DoesNotTransform
      }
    , { id = 1437
      , nationalDexNumber = 310
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Manectric" Mega
      , typing = Single Electric
      , ability = Nothing
      , imageUrl = imageUrlWithForm 310 "Manectric" Mega
      , evolutionData = EvolvesFrom [ 438 ] "Holding Manectite"
      , transformationData = DoesNotTransform
      }
    , { id = 439
      , nationalDexNumber = 311
      , originalPokemonID = Nothing
      , fullName = "Plusle"
      , typing = Single Electric
      , ability = Nothing
      , imageUrl = imageUrl 311 "Plusle"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 440
      , nationalDexNumber = 312
      , originalPokemonID = Nothing
      , fullName = "Minun"
      , typing = Single Electric
      , ability = Just VoltAbsorb
      , imageUrl = imageUrl 312 "Minun"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 441
      , nationalDexNumber = 313
      , originalPokemonID = Nothing
      , fullName = "Volbeat"
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imageUrl 313 "Volbeat"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 442
      , nationalDexNumber = 314
      , originalPokemonID = Nothing
      , fullName = "Illumise"
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imageUrl 314 "Illumise"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 604
      , nationalDexNumber = 406
      , originalPokemonID = Nothing
      , fullName = "Budew"
      , typing = Double Grass Poison
      , ability = Nothing
      , imageUrl = imageUrl 406 "Budew"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 443
      , nationalDexNumber = 315
      , originalPokemonID = Nothing
      , fullName = "Roselia"
      , typing = Double Grass Poison
      , ability = Nothing
      , imageUrl = imageUrl 315 "Roselia"
      , evolutionData = EvolvesFrom [ 604 ] "Level during the day with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 605
      , nationalDexNumber = 407
      , originalPokemonID = Nothing
      , fullName = "Roserade"
      , typing = Double Grass Poison
      , ability = Nothing
      , imageUrl = imageUrl 407 "Roserade"
      , evolutionData = EvolvesFrom [ 443 ] "Use Shiny Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 444
      , nationalDexNumber = 316
      , originalPokemonID = Nothing
      , fullName = "Gulpin"
      , typing = Single Poison
      , ability = Nothing
      , imageUrl = imageUrl 316 "Gulpin"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 445
      , nationalDexNumber = 317
      , originalPokemonID = Nothing
      , fullName = "Swalot"
      , typing = Single Poison
      , ability = Nothing
      , imageUrl = imageUrl 317 "Swalot"
      , evolutionData = EvolvesFrom [ 444 ] "Level 26"
      , transformationData = DoesNotTransform
      }
    , { id = 446
      , nationalDexNumber = 318
      , originalPokemonID = Nothing
      , fullName = "Carvanha"
      , typing = Double Water Dark
      , ability = Nothing
      , imageUrl = imageUrl 318 "Carvanha"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 447
      , nationalDexNumber = 319
      , originalPokemonID = Nothing
      , fullName = "Sharpedo"
      , typing = Double Water Dark
      , ability = Nothing
      , imageUrl = imageUrl 319 "Sharpedo"
      , evolutionData = EvolvesFrom [ 446 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 1587
      , nationalDexNumber = 319
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Sharpedo" Mega
      , typing = Double Water Dark
      , ability = Nothing
      , imageUrl = imageUrlWithForm 319 "Sharpedo" Mega
      , evolutionData = EvolvesFrom [ 447 ] "Holding Sharpedonite"
      , transformationData = DoesNotTransform
      }
    , { id = 448
      , nationalDexNumber = 320
      , originalPokemonID = Nothing
      , fullName = "Wailmer"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 320 "Wailmer"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 449
      , nationalDexNumber = 321
      , originalPokemonID = Nothing
      , fullName = "Wailord"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 321 "Wailord"
      , evolutionData = EvolvesFrom [ 448 ] "Level 40"
      , transformationData = DoesNotTransform
      }
    , { id = 450
      , nationalDexNumber = 322
      , originalPokemonID = Nothing
      , fullName = "Numel"
      , typing = Double Fire Ground
      , ability = Nothing
      , imageUrl = imageUrl 322 "Numel"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 451
      , nationalDexNumber = 323
      , originalPokemonID = Nothing
      , fullName = "Camerupt"
      , typing = Double Fire Ground
      , ability = Nothing
      , imageUrl = imageUrl 323 "Camerupt"
      , evolutionData = EvolvesFrom [ 450 ] "Level 33"
      , transformationData = DoesNotTransform
      }
    , { id = 1588
      , nationalDexNumber = 323
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Camerupt" Mega
      , typing = Double Fire Ground
      , ability = Nothing
      , imageUrl = imageUrlWithForm 323 "Camerupt" Mega
      , evolutionData = EvolvesFrom [ 451 ] "Holding Cameruptite"
      , transformationData = DoesNotTransform
      }
    , { id = 452
      , nationalDexNumber = 324
      , originalPokemonID = Nothing
      , fullName = "Torkoal"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrl 324 "Torkoal"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 453
      , nationalDexNumber = 325
      , originalPokemonID = Nothing
      , fullName = "Spoink"
      , typing = Single Psychic
      , ability = Just ThickFat
      , imageUrl = imageUrl 325 "Spoink"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 454
      , nationalDexNumber = 326
      , originalPokemonID = Nothing
      , fullName = "Grumpig"
      , typing = Single Psychic
      , ability = Just ThickFat
      , imageUrl = imageUrl 326 "Grumpig"
      , evolutionData = EvolvesFrom [ 453 ] "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 455
      , nationalDexNumber = 327
      , originalPokemonID = Nothing
      , fullName = "Spinda"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 327 "Spinda"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 456
      , nationalDexNumber = 328
      , originalPokemonID = Nothing
      , fullName = "Trapinch"
      , typing = Single Ground
      , ability = Nothing
      , imageUrl = imageUrl 328 "Trapinch"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 457
      , nationalDexNumber = 329
      , originalPokemonID = Nothing
      , fullName = "Vibrava"
      , typing = Double Ground Dragon
      , ability = Just Levitate
      , imageUrl = imageUrl 329 "Vibrava"
      , evolutionData = EvolvesFrom [ 456 ] "Level 35"
      , transformationData = DoesNotTransform
      }
    , { id = 458
      , nationalDexNumber = 330
      , originalPokemonID = Nothing
      , fullName = "Flygon"
      , typing = Double Ground Dragon
      , ability = Just Levitate
      , imageUrl = imageUrl 330 "Flygon"
      , evolutionData = EvolvesFrom [ 457 ] "Level 45"
      , transformationData = DoesNotTransform
      }
    , { id = 459
      , nationalDexNumber = 331
      , originalPokemonID = Nothing
      , fullName = "Cacnea"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrl 331 "Cacnea"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 460
      , nationalDexNumber = 332
      , originalPokemonID = Nothing
      , fullName = "Cacturne"
      , typing = Double Grass Dark
      , ability = Nothing
      , imageUrl = imageUrl 332 "Cacturne"
      , evolutionData = EvolvesFrom [ 459 ] "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 461
      , nationalDexNumber = 333
      , originalPokemonID = Nothing
      , fullName = "Swablu"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrl 333 "Swablu"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 462
      , nationalDexNumber = 334
      , originalPokemonID = Nothing
      , fullName = "Altaria"
      , typing = Double Dragon Flying
      , ability = Nothing
      , imageUrl = imageUrl 334 "Altaria"
      , evolutionData = EvolvesFrom [ 461 ] "Level 35"
      , transformationData = DoesNotTransform
      }
    , { id = 1589
      , nationalDexNumber = 334
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Altaria" Mega
      , typing = Double Dragon Fairy
      , ability = Nothing
      , imageUrl = imageUrlWithForm 334 "Altaria" Mega
      , evolutionData = EvolvesFrom [ 462 ] "Holding Altarianite"
      , transformationData = DoesNotTransform
      }
    , { id = 463
      , nationalDexNumber = 335
      , originalPokemonID = Nothing
      , fullName = "Zangoose"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 335 "Zangoose"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 464
      , nationalDexNumber = 336
      , originalPokemonID = Nothing
      , fullName = "Seviper"
      , typing = Single Poison
      , ability = Nothing
      , imageUrl = imageUrl 336 "Seviper"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 465
      , nationalDexNumber = 337
      , originalPokemonID = Nothing
      , fullName = "Lunatone"
      , typing = Double Rock Psychic
      , ability = Just Levitate
      , imageUrl = imageUrl 337 "Lunatone"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 466
      , nationalDexNumber = 338
      , originalPokemonID = Nothing
      , fullName = "Solrock"
      , typing = Double Rock Psychic
      , ability = Just Levitate
      , imageUrl = imageUrl 338 "Solrock"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 467
      , nationalDexNumber = 339
      , originalPokemonID = Nothing
      , fullName = "Barboach"
      , typing = Double Water Ground
      , ability = Nothing
      , imageUrl = imageUrl 339 "Barboach"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 468
      , nationalDexNumber = 340
      , originalPokemonID = Nothing
      , fullName = "Whiscash"
      , typing = Double Water Ground
      , ability = Nothing
      , imageUrl = imageUrl 340 "Whiscash"
      , evolutionData = EvolvesFrom [ 467 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 469
      , nationalDexNumber = 341
      , originalPokemonID = Nothing
      , fullName = "Corphish"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 341 "Corphish"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 470
      , nationalDexNumber = 342
      , originalPokemonID = Nothing
      , fullName = "Crawdaunt"
      , typing = Double Water Dark
      , ability = Nothing
      , imageUrl = imageUrl 342 "Crawdaunt"
      , evolutionData = EvolvesFrom [ 469 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 471
      , nationalDexNumber = 343
      , originalPokemonID = Nothing
      , fullName = "Baltoy"
      , typing = Double Ground Psychic
      , ability = Just Levitate
      , imageUrl = imageUrl 343 "Baltoy"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 472
      , nationalDexNumber = 344
      , originalPokemonID = Nothing
      , fullName = "Claydol"
      , typing = Double Ground Psychic
      , ability = Just Levitate
      , imageUrl = imageUrl 344 "Claydol"
      , evolutionData = EvolvesFrom [ 471 ] "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 473
      , nationalDexNumber = 345
      , originalPokemonID = Nothing
      , fullName = "Lileep"
      , typing = Double Rock Grass
      , ability = Nothing
      , imageUrl = imageUrl 345 "Lileep"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 474
      , nationalDexNumber = 346
      , originalPokemonID = Nothing
      , fullName = "Cradily"
      , typing = Double Rock Grass
      , ability = Nothing
      , imageUrl = imageUrl 346 "Cradily"
      , evolutionData = EvolvesFrom [ 473 ] "Level 40"
      , transformationData = DoesNotTransform
      }
    , { id = 475
      , nationalDexNumber = 347
      , originalPokemonID = Nothing
      , fullName = "Anorith"
      , typing = Double Rock Bug
      , ability = Nothing
      , imageUrl = imageUrl 347 "Anorith"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 476
      , nationalDexNumber = 348
      , originalPokemonID = Nothing
      , fullName = "Armaldo"
      , typing = Double Rock Bug
      , ability = Nothing
      , imageUrl = imageUrl 348 "Armaldo"
      , evolutionData = EvolvesFrom [ 475 ] "Level 40"
      , transformationData = DoesNotTransform
      }
    , { id = 477
      , nationalDexNumber = 349
      , originalPokemonID = Nothing
      , fullName = "Feebas"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 349 "Feebas"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 478
      , nationalDexNumber = 350
      , originalPokemonID = Nothing
      , fullName = "Milotic"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 350 "Milotic"
      , evolutionData = EvolvesFrom [ 477 ] "Trade holding Prism Scale"
      , transformationData = DoesNotTransform
      }
    , { id = 479
      , nationalDexNumber = 351
      , originalPokemonID = Nothing
      , fullName = "Castform"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 351 "Castform"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 1 "During normal weather, fog, sandstorm, or shadowy aura"
      }
    , { id = 1565
      , nationalDexNumber = 351
      , originalPokemonID = Just 479
      , fullName = nameWithForm "Castform" <| Unique "" "Sunny"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrlWithForm 351 "Castform" <| Unique "" "Sunny"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 1 "During harsh sunlight"
      }
    , { id = 1566
      , nationalDexNumber = 351
      , originalPokemonID = Just 479
      , fullName = nameWithForm "Castform" <| Unique "" "Rainy"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlWithForm 351 "Castform" <| Unique "" "Rainy"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 1 "During rain"
      }
    , { id = 1567
      , nationalDexNumber = 351
      , originalPokemonID = Just 479
      , fullName = nameWithForm "Castform" <| Unique "" "Snowy"
      , typing = Single Ice
      , ability = Nothing
      , imageUrl = imageUrlWithForm 351 "Castform" <| Unique "" "Snowy"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 1 "During hail"
      }
    , { id = 480
      , nationalDexNumber = 352
      , originalPokemonID = Nothing
      , fullName = "Kecleon"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 352 "Kecleon"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 481
      , nationalDexNumber = 353
      , originalPokemonID = Nothing
      , fullName = "Shuppet"
      , typing = Single Ghost
      , ability = Nothing
      , imageUrl = imageUrl 353 "Shuppet"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 482
      , nationalDexNumber = 354
      , originalPokemonID = Nothing
      , fullName = "Banette"
      , typing = Single Ghost
      , ability = Nothing
      , imageUrl = imageUrl 354 "Banette"
      , evolutionData = EvolvesFrom [ 481 ] "Level 37"
      , transformationData = DoesNotTransform
      }
    , { id = 1592
      , nationalDexNumber = 354
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Banette" Mega
      , typing = Single Ghost
      , ability = Nothing
      , imageUrl = imageUrlWithForm 354 "Banette" Mega
      , evolutionData = EvolvesFrom [ 482 ] "Holding Banettite"
      , transformationData = DoesNotTransform
      }
    , { id = 483
      , nationalDexNumber = 355
      , originalPokemonID = Nothing
      , fullName = "Duskull"
      , typing = Single Ghost
      , ability = Just Levitate
      , imageUrl = imageUrl 355 "Duskull"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 484
      , nationalDexNumber = 356
      , originalPokemonID = Nothing
      , fullName = "Dusclops"
      , typing = Single Ghost
      , ability = Nothing
      , imageUrl = imageUrl 356 "Dusclops"
      , evolutionData = EvolvesFrom [ 483 ] "Level 37"
      , transformationData = DoesNotTransform
      }
    , { id = 727
      , nationalDexNumber = 477
      , originalPokemonID = Nothing
      , fullName = "Dusknoir"
      , typing = Single Ghost
      , ability = Nothing
      , imageUrl = imageUrl 477 "Dusknoir"
      , evolutionData = EvolvesFrom [ 484 ] "Trade holding Reaper Cloth"
      , transformationData = DoesNotTransform
      }
    , { id = 485
      , nationalDexNumber = 357
      , originalPokemonID = Nothing
      , fullName = "Tropius"
      , typing = Double Grass Flying
      , ability = Nothing
      , imageUrl = imageUrl 357 "Tropius"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 631
      , nationalDexNumber = 433
      , originalPokemonID = Nothing
      , fullName = "Chingling"
      , typing = Single Psychic
      , ability = Just Levitate
      , imageUrl = imageUrl 433 "Chingling"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 486
      , nationalDexNumber = 358
      , originalPokemonID = Nothing
      , fullName = "Chimecho"
      , typing = Single Psychic
      , ability = Just Levitate
      , imageUrl = imageUrl 358 "Chimecho"
      , evolutionData = EvolvesFrom [ 631 ] "Level during the night with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 487
      , nationalDexNumber = 359
      , originalPokemonID = Nothing
      , fullName = "Absol"
      , typing = Single Dark
      , ability = Nothing
      , imageUrl = imageUrl 359 "Absol"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1591
      , nationalDexNumber = 359
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Absol" Mega
      , typing = Single Dark
      , ability = Nothing
      , imageUrl = imageUrlWithForm 359 "Absol" Mega
      , evolutionData = EvolvesFrom [ 487 ] "Holding Absolite"
      , transformationData = DoesNotTransform
      }
    , { id = 489
      , nationalDexNumber = 361
      , originalPokemonID = Nothing
      , fullName = "Snorunt"
      , typing = Single Ice
      , ability = Nothing
      , imageUrl = imageUrl 361 "Snorunt"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 490
      , nationalDexNumber = 362
      , originalPokemonID = Nothing
      , fullName = "Glalie"
      , typing = Single Ice
      , ability = Nothing
      , imageUrl = imageUrl 362 "Glalie"
      , evolutionData = EvolvesFrom [ 489 ] "Level 42"
      , transformationData = DoesNotTransform
      }
    , { id = 1560
      , nationalDexNumber = 362
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Glalie" Mega
      , typing = Single Ice
      , ability = Nothing
      , imageUrl = imageUrlWithForm 362 "Glalie" Mega
      , evolutionData = EvolvesFrom [ 490 ] "Holding Glalitite"
      , transformationData = DoesNotTransform
      }
    , { id = 728
      , nationalDexNumber = 478
      , originalPokemonID = Nothing
      , fullName = "Froslass"
      , typing = Double Ice Ghost
      , ability = Nothing
      , imageUrl = imageUrl 478 "Froslass"
      , evolutionData = EvolvesFrom [ 489 ] "Use Dawn Stone when female"
      , transformationData = DoesNotTransform
      }
    , { id = 491
      , nationalDexNumber = 363
      , originalPokemonID = Nothing
      , fullName = "Spheal"
      , typing = Double Ice Water
      , ability = Just ThickFat
      , imageUrl = imageUrl 363 "Spheal"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 492
      , nationalDexNumber = 364
      , originalPokemonID = Nothing
      , fullName = "Sealeo"
      , typing = Double Ice Water
      , ability = Just ThickFat
      , imageUrl = imageUrl 364 "Sealeo"
      , evolutionData = EvolvesFrom [ 491 ] "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 493
      , nationalDexNumber = 365
      , originalPokemonID = Nothing
      , fullName = "Walrein"
      , typing = Double Ice Water
      , ability = Just ThickFat
      , imageUrl = imageUrl 365 "Walrein"
      , evolutionData = EvolvesFrom [ 492 ] "Level 44"
      , transformationData = DoesNotTransform
      }
    , { id = 494
      , nationalDexNumber = 366
      , originalPokemonID = Nothing
      , fullName = "Clamperl"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 366 "Clamperl"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 495
      , nationalDexNumber = 367
      , originalPokemonID = Nothing
      , fullName = "Huntail"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 367 "Huntail"
      , evolutionData = EvolvesFrom [ 494 ] "Trade holding Deep Sea Tooth"
      , transformationData = DoesNotTransform
      }
    , { id = 496
      , nationalDexNumber = 368
      , originalPokemonID = Nothing
      , fullName = "Gorebyss"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 368 "Gorebyss"
      , evolutionData = EvolvesFrom [ 494 ] "Trade holding Deep Sea Scale"
      , transformationData = DoesNotTransform
      }
    , { id = 497
      , nationalDexNumber = 369
      , originalPokemonID = Nothing
      , fullName = "Relicanth"
      , typing = Double Water Rock
      , ability = Nothing
      , imageUrl = imageUrl 369 "Relicanth"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 498
      , nationalDexNumber = 370
      , originalPokemonID = Nothing
      , fullName = "Luvdisc"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 370 "Luvdisc"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 499
      , nationalDexNumber = 371
      , originalPokemonID = Nothing
      , fullName = "Bagon"
      , typing = Single Dragon
      , ability = Nothing
      , imageUrl = imageUrl 371 "Bagon"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 500
      , nationalDexNumber = 372
      , originalPokemonID = Nothing
      , fullName = "Shelgon"
      , typing = Single Dragon
      , ability = Nothing
      , imageUrl = imageUrl 372 "Shelgon"
      , evolutionData = EvolvesFrom [ 499 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 501
      , nationalDexNumber = 373
      , originalPokemonID = Nothing
      , fullName = "Salamence"
      , typing = Double Dragon Flying
      , ability = Nothing
      , imageUrl = imageUrl 373 "Salamence"
      , evolutionData = EvolvesFrom [ 500 ] "Level 50"
      , transformationData = DoesNotTransform
      }
    , { id = 1586
      , nationalDexNumber = 373
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Salamence" Mega
      , typing = Double Dragon Flying
      , ability = Nothing
      , imageUrl = imageUrlWithForm 373 "Salamence" Mega
      , evolutionData = EvolvesFrom [ 501 ] "Holding Salamencite"
      , transformationData = DoesNotTransform
      }
    , { id = 502
      , nationalDexNumber = 374
      , originalPokemonID = Nothing
      , fullName = "Beldum"
      , typing = Double Steel Psychic
      , ability = Nothing
      , imageUrl = imageUrl 374 "Beldum"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 503
      , nationalDexNumber = 375
      , originalPokemonID = Nothing
      , fullName = "Metang"
      , typing = Double Steel Psychic
      , ability = Nothing
      , imageUrl = imageUrl 375 "Metang"
      , evolutionData = EvolvesFrom [ 502 ] "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 504
      , nationalDexNumber = 376
      , originalPokemonID = Nothing
      , fullName = "Metagross"
      , typing = Double Steel Psychic
      , ability = Nothing
      , imageUrl = imageUrl 376 "Metagross"
      , evolutionData = EvolvesFrom [ 503 ] "Level 45"
      , transformationData = DoesNotTransform
      }
    , { id = 1585
      , nationalDexNumber = 376
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Metagross" Mega
      , typing = Double Steel Psychic
      , ability = Nothing
      , imageUrl = imageUrlWithForm 376 "Metagross" Mega
      , evolutionData = EvolvesFrom [ 504 ] "Holding Metagrossite"
      , transformationData = DoesNotTransform
      }
    , { id = 505
      , nationalDexNumber = 377
      , originalPokemonID = Nothing
      , fullName = "Regirock"
      , typing = Single Rock
      , ability = Nothing
      , imageUrl = imageUrl 377 "Regirock"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 506
      , nationalDexNumber = 378
      , originalPokemonID = Nothing
      , fullName = "Regice"
      , typing = Single Ice
      , ability = Nothing
      , imageUrl = imageUrl 378 "Regice"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 507
      , nationalDexNumber = 379
      , originalPokemonID = Nothing
      , fullName = "Registeel"
      , typing = Single Steel
      , ability = Nothing
      , imageUrl = imageUrl 379 "Registeel"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 508
      , nationalDexNumber = 380
      , originalPokemonID = Nothing
      , fullName = "Latias"
      , typing = Double Dragon Psychic
      , ability = Just Levitate
      , imageUrl = imageUrl 380 "Latias"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1400
      , nationalDexNumber = 380
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Latias" Mega
      , typing = Double Dragon Psychic
      , ability = Just Levitate
      , imageUrl = imageUrlWithForm 380 "Latias" Mega
      , evolutionData = EvolvesFrom [ 508 ] "Holding Latiasite"
      , transformationData = DoesNotTransform
      }
    , { id = 509
      , nationalDexNumber = 381
      , originalPokemonID = Nothing
      , fullName = "Latios"
      , typing = Double Dragon Psychic
      , ability = Just Levitate
      , imageUrl = imageUrl 381 "Latios"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1399
      , nationalDexNumber = 381
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Latios" Mega
      , typing = Double Dragon Psychic
      , ability = Just Levitate
      , imageUrl = imageUrlWithForm 381 "Latios" Mega
      , evolutionData = EvolvesFrom [ 509 ] "Holding Latiosite"
      , transformationData = DoesNotTransform
      }
    , { id = 510
      , nationalDexNumber = 382
      , originalPokemonID = Nothing
      , fullName = "Kyogre"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 382 "Kyogre"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 2 "If not holding Blue Orb"
      }
    , { id = 1432
      , nationalDexNumber = 382
      , originalPokemonID = Just 510
      , fullName = nameWithForm "Kyogre" <| Unique "Primal" ""
      , typing = Single Water
      , ability = Just PrimordialSea
      , imageUrl = imageUrlWithForm 382 "Kyogre" <| Unique "" "Primal"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 2 "While holding Blue Orb"
      }
    , { id = 511
      , nationalDexNumber = 383
      , originalPokemonID = Nothing
      , fullName = "Groudon"
      , typing = Single Ground
      , ability = Nothing
      , imageUrl = imageUrl 383 "Groudon"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 3 "If not holding Red Orb"
      }
    , { id = 1433
      , nationalDexNumber = 383
      , originalPokemonID = Just 511
      , fullName = nameWithForm "Groudon" <| Unique "Primal" ""
      , typing = Double Ground Fire
      , ability = Just DesolateLand
      , imageUrl = imageUrlWithForm 383 "Groudon" <| Unique "" "Primal"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 3 "While holding Red Orb"
      }
    , { id = 512
      , nationalDexNumber = 384
      , originalPokemonID = Nothing
      , fullName = "Rayquaza"
      , typing = Double Dragon Flying
      , ability = Nothing
      , imageUrl = imageUrl 384 "Rayquaza"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1431
      , nationalDexNumber = 384
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Rayquaza" Mega
      , typing = Double Dragon Flying
      , ability = Just DeltaStream
      , imageUrl = imageUrlWithForm 384 "Rayquaza" Mega
      , evolutionData = EvolvesFrom [ 512 ] "Knowing Dragon Ascent and not holding a Z-Crystal"
      , transformationData = DoesNotTransform
      }
    , { id = 513
      , nationalDexNumber = 385
      , originalPokemonID = Nothing
      , fullName = "Jirachi"
      , typing = Double Steel Psychic
      , ability = Nothing
      , imageUrl = imageUrl 385 "Jirachi"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 514
      , nationalDexNumber = 386
      , originalPokemonID = Nothing
      , fullName = "Deoxys"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrl 386 "Deoxys"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 4 "In Pokémon Ruby and Sapphire"
      }
    , { id = 1391
      , nationalDexNumber = 386
      , originalPokemonID = Just 514
      , fullName = nameWithForm "Deoxys" <| Unique "" "Attack"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrlWithForm 386 "Deoxys" <| Unique "" "Attack"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 4 "In Pokémon FireRed"
      }
    , { id = 1392
      , nationalDexNumber = 386
      , originalPokemonID = Just 514
      , fullName = nameWithForm "Deoxys" <| Unique "" "Defense"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrlWithForm 386 "Deoxys" <| Unique "" "Defense"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 4 "In Pokémon LeafGreen"
      }
    , { id = 1393
      , nationalDexNumber = 386
      , originalPokemonID = Just 514
      , fullName = nameWithForm "Deoxys" <| Unique "" "Speed"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrlWithForm 386 "Deoxys" <| Unique "" "Speed"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 4 "In Pokémon Emerald"
      }
    , { id = 591
      , nationalDexNumber = 387
      , originalPokemonID = Nothing
      , fullName = "Turtwig"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrl 387 "Turtwig"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 592
      , nationalDexNumber = 388
      , originalPokemonID = Nothing
      , fullName = "Grotle"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrl 388 "Grotle"
      , evolutionData = EvolvesFrom [ 591 ] "Level 18"
      , transformationData = DoesNotTransform
      }
    , { id = 593
      , nationalDexNumber = 389
      , originalPokemonID = Nothing
      , fullName = "Torterra"
      , typing = Double Grass Ground
      , ability = Nothing
      , imageUrl = imageUrl 389 "Torterra"
      , evolutionData = EvolvesFrom [ 592 ] "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 34
      , nationalDexNumber = 390
      , originalPokemonID = Nothing
      , fullName = "Chimchar"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrl 390 "Chimchar"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 35
      , nationalDexNumber = 391
      , originalPokemonID = Nothing
      , fullName = "Monferno"
      , typing = Double Fire Fighting
      , ability = Nothing
      , imageUrl = imageUrl 391 "Monferno"
      , evolutionData = EvolvesFrom [ 34 ] "Level 14"
      , transformationData = DoesNotTransform
      }
    , { id = 19
      , nationalDexNumber = 392
      , originalPokemonID = Nothing
      , fullName = "Infernape"
      , typing = Double Fire Fighting
      , ability = Nothing
      , imageUrl = imageUrl 392 "Infernape"
      , evolutionData = EvolvesFrom [ 35 ] "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 594
      , nationalDexNumber = 393
      , originalPokemonID = Nothing
      , fullName = "Piplup"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 393 "Piplup"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 595
      , nationalDexNumber = 394
      , originalPokemonID = Nothing
      , fullName = "Prinplup"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 394 "Prinplup"
      , evolutionData = EvolvesFrom [ 594 ] "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 596
      , nationalDexNumber = 395
      , originalPokemonID = Nothing
      , fullName = "Empoleon"
      , typing = Double Water Steel
      , ability = Nothing
      , imageUrl = imageUrl 395 "Empoleon"
      , evolutionData = EvolvesFrom [ 595 ] "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 597
      , nationalDexNumber = 396
      , originalPokemonID = Nothing
      , fullName = "Starly"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrl 396 "Starly"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 598
      , nationalDexNumber = 397
      , originalPokemonID = Nothing
      , fullName = "Staravia"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrl 397 "Staravia"
      , evolutionData = EvolvesFrom [ 597 ] "Level 14"
      , transformationData = DoesNotTransform
      }
    , { id = 599
      , nationalDexNumber = 398
      , originalPokemonID = Nothing
      , fullName = "Staraptor"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrl 398 "Staraptor"
      , evolutionData = EvolvesFrom [ 598 ] "Level 34"
      , transformationData = DoesNotTransform
      }
    , { id = 600
      , nationalDexNumber = 399
      , originalPokemonID = Nothing
      , fullName = "Bidoof"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 399 "Bidoof"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 601
      , nationalDexNumber = 400
      , originalPokemonID = Nothing
      , fullName = "Bibarel"
      , typing = Double Normal Water
      , ability = Nothing
      , imageUrl = imageUrl 400 "Bibarel"
      , evolutionData = EvolvesFrom [ 600 ] "Level 15"
      , transformationData = DoesNotTransform
      }
    , { id = 602
      , nationalDexNumber = 401
      , originalPokemonID = Nothing
      , fullName = "Kricketot"
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imageUrl 401 "Kricketot"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 603
      , nationalDexNumber = 402
      , originalPokemonID = Nothing
      , fullName = "Kricketune"
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imageUrl 402 "Kricketune"
      , evolutionData = EvolvesFrom [ 602 ] "Level 10"
      , transformationData = DoesNotTransform
      }
    , { id = 20
      , nationalDexNumber = 403
      , originalPokemonID = Nothing
      , fullName = "Shinx"
      , typing = Single Electric
      , ability = Nothing
      , imageUrl = imageUrl 403 "Shinx"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 21
      , nationalDexNumber = 404
      , originalPokemonID = Nothing
      , fullName = "Luxio"
      , typing = Single Electric
      , ability = Nothing
      , imageUrl = imageUrl 404 "Luxio"
      , evolutionData = EvolvesFrom [ 20 ] "Level 15"
      , transformationData = DoesNotTransform
      }
    , { id = 22
      , nationalDexNumber = 405
      , originalPokemonID = Nothing
      , fullName = "Luxray"
      , typing = Single Electric
      , ability = Nothing
      , imageUrl = imageUrl 405 "Luxray"
      , evolutionData = EvolvesFrom [ 21 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 606
      , nationalDexNumber = 408
      , originalPokemonID = Nothing
      , fullName = "Cranidos"
      , typing = Single Rock
      , ability = Nothing
      , imageUrl = imageUrl 408 "Cranidos"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 607
      , nationalDexNumber = 409
      , originalPokemonID = Nothing
      , fullName = "Rampardos"
      , typing = Single Rock
      , ability = Nothing
      , imageUrl = imageUrl 409 "Rampardos"
      , evolutionData = EvolvesFrom [ 606 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 608
      , nationalDexNumber = 410
      , originalPokemonID = Nothing
      , fullName = "Shieldon"
      , typing = Double Rock Steel
      , ability = Nothing
      , imageUrl = imageUrl 410 "Shieldon"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 609
      , nationalDexNumber = 411
      , originalPokemonID = Nothing
      , fullName = "Bastiodon"
      , typing = Double Rock Steel
      , ability = Nothing
      , imageUrl = imageUrl 411 "Bastiodon"
      , evolutionData = EvolvesFrom [ 608 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 610
      , nationalDexNumber = 412
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Burmy" <| Unique "" "Plant"
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imageUrlWithForm 412 "Burmy" <| Unique "" "Plant"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 5 "If it last battled in grass"
      }
    , { id = 611
      , nationalDexNumber = 413
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Wormadam" <| Unique "" "Plant"
      , typing = Double Bug Grass
      , ability = Nothing
      , imageUrl = imageUrlWithForm 413 "Wormadam" <| Unique "" "Plant"
      , evolutionData = EvolvesFrom [ 610 ] "Level 20 when female"
      , transformationData = DoesNotTransform
      }
    , { id = 1606
      , nationalDexNumber = 412
      , originalPokemonID = Just 610
      , fullName = nameWithForm "Burmy" <| Unique "" "Sandy"
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imageUrlWithForm 412 "Burmy" <| Unique "" "Sandy"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 5 "If it last battled in a cave"
      }
    , { id = 1568
      , nationalDexNumber = 413
      , originalPokemonID = Just 611
      , fullName = nameWithForm "Wormadam" <| Unique "" "Sandy"
      , typing = Double Bug Ground
      , ability = Nothing
      , imageUrl = imageUrlWithForm 413 "Wormadam" <| Unique "" "Sandy"
      , evolutionData = EvolvesFrom [ 1606 ] "Level 20 when female"
      , transformationData = DoesNotTransform
      }
    , { id = 1607
      , nationalDexNumber = 412
      , originalPokemonID = Just 610
      , fullName = nameWithForm "Burmy" <| Unique "" "Trash"
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imageUrlWithForm 412 "Burmy" <| Unique "" "Trash"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 5 "If it last battled in a building"
      }
    , { id = 1569
      , nationalDexNumber = 413
      , originalPokemonID = Just 611
      , fullName = nameWithForm "Wormadam" <| Unique "" "Trash"
      , typing = Double Bug Steel
      , ability = Nothing
      , imageUrl = imageUrlWithForm 413 "Wormadam" <| Unique "" "Trash"
      , evolutionData = EvolvesFrom [ 1607 ] "Level 20 when female"
      , transformationData = DoesNotTransform
      }
    , { id = 612
      , nationalDexNumber = 414
      , originalPokemonID = Nothing
      , fullName = "Mothim"
      , typing = Double Bug Flying
      , ability = Nothing
      , imageUrl = imageUrl 414 "Mothim"
      , evolutionData = EvolvesFrom [ 610, 1606, 1607 ] "Level 20 when male"
      , transformationData = DoesNotTransform
      }
    , { id = 613
      , nationalDexNumber = 415
      , originalPokemonID = Nothing
      , fullName = "Combee"
      , typing = Double Bug Flying
      , ability = Nothing
      , imageUrl = imageUrl 415 "Combee"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 614
      , nationalDexNumber = 416
      , originalPokemonID = Nothing
      , fullName = "Vespiquen"
      , typing = Double Bug Flying
      , ability = Nothing
      , imageUrl = imageUrl 416 "Vespiquen"
      , evolutionData = EvolvesFrom [ 613 ] "Level 21 When Female"
      , transformationData = DoesNotTransform
      }
    , { id = 615
      , nationalDexNumber = 417
      , originalPokemonID = Nothing
      , fullName = "Pachirisu"
      , typing = Single Electric
      , ability = Nothing
      , imageUrl = imageUrl 417 "Pachirisu"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 616
      , nationalDexNumber = 418
      , originalPokemonID = Nothing
      , fullName = "Buizel"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 418 "Buizel"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 617
      , nationalDexNumber = 419
      , originalPokemonID = Nothing
      , fullName = "Floatzel"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 419 "Floatzel"
      , evolutionData = EvolvesFrom [ 616 ] "Level 26"
      , transformationData = DoesNotTransform
      }
    , { id = 618
      , nationalDexNumber = 420
      , originalPokemonID = Nothing
      , fullName = "Cherubi"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrl 420 "Cherubi"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 619
      , nationalDexNumber = 421
      , originalPokemonID = Nothing
      , fullName = "Cherrim"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrl 421 "Cherrim"
      , evolutionData = EvolvesFrom [ 618 ] "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 620
      , nationalDexNumber = 422
      , originalPokemonID = Nothing
      , fullName = "Shellos"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlWithForm 422 "Shellos" <| Unique "" "West"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 621
      , nationalDexNumber = 423
      , originalPokemonID = Nothing
      , fullName = "Gastrodon"
      , typing = Double Water Ground
      , ability = Nothing
      , imageUrl = imageUrlWithForm 423 "Gastrodon" <| Unique "" "West"
      , evolutionData = EvolvesFrom [ 620 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 623
      , nationalDexNumber = 425
      , originalPokemonID = Nothing
      , fullName = "Drifloon"
      , typing = Double Ghost Flying
      , ability = Nothing
      , imageUrl = imageUrl 425 "Drifloon"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 624
      , nationalDexNumber = 426
      , originalPokemonID = Nothing
      , fullName = "Drifblim"
      , typing = Double Ghost Flying
      , ability = Nothing
      , imageUrl = imageUrl 426 "Drifblim"
      , evolutionData = EvolvesFrom [ 623 ] "Level 28"
      , transformationData = DoesNotTransform
      }
    , { id = 625
      , nationalDexNumber = 427
      , originalPokemonID = Nothing
      , fullName = "Buneary"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 427 "Buneary"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 626
      , nationalDexNumber = 428
      , originalPokemonID = Nothing
      , fullName = "Lopunny"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 428 "Lopunny"
      , evolutionData = EvolvesFrom [ 625 ] "Level with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 1593
      , nationalDexNumber = 428
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Lopunny" Mega
      , typing = Double Normal Fighting
      , ability = Nothing
      , imageUrl = imageUrlWithForm 428 "Lopunny" Mega
      , evolutionData = EvolvesFrom [ 626 ] "Holding Lopunnite"
      , transformationData = DoesNotTransform
      }
    , { id = 629
      , nationalDexNumber = 431
      , originalPokemonID = Nothing
      , fullName = "Glameow"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 431 "Glameow"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 630
      , nationalDexNumber = 432
      , originalPokemonID = Nothing
      , fullName = "Purugly"
      , typing = Single Normal
      , ability = Just ThickFat
      , imageUrl = imageUrl 432 "Purugly"
      , evolutionData = EvolvesFrom [ 629 ] "Level 38"
      , transformationData = DoesNotTransform
      }
    , { id = 632
      , nationalDexNumber = 434
      , originalPokemonID = Nothing
      , fullName = "Stunky"
      , typing = Double Poison Dark
      , ability = Nothing
      , imageUrl = imageUrl 434 "Stunky"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 633
      , nationalDexNumber = 435
      , originalPokemonID = Nothing
      , fullName = "Skuntank"
      , typing = Double Poison Dark
      , ability = Nothing
      , imageUrl = imageUrl 435 "Skuntank"
      , evolutionData = EvolvesFrom [ 632 ] "Level 34"
      , transformationData = DoesNotTransform
      }
    , { id = 634
      , nationalDexNumber = 436
      , originalPokemonID = Nothing
      , fullName = "Bronzor"
      , typing = Double Steel Psychic
      , ability = Just Levitate
      , imageUrl = imageUrl 436 "Bronzor"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 635
      , nationalDexNumber = 437
      , originalPokemonID = Nothing
      , fullName = "Bronzong"
      , typing = Double Steel Psychic
      , ability = Just Levitate
      , imageUrl = imageUrl 437 "Bronzong"
      , evolutionData = EvolvesFrom [ 634 ] "Level 33"
      , transformationData = DoesNotTransform
      }
    , { id = 639
      , nationalDexNumber = 441
      , originalPokemonID = Nothing
      , fullName = "Chatot"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrl 441 "Chatot"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 640
      , nationalDexNumber = 442
      , originalPokemonID = Nothing
      , fullName = "Spiritomb"
      , typing = Double Ghost Dark
      , ability = Nothing
      , imageUrl = imageUrl 442 "Spiritomb"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 641
      , nationalDexNumber = 443
      , originalPokemonID = Nothing
      , fullName = "Gible"
      , typing = Double Dragon Ground
      , ability = Nothing
      , imageUrl = imageUrl 443 "Gible"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 642
      , nationalDexNumber = 444
      , originalPokemonID = Nothing
      , fullName = "Gabite"
      , typing = Double Dragon Ground
      , ability = Nothing
      , imageUrl = imageUrl 444 "Gabite"
      , evolutionData = EvolvesFrom [ 641 ] "Level 24"
      , transformationData = DoesNotTransform
      }
    , { id = 643
      , nationalDexNumber = 445
      , originalPokemonID = Nothing
      , fullName = "Garchomp"
      , typing = Double Dragon Ground
      , ability = Nothing
      , imageUrl = imageUrl 445 "Garchomp"
      , evolutionData = EvolvesFrom [ 642 ] "Level 48"
      , transformationData = DoesNotTransform
      }
    , { id = 1438
      , nationalDexNumber = 445
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Garchomp" Mega
      , typing = Double Dragon Ground
      , ability = Nothing
      , imageUrl = imageUrlWithForm 445 "Garchomp" Mega
      , evolutionData = EvolvesFrom [ 643 ] "Holding Garchompite"
      , transformationData = DoesNotTransform
      }
    , { id = 645
      , nationalDexNumber = 447
      , originalPokemonID = Nothing
      , fullName = "Riolu"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrl 447 "Riolu"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 646
      , nationalDexNumber = 448
      , originalPokemonID = Nothing
      , fullName = "Lucario"
      , typing = Double Fighting Steel
      , ability = Nothing
      , imageUrl = imageUrl 448 "Lucario"
      , evolutionData = EvolvesFrom [ 645 ] "Level during the day with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 1561
      , nationalDexNumber = 448
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Lucario" Mega
      , typing = Double Fighting Steel
      , ability = Nothing
      , imageUrl = imageUrlWithForm 448 "Lucario" Mega
      , evolutionData = EvolvesFrom [ 646 ] "Holding Lucarionite"
      , transformationData = DoesNotTransform
      }
    , { id = 647
      , nationalDexNumber = 449
      , originalPokemonID = Nothing
      , fullName = "Hippopotas"
      , typing = Single Ground
      , ability = Nothing
      , imageUrl = imageUrl 449 "Hippopotas"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 648
      , nationalDexNumber = 450
      , originalPokemonID = Nothing
      , fullName = "Hippowdon"
      , typing = Single Ground
      , ability = Nothing
      , imageUrl = imageUrl 450 "Hippowdon"
      , evolutionData = EvolvesFrom [ 647 ] "Level 34"
      , transformationData = DoesNotTransform
      }
    , { id = 649
      , nationalDexNumber = 451
      , originalPokemonID = Nothing
      , fullName = "Skorupi"
      , typing = Double Poison Bug
      , ability = Nothing
      , imageUrl = imageUrl 451 "Skorupi"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 650
      , nationalDexNumber = 452
      , originalPokemonID = Nothing
      , fullName = "Drapion"
      , typing = Double Poison Dark
      , ability = Nothing
      , imageUrl = imageUrl 452 "Drapion"
      , evolutionData = EvolvesFrom [ 649 ] "Level 40"
      , transformationData = DoesNotTransform
      }
    , { id = 651
      , nationalDexNumber = 453
      , originalPokemonID = Nothing
      , fullName = "Croagunk"
      , typing = Double Poison Fighting
      , ability = Just DrySkin
      , imageUrl = imageUrl 453 "Croagunk"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 652
      , nationalDexNumber = 454
      , originalPokemonID = Nothing
      , fullName = "Toxicroak"
      , typing = Double Poison Fighting
      , ability = Just DrySkin
      , imageUrl = imageUrl 454 "Toxicroak"
      , evolutionData = EvolvesFrom [ 651 ] "Level 37"
      , transformationData = DoesNotTransform
      }
    , { id = 653
      , nationalDexNumber = 455
      , originalPokemonID = Nothing
      , fullName = "Carnivine"
      , typing = Single Grass
      , ability = Just Levitate
      , imageUrl = imageUrl 455 "Carnivine"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 654
      , nationalDexNumber = 456
      , originalPokemonID = Nothing
      , fullName = "Finneon"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 456 "Finneon"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 655
      , nationalDexNumber = 457
      , originalPokemonID = Nothing
      , fullName = "Lumineon"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 457 "Lumineon"
      , evolutionData = EvolvesFrom [ 654 ] "Level 31"
      , transformationData = DoesNotTransform
      }
    , { id = 657
      , nationalDexNumber = 459
      , originalPokemonID = Nothing
      , fullName = "Snover"
      , typing = Double Grass Ice
      , ability = Nothing
      , imageUrl = imageUrl 459 "Snover"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 658
      , nationalDexNumber = 460
      , originalPokemonID = Nothing
      , fullName = "Abomasnow"
      , typing = Double Grass Ice
      , ability = Nothing
      , imageUrl = imageUrl 460 "Abomasnow"
      , evolutionData = EvolvesFrom [ 657 ] "Level 40"
      , transformationData = DoesNotTransform
      }
    , { id = 1562
      , nationalDexNumber = 460
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Abomasnow" Mega
      , typing = Double Grass Ice
      , ability = Nothing
      , imageUrl = imageUrlWithForm 460 "Abomasnow" Mega
      , evolutionData = EvolvesFrom [ 658 ] "Holding Abomasite"
      , transformationData = DoesNotTransform
      }
    , { id = 729
      , nationalDexNumber = 479
      , originalPokemonID = Nothing
      , fullName = "Rotom"
      , typing = Double Electric Ghost
      , ability = Just Levitate
      , imageUrl = imageUrl 479 "Rotom"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 6 "When not possessing an appliance"
      }
    , { id = 1408
      , nationalDexNumber = 479
      , originalPokemonID = Just 729
      , fullName = nameWithForm "Rotom" <| Unique "" "Heat"
      , typing = Double Electric Fire
      , ability = Just Levitate
      , imageUrl = imageUrlWithForm 479 "Rotom" <| Unique "" "Heat"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 6 "When it possesses a microwave oven"
      }
    , { id = 1409
      , nationalDexNumber = 479
      , originalPokemonID = Just 729
      , fullName = nameWithForm "Rotom" <| Unique "" "Wash"
      , typing = Double Electric Water
      , ability = Just Levitate
      , imageUrl = imageUrlWithForm 479 "Rotom" <| Unique "" "Wash"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 6 "When it possesses a washing machine"
      }
    , { id = 1410
      , nationalDexNumber = 479
      , originalPokemonID = Just 729
      , fullName = nameWithForm "Rotom" <| Unique "" "Frost"
      , typing = Double Electric Ice
      , ability = Just Levitate
      , imageUrl = imageUrlWithForm 479 "Rotom" <| Unique "" "Frost"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 6 "When it possesses a refrigerator"
      }
    , { id = 1411
      , nationalDexNumber = 479
      , originalPokemonID = Just 729
      , fullName = nameWithForm "Rotom" <| Unique "" "Fan"
      , typing = Double Electric Flying
      , ability = Just Levitate
      , imageUrl = imageUrlWithForm 479 "Rotom" <| Unique "" "Fan"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 6 "When it possesses an electric fan"
      }
    , { id = 1413
      , nationalDexNumber = 479
      , originalPokemonID = Just 729
      , fullName = nameWithForm "Rotom" <| Unique "" "Mow"
      , typing = Double Electric Grass
      , ability = Just Levitate
      , imageUrl = imageUrlWithForm 479 "Rotom" <| Unique "" "Mow"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 6 "When it possesses a lawn mower"
      }
    , { id = 730
      , nationalDexNumber = 480
      , originalPokemonID = Nothing
      , fullName = "Uxie"
      , typing = Single Psychic
      , ability = Just Levitate
      , imageUrl = imageUrl 480 "Uxie"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 731
      , nationalDexNumber = 481
      , originalPokemonID = Nothing
      , fullName = "Mesprit"
      , typing = Single Psychic
      , ability = Just Levitate
      , imageUrl = imageUrl 481 "Mesprit"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 732
      , nationalDexNumber = 482
      , originalPokemonID = Nothing
      , fullName = "Azelf"
      , typing = Single Psychic
      , ability = Just Levitate
      , imageUrl = imageUrl 482 "Azelf"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 733
      , nationalDexNumber = 483
      , originalPokemonID = Nothing
      , fullName = "Dialga"
      , typing = Double Steel Dragon
      , ability = Nothing
      , imageUrl = imageUrl 483 "Dialga"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 734
      , nationalDexNumber = 484
      , originalPokemonID = Nothing
      , fullName = "Palkia"
      , typing = Double Water Dragon
      , ability = Nothing
      , imageUrl = imageUrl 484 "Palkia"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 735
      , nationalDexNumber = 485
      , originalPokemonID = Nothing
      , fullName = "Heatran"
      , typing = Double Fire Steel
      , ability = Just FlashFire
      , imageUrl = imageUrl 485 "Heatran"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 736
      , nationalDexNumber = 486
      , originalPokemonID = Nothing
      , fullName = "Regigigas"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 486 "Regigigas"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 737
      , nationalDexNumber = 487
      , originalPokemonID = Nothing
      , fullName = "Giratina"
      , typing = Double Ghost Dragon
      , ability = Nothing
      , imageUrl = imageUrl 487 "Giratina"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 7 "If not holding a Griseous Orb"
      }
    , { id = 1401
      , nationalDexNumber = 487
      , originalPokemonID = Just 737
      , fullName = nameWithForm "Giratina" <| Unique "" "Origin"
      , typing = Double Ghost Dragon
      , ability = Just Levitate
      , imageUrl = imageUrlWithForm 487 "Giratina" <| Unique "" "Origin"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 7 "While holding a Griseous Orb"
      }
    , { id = 738
      , nationalDexNumber = 488
      , originalPokemonID = Nothing
      , fullName = "Cresselia"
      , typing = Single Psychic
      , ability = Just Levitate
      , imageUrl = imageUrl 488 "Cresselia"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 36
      , nationalDexNumber = 489
      , originalPokemonID = Nothing
      , fullName = "Phione"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 489 "Phione"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 37
      , nationalDexNumber = 490
      , originalPokemonID = Nothing
      , fullName = "Manaphy"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 490 "Manaphy"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 739
      , nationalDexNumber = 491
      , originalPokemonID = Nothing
      , fullName = "Darkrai"
      , typing = Single Dark
      , ability = Nothing
      , imageUrl = imageUrl 491 "Darkrai"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 740
      , nationalDexNumber = 492
      , originalPokemonID = Nothing
      , fullName = "Shaymin"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrl 492 "Shaymin"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 8 "During the night or when frozen"
      }
    , { id = 1407
      , nationalDexNumber = 492
      , originalPokemonID = Just 740
      , fullName = nameWithForm "Shaymin" <| Unique "" "Sky"
      , typing = Double Grass Flying
      , ability = Nothing
      , imageUrl = imageUrlWithForm 492 "Shaymin" <| Unique "" "Sky"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 8 "Use a Gracidea Flower in the daytime"
      }
    , { id = 741
      , nationalDexNumber = 493
      , originalPokemonID = Nothing
      , fullName = "Arceus"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 493 "Arceus"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 742
      , nationalDexNumber = 494
      , originalPokemonID = Nothing
      , fullName = "Victini"
      , typing = Double Psychic Fire
      , ability = Nothing
      , imageUrl = imageUrl 494 "Victini"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 829
      , nationalDexNumber = 495
      , originalPokemonID = Nothing
      , fullName = "Snivy"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrl 495 "Snivy"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 830
      , nationalDexNumber = 496
      , originalPokemonID = Nothing
      , fullName = "Servine"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrl 496 "Servine"
      , evolutionData = EvolvesFrom [ 829 ] "Level 17"
      , transformationData = DoesNotTransform
      }
    , { id = 831
      , nationalDexNumber = 497
      , originalPokemonID = Nothing
      , fullName = "Serperior"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrl 497 "Serperior"
      , evolutionData = EvolvesFrom [ 830 ] "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 832
      , nationalDexNumber = 498
      , originalPokemonID = Nothing
      , fullName = "Tepig"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrl 498 "Tepig"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 833
      , nationalDexNumber = 499
      , originalPokemonID = Nothing
      , fullName = "Pignite"
      , typing = Double Fire Fighting
      , ability = Nothing
      , imageUrl = imageUrl 499 "Pignite"
      , evolutionData = EvolvesFrom [ 832 ] "Level 17"
      , transformationData = DoesNotTransform
      }
    , { id = 834
      , nationalDexNumber = 500
      , originalPokemonID = Nothing
      , fullName = "Emboar"
      , typing = Double Fire Fighting
      , ability = Nothing
      , imageUrl = imageUrl 500 "Emboar"
      , evolutionData = EvolvesFrom [ 833 ] "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 835
      , nationalDexNumber = 501
      , originalPokemonID = Nothing
      , fullName = "Oshawott"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 501 "Oshawott"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 836
      , nationalDexNumber = 502
      , originalPokemonID = Nothing
      , fullName = "Dewott"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 502 "Dewott"
      , evolutionData = EvolvesFrom [ 835 ] "Level 17"
      , transformationData = DoesNotTransform
      }
    , { id = 837
      , nationalDexNumber = 503
      , originalPokemonID = Nothing
      , fullName = "Samurott"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 503 "Samurott"
      , evolutionData = EvolvesFrom [ 836 ] "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 1624
      , nationalDexNumber = 503
      , originalPokemonID = Just 837
      , fullName = nameWithForm "Samurott" Hisuian
      , typing = Double Water Dark
      , ability = Nothing
      , imageUrl = imageUrlWithForm 503 "Samurott" Hisuian
      , evolutionData = EvolvesFrom [ 836 ] "Level 36 in Legends: Arceus"
      , transformationData = DoesNotTransform
      }
    , { id = 838
      , nationalDexNumber = 504
      , originalPokemonID = Nothing
      , fullName = "Patrat"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 504 "Patrat"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 839
      , nationalDexNumber = 505
      , originalPokemonID = Nothing
      , fullName = "Watchog"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 505 "Watchog"
      , evolutionData = EvolvesFrom [ 838 ] "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 840
      , nationalDexNumber = 506
      , originalPokemonID = Nothing
      , fullName = "Lillipup"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 506 "Lillipup"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 841
      , nationalDexNumber = 507
      , originalPokemonID = Nothing
      , fullName = "Herdier"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 507 "Herdier"
      , evolutionData = EvolvesFrom [ 840 ] "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 842
      , nationalDexNumber = 508
      , originalPokemonID = Nothing
      , fullName = "Stoutland"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 508 "Stoutland"
      , evolutionData = EvolvesFrom [ 841 ] "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 843
      , nationalDexNumber = 509
      , originalPokemonID = Nothing
      , fullName = "Purrloin"
      , typing = Single Dark
      , ability = Nothing
      , imageUrl = imageUrl 509 "Purrloin"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 844
      , nationalDexNumber = 510
      , originalPokemonID = Nothing
      , fullName = "Liepard"
      , typing = Single Dark
      , ability = Nothing
      , imageUrl = imageUrl 510 "Liepard"
      , evolutionData = EvolvesFrom [ 843 ] "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 845
      , nationalDexNumber = 511
      , originalPokemonID = Nothing
      , fullName = "Pansage"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrl 511 "Pansage"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 846
      , nationalDexNumber = 512
      , originalPokemonID = Nothing
      , fullName = "Simisage"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrl 512 "Simisage"
      , evolutionData = EvolvesFrom [ 845 ] "Use Leaf Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 847
      , nationalDexNumber = 513
      , originalPokemonID = Nothing
      , fullName = "Pansear"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrl 513 "Pansear"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 848
      , nationalDexNumber = 514
      , originalPokemonID = Nothing
      , fullName = "Simisear"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrl 514 "Simisear"
      , evolutionData = EvolvesFrom [ 847 ] "Use Fire Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 849
      , nationalDexNumber = 515
      , originalPokemonID = Nothing
      , fullName = "Panpour"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 515 "Panpour"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 850
      , nationalDexNumber = 516
      , originalPokemonID = Nothing
      , fullName = "Simipour"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 516 "Simipour"
      , evolutionData = EvolvesFrom [ 849 ] "Use Water Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 851
      , nationalDexNumber = 517
      , originalPokemonID = Nothing
      , fullName = "Munna"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrl 517 "Munna"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 852
      , nationalDexNumber = 518
      , originalPokemonID = Nothing
      , fullName = "Musharna"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrl 518 "Musharna"
      , evolutionData = EvolvesFrom [ 851 ] "Use Moon Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 853
      , nationalDexNumber = 519
      , originalPokemonID = Nothing
      , fullName = "Pidove"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrl 519 "Pidove"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 854
      , nationalDexNumber = 520
      , originalPokemonID = Nothing
      , fullName = "Tranquill"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrl 520 "Tranquill"
      , evolutionData = EvolvesFrom [ 853 ] "Level 21"
      , transformationData = DoesNotTransform
      }
    , { id = 855
      , nationalDexNumber = 521
      , originalPokemonID = Nothing
      , fullName = "Unfezant"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrl 521 "Unfezant"
      , evolutionData = EvolvesFrom [ 854 ] "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 856
      , nationalDexNumber = 522
      , originalPokemonID = Nothing
      , fullName = "Blitzle"
      , typing = Single Electric
      , ability = Just LightningRod
      , imageUrl = imageUrl 522 "Blitzle"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 857
      , nationalDexNumber = 523
      , originalPokemonID = Nothing
      , fullName = "Zebstrika"
      , typing = Single Electric
      , ability = Just LightningRod
      , imageUrl = imageUrl 523 "Zebstrika"
      , evolutionData = EvolvesFrom [ 856 ] "Level 27"
      , transformationData = DoesNotTransform
      }
    , { id = 858
      , nationalDexNumber = 524
      , originalPokemonID = Nothing
      , fullName = "Roggenrola"
      , typing = Single Rock
      , ability = Nothing
      , imageUrl = imageUrl 524 "Roggenrola"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 859
      , nationalDexNumber = 525
      , originalPokemonID = Nothing
      , fullName = "Boldore"
      , typing = Single Rock
      , ability = Nothing
      , imageUrl = imageUrl 525 "Boldore"
      , evolutionData = EvolvesFrom [ 858 ] "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 860
      , nationalDexNumber = 526
      , originalPokemonID = Nothing
      , fullName = "Gigalith"
      , typing = Single Rock
      , ability = Nothing
      , imageUrl = imageUrl 526 "Gigalith"
      , evolutionData = EvolvesFrom [ 859 ] "Trade"
      , transformationData = DoesNotTransform
      }
    , { id = 861
      , nationalDexNumber = 527
      , originalPokemonID = Nothing
      , fullName = "Woobat"
      , typing = Double Psychic Flying
      , ability = Nothing
      , imageUrl = imageUrl 527 "Woobat"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 862
      , nationalDexNumber = 528
      , originalPokemonID = Nothing
      , fullName = "Swoobat"
      , typing = Double Psychic Flying
      , ability = Nothing
      , imageUrl = imageUrl 528 "Swoobat"
      , evolutionData = EvolvesFrom [ 861 ] "Level with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 863
      , nationalDexNumber = 529
      , originalPokemonID = Nothing
      , fullName = "Drilbur"
      , typing = Single Ground
      , ability = Nothing
      , imageUrl = imageUrl 529 "Drilbur"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 864
      , nationalDexNumber = 530
      , originalPokemonID = Nothing
      , fullName = "Excadrill"
      , typing = Double Ground Steel
      , ability = Nothing
      , imageUrl = imageUrl 530 "Excadrill"
      , evolutionData = EvolvesFrom [ 863 ] "Level 31"
      , transformationData = DoesNotTransform
      }
    , { id = 865
      , nationalDexNumber = 531
      , originalPokemonID = Nothing
      , fullName = "Audino"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 531 "Audino"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1563
      , nationalDexNumber = 531
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Audino" Mega
      , typing = Double Normal Fairy
      , ability = Nothing
      , imageUrl = imageUrlWithForm 531 "Audino" Mega
      , evolutionData = EvolvesFrom [ 865 ] "Holding Audinite"
      , transformationData = DoesNotTransform
      }
    , { id = 866
      , nationalDexNumber = 532
      , originalPokemonID = Nothing
      , fullName = "Timburr"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrl 532 "Timburr"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 867
      , nationalDexNumber = 533
      , originalPokemonID = Nothing
      , fullName = "Gurdurr"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrl 533 "Gurdurr"
      , evolutionData = EvolvesFrom [ 866 ] "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 868
      , nationalDexNumber = 534
      , originalPokemonID = Nothing
      , fullName = "Conkeldurr"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrl 534 "Conkeldurr"
      , evolutionData = EvolvesFrom [ 867 ] "Trade"
      , transformationData = DoesNotTransform
      }
    , { id = 869
      , nationalDexNumber = 535
      , originalPokemonID = Nothing
      , fullName = "Tympole"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 535 "Tympole"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 870
      , nationalDexNumber = 536
      , originalPokemonID = Nothing
      , fullName = "Palpitoad"
      , typing = Double Water Ground
      , ability = Nothing
      , imageUrl = imageUrl 536 "Palpitoad"
      , evolutionData = EvolvesFrom [ 869 ] "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 871
      , nationalDexNumber = 537
      , originalPokemonID = Nothing
      , fullName = "Seismitoad"
      , typing = Double Water Ground
      , ability = Nothing
      , imageUrl = imageUrl 537 "Seismitoad"
      , evolutionData = EvolvesFrom [ 870 ] "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 872
      , nationalDexNumber = 538
      , originalPokemonID = Nothing
      , fullName = "Throh"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrl 538 "Throh"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 873
      , nationalDexNumber = 539
      , originalPokemonID = Nothing
      , fullName = "Sawk"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrl 539 "Sawk"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 874
      , nationalDexNumber = 540
      , originalPokemonID = Nothing
      , fullName = "Sewaddle"
      , typing = Double Bug Grass
      , ability = Nothing
      , imageUrl = imageUrl 540 "Sewaddle"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 875
      , nationalDexNumber = 541
      , originalPokemonID = Nothing
      , fullName = "Swadloon"
      , typing = Double Bug Grass
      , ability = Nothing
      , imageUrl = imageUrl 541 "Swadloon"
      , evolutionData = EvolvesFrom [ 874 ] "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 876
      , nationalDexNumber = 542
      , originalPokemonID = Nothing
      , fullName = "Leavanny"
      , typing = Double Bug Grass
      , ability = Nothing
      , imageUrl = imageUrl 542 "Leavanny"
      , evolutionData = EvolvesFrom [ 875 ] "Level with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 877
      , nationalDexNumber = 543
      , originalPokemonID = Nothing
      , fullName = "Venipede"
      , typing = Double Bug Poison
      , ability = Nothing
      , imageUrl = imageUrl 543 "Venipede"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 878
      , nationalDexNumber = 544
      , originalPokemonID = Nothing
      , fullName = "Whirlipede"
      , typing = Double Bug Poison
      , ability = Nothing
      , imageUrl = imageUrl 544 "Whirlipede"
      , evolutionData = EvolvesFrom [ 877 ] "Level 22"
      , transformationData = DoesNotTransform
      }
    , { id = 879
      , nationalDexNumber = 545
      , originalPokemonID = Nothing
      , fullName = "Scolipede"
      , typing = Double Bug Poison
      , ability = Nothing
      , imageUrl = imageUrl 545 "Scolipede"
      , evolutionData = EvolvesFrom [ 878 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 880
      , nationalDexNumber = 546
      , originalPokemonID = Nothing
      , fullName = "Cottonee"
      , typing = Double Grass Fairy
      , ability = Nothing
      , imageUrl = imageUrl 546 "Cottonee"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 881
      , nationalDexNumber = 547
      , originalPokemonID = Nothing
      , fullName = "Whimsicott"
      , typing = Double Grass Fairy
      , ability = Nothing
      , imageUrl = imageUrl 547 "Whimsicott"
      , evolutionData = EvolvesFrom [ 880 ] "Use Sun Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 882
      , nationalDexNumber = 548
      , originalPokemonID = Nothing
      , fullName = "Petilil"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrl 548 "Petilil"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 883
      , nationalDexNumber = 549
      , originalPokemonID = Nothing
      , fullName = "Lilligant"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrl 549 "Lilligant"
      , evolutionData = EvolvesFrom [ 882 ] "Use Sun Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 1625
      , nationalDexNumber = 549
      , originalPokemonID = Just 883
      , fullName = nameWithForm "Lilligant" Hisuian
      , typing = Double Grass Fighting
      , ability = Nothing
      , imageUrl = imageUrlWithForm 549 "Lilligant" Hisuian
      , evolutionData = EvolvesFrom [ 882 ] "Use Sun Stone in Legends: Arceus"
      , transformationData = DoesNotTransform
      }
    , { id = 885
      , nationalDexNumber = 550
      , originalPokemonID = Nothing
      , fullName = "Basculin"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlWithForm 550 "Basculin" <| Unique "" "Red"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1414
      , nationalDexNumber = 550
      , originalPokemonID = Just 885
      , fullName = nameWithForm "Basculin" <| Unique "Blue-Striped" ""
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlWithForm 550 "Basculin" <| Unique "" "Blue"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1608
      , nationalDexNumber = 550
      , originalPokemonID = Just 885
      , fullName = nameWithForm "Basculin" <| Unique "White-Striped" ""
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlWithForm 550 "Basculin" <| Unique "" "White"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1612
      , nationalDexNumber = 902
      , originalPokemonID = Nothing
      , fullName = "Basculegion"
      , typing = Double Water Ghost
      , ability = Nothing
      , imageUrl = imageUrlWithForm 902 "Basculegion" <| Unique "" "Female"
      , evolutionData = EvolvesFrom [ 1608 ] "After losing at least 294 HP from recoil damage"
      , transformationData = DoesNotTransform
      }
    , { id = 886
      , nationalDexNumber = 551
      , originalPokemonID = Nothing
      , fullName = "Sandile"
      , typing = Double Ground Dark
      , ability = Nothing
      , imageUrl = imageUrl 551 "Sandile"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 887
      , nationalDexNumber = 552
      , originalPokemonID = Nothing
      , fullName = "Krokorok"
      , typing = Double Ground Dark
      , ability = Nothing
      , imageUrl = imageUrl 552 "Krokorok"
      , evolutionData = EvolvesFrom [ 886 ] "Level 29"
      , transformationData = DoesNotTransform
      }
    , { id = 888
      , nationalDexNumber = 553
      , originalPokemonID = Nothing
      , fullName = "Krookodile"
      , typing = Double Ground Dark
      , ability = Nothing
      , imageUrl = imageUrl 553 "Krookodile"
      , evolutionData = EvolvesFrom [ 887 ] "Level 40"
      , transformationData = DoesNotTransform
      }
    , { id = 889
      , nationalDexNumber = 554
      , originalPokemonID = Nothing
      , fullName = "Darumaka"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrl 554 "Darumaka"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 890
      , nationalDexNumber = 555
      , originalPokemonID = Nothing
      , fullName = "Darmanitan"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrl 555 "Darmanitan"
      , evolutionData = EvolvesFrom [ 889 ] "Level 35"
      , transformationData = Transforms 9 "With HP above half"
      }
    , { id = 1554
      , nationalDexNumber = 555
      , originalPokemonID = Just 890
      , fullName = nameWithForm "Darmanitan" <| Unique "Zen" ""
      , typing = Double Fire Psychic
      , ability = Nothing
      , imageUrl = imageUrlWithForm 555 "Darmanitan" <| Unique "" "Zen"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 9 "With HP below half"
      }
    , { id = 1557
      , nationalDexNumber = 554
      , originalPokemonID = Just 889
      , fullName = nameWithForm "Darumaka" Galarian
      , typing = Single Ice
      , ability = Nothing
      , imageUrl = imageUrlWithForm 554 "Darumaka" Galarian
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1555
      , nationalDexNumber = 555
      , originalPokemonID = Just 890
      , fullName = nameWithForm "Darmanitan" Galarian
      , typing = Single Ice
      , ability = Nothing
      , imageUrl = imageUrlWithForm 555 "Darmanitan" Galarian
      , evolutionData = EvolvesFrom [ 1557 ] "Use Ice Stone"
      , transformationData = Transforms 10 "With HP above half"
      }
    , { id = 1556
      , nationalDexNumber = 555
      , originalPokemonID = Just 890
      , fullName = nameWithForm "Darmanitan" <| Unique "Galarian Zen" ""
      , typing = Double Ice Fire
      , ability = Nothing
      , imageUrl = imageUrlWithForm 555 "Darmanitan" <| Unique "" "Galar-Zen"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 10 "With HP below half"
      }
    , { id = 891
      , nationalDexNumber = 556
      , originalPokemonID = Nothing
      , fullName = "Maractus"
      , typing = Single Grass
      , ability = Just WaterAbsorb
      , imageUrl = imageUrl 556 "Maractus"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 892
      , nationalDexNumber = 557
      , originalPokemonID = Nothing
      , fullName = "Dwebble"
      , typing = Double Bug Rock
      , ability = Nothing
      , imageUrl = imageUrl 557 "Dwebble"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 893
      , nationalDexNumber = 558
      , originalPokemonID = Nothing
      , fullName = "Crustle"
      , typing = Double Bug Rock
      , ability = Nothing
      , imageUrl = imageUrl 558 "Crustle"
      , evolutionData = EvolvesFrom [ 892 ] "Level 34"
      , transformationData = DoesNotTransform
      }
    , { id = 894
      , nationalDexNumber = 559
      , originalPokemonID = Nothing
      , fullName = "Scraggy"
      , typing = Double Dark Fighting
      , ability = Nothing
      , imageUrl = imageUrl 559 "Scraggy"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 914
      , nationalDexNumber = 560
      , originalPokemonID = Nothing
      , fullName = "Scrafty"
      , typing = Double Dark Fighting
      , ability = Nothing
      , imageUrl = imageUrl 560 "Scrafty"
      , evolutionData = EvolvesFrom [ 894 ] "Level 39"
      , transformationData = DoesNotTransform
      }
    , { id = 915
      , nationalDexNumber = 561
      , originalPokemonID = Nothing
      , fullName = "Sigilyph"
      , typing = Double Psychic Flying
      , ability = Nothing
      , imageUrl = imageUrl 561 "Sigilyph"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 916
      , nationalDexNumber = 562
      , originalPokemonID = Nothing
      , fullName = "Yamask"
      , typing = Single Ghost
      , ability = Nothing
      , imageUrl = imageUrl 562 "Yamask"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 917
      , nationalDexNumber = 563
      , originalPokemonID = Nothing
      , fullName = "Cofagrigus"
      , typing = Single Ghost
      , ability = Nothing
      , imageUrl = imageUrl 563 "Cofagrigus"
      , evolutionData = EvolvesFrom [ 916 ] "Level 34"
      , transformationData = DoesNotTransform
      }
    , { id = 1464
      , nationalDexNumber = 562
      , originalPokemonID = Just 916
      , fullName = nameWithForm "Yamask" Galarian
      , typing = Double Ground Ghost
      , ability = Nothing
      , imageUrl = imageUrlWithForm 562 "Yamask" Galarian
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1465
      , nationalDexNumber = 867
      , originalPokemonID = Nothing
      , fullName = "Runerigus"
      , typing = Double Ground Ghost
      , ability = Nothing
      , imageUrl = imageUrl 867 "Runerigus"
      , evolutionData = EvolvesFrom [ 1464 ] "Take 49+ damage and travel under the Stone Bridge in Dusty Bowl in Galar"
      , transformationData = DoesNotTransform
      }
    , { id = 918
      , nationalDexNumber = 564
      , originalPokemonID = Nothing
      , fullName = "Tirtouga"
      , typing = Double Water Rock
      , ability = Nothing
      , imageUrl = imageUrl 564 "Tirtouga"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 919
      , nationalDexNumber = 565
      , originalPokemonID = Nothing
      , fullName = "Carracosta"
      , typing = Double Water Rock
      , ability = Nothing
      , imageUrl = imageUrl 565 "Carracosta"
      , evolutionData = EvolvesFrom [ 918 ] "Level 37"
      , transformationData = DoesNotTransform
      }
    , { id = 920
      , nationalDexNumber = 566
      , originalPokemonID = Nothing
      , fullName = "Archen"
      , typing = Double Rock Flying
      , ability = Nothing
      , imageUrl = imageUrl 566 "Archen"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 921
      , nationalDexNumber = 567
      , originalPokemonID = Nothing
      , fullName = "Archeops"
      , typing = Double Rock Flying
      , ability = Nothing
      , imageUrl = imageUrl 567 "Archeops"
      , evolutionData = EvolvesFrom [ 920 ] "Level 37"
      , transformationData = DoesNotTransform
      }
    , { id = 922
      , nationalDexNumber = 568
      , originalPokemonID = Nothing
      , fullName = "Trubbish"
      , typing = Single Poison
      , ability = Nothing
      , imageUrl = imageUrl 568 "Trubbish"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 923
      , nationalDexNumber = 569
      , originalPokemonID = Nothing
      , fullName = "Garbodor"
      , typing = Single Poison
      , ability = Nothing
      , imageUrl = imageUrl 569 "Garbodor"
      , evolutionData = EvolvesFrom [ 922 ] "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 939
      , nationalDexNumber = 570
      , originalPokemonID = Nothing
      , fullName = "Zorua"
      , typing = Single Dark
      , ability = Nothing
      , imageUrl = imageUrl 570 "Zorua"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 940
      , nationalDexNumber = 571
      , originalPokemonID = Nothing
      , fullName = "Zoroark"
      , typing = Single Dark
      , ability = Nothing
      , imageUrl = imageUrl 571 "Zoroark"
      , evolutionData = EvolvesFrom [ 939 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 1626
      , nationalDexNumber = 570
      , originalPokemonID = Just 939
      , fullName = nameWithForm "Zorua" Hisuian
      , typing = Double Normal Ghost
      , ability = Nothing
      , imageUrl = imageUrlWithForm 570 "Zorua" Hisuian
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1627
      , nationalDexNumber = 571
      , originalPokemonID = Just 940
      , fullName = nameWithForm "Zoroark" Hisuian
      , typing = Double Normal Ghost
      , ability = Nothing
      , imageUrl = imageUrlWithForm 571 "Zoroark" Hisuian
      , evolutionData = EvolvesFrom [ 1626 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 941
      , nationalDexNumber = 572
      , originalPokemonID = Nothing
      , fullName = "Minccino"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 572 "Minccino"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 942
      , nationalDexNumber = 573
      , originalPokemonID = Nothing
      , fullName = "Cinccino"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 573 "Cinccino"
      , evolutionData = EvolvesFrom [ 941 ] "Use Shiny Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 943
      , nationalDexNumber = 574
      , originalPokemonID = Nothing
      , fullName = "Gothita"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrl 574 "Gothita"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 944
      , nationalDexNumber = 575
      , originalPokemonID = Nothing
      , fullName = "Gothorita"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrl 575 "Gothorita"
      , evolutionData = EvolvesFrom [ 943 ] "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 945
      , nationalDexNumber = 576
      , originalPokemonID = Nothing
      , fullName = "Gothitelle"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrl 576 "Gothitelle"
      , evolutionData = EvolvesFrom [ 944 ] "Level 41"
      , transformationData = DoesNotTransform
      }
    , { id = 946
      , nationalDexNumber = 577
      , originalPokemonID = Nothing
      , fullName = "Solosis"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrl 577 "Solosis"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 947
      , nationalDexNumber = 578
      , originalPokemonID = Nothing
      , fullName = "Duosion"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrl 578 "Duosion"
      , evolutionData = EvolvesFrom [ 946 ] "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 948
      , nationalDexNumber = 579
      , originalPokemonID = Nothing
      , fullName = "Reuniclus"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrl 579 "Reuniclus"
      , evolutionData = EvolvesFrom [ 947 ] "Level 41"
      , transformationData = DoesNotTransform
      }
    , { id = 950
      , nationalDexNumber = 580
      , originalPokemonID = Nothing
      , fullName = "Ducklett"
      , typing = Double Water Flying
      , ability = Nothing
      , imageUrl = imageUrl 580 "Ducklett"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 951
      , nationalDexNumber = 581
      , originalPokemonID = Nothing
      , fullName = "Swanna"
      , typing = Double Water Flying
      , ability = Nothing
      , imageUrl = imageUrl 581 "Swanna"
      , evolutionData = EvolvesFrom [ 950 ] "Level 35"
      , transformationData = DoesNotTransform
      }
    , { id = 952
      , nationalDexNumber = 582
      , originalPokemonID = Nothing
      , fullName = "Vanillite"
      , typing = Single Ice
      , ability = Nothing
      , imageUrl = imageUrl 582 "Vanillite"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 953
      , nationalDexNumber = 583
      , originalPokemonID = Nothing
      , fullName = "Vanillish"
      , typing = Single Ice
      , ability = Nothing
      , imageUrl = imageUrl 583 "Vanillish"
      , evolutionData = EvolvesFrom [ 952 ] "Level 35"
      , transformationData = DoesNotTransform
      }
    , { id = 954
      , nationalDexNumber = 584
      , originalPokemonID = Nothing
      , fullName = "Vanilluxe"
      , typing = Single Ice
      , ability = Nothing
      , imageUrl = imageUrl 584 "Vanilluxe"
      , evolutionData = EvolvesFrom [ 953 ] "Level 47"
      , transformationData = DoesNotTransform
      }
    , { id = 955
      , nationalDexNumber = 585
      , originalPokemonID = Nothing
      , fullName = "Deerling"
      , typing = Double Normal Grass
      , ability = Just SapSipper
      , imageUrl = imageUrl 585 "Deerling"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 956
      , nationalDexNumber = 586
      , originalPokemonID = Nothing
      , fullName = "Sawsbuck"
      , typing = Double Normal Grass
      , ability = Just SapSipper
      , imageUrl = imageUrl 586 "Sawsbuck"
      , evolutionData = EvolvesFrom [ 955 ] "Level 34"
      , transformationData = DoesNotTransform
      }
    , { id = 957
      , nationalDexNumber = 587
      , originalPokemonID = Nothing
      , fullName = "Emolga"
      , typing = Double Electric Flying
      , ability = Nothing
      , imageUrl = imageUrl 587 "Emolga"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 958
      , nationalDexNumber = 588
      , originalPokemonID = Nothing
      , fullName = "Karrablast"
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imageUrl 588 "Karrablast"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 959
      , nationalDexNumber = 589
      , originalPokemonID = Nothing
      , fullName = "Escavalier"
      , typing = Double Bug Steel
      , ability = Nothing
      , imageUrl = imageUrl 589 "Escavalier"
      , evolutionData = EvolvesFrom [ 958 ] "Trade for a Shelmet"
      , transformationData = DoesNotTransform
      }
    , { id = 961
      , nationalDexNumber = 590
      , originalPokemonID = Nothing
      , fullName = "Foongus"
      , typing = Double Grass Poison
      , ability = Nothing
      , imageUrl = imageUrl 590 "Foongus"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 962
      , nationalDexNumber = 591
      , originalPokemonID = Nothing
      , fullName = "Amoonguss"
      , typing = Double Grass Poison
      , ability = Nothing
      , imageUrl = imageUrl 591 "Amoonguss"
      , evolutionData = EvolvesFrom [ 961 ] "Level 39"
      , transformationData = DoesNotTransform
      }
    , { id = 963
      , nationalDexNumber = 592
      , originalPokemonID = Nothing
      , fullName = "Frillish"
      , typing = Double Water Ghost
      , ability = Just WaterAbsorb
      , imageUrl = imageUrl 592 "Frillish"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 964
      , nationalDexNumber = 593
      , originalPokemonID = Nothing
      , fullName = "Jellicent"
      , typing = Double Water Ghost
      , ability = Just WaterAbsorb
      , imageUrl = imageUrl 593 "Jellicent"
      , evolutionData = EvolvesFrom [ 963 ] "Level 40"
      , transformationData = DoesNotTransform
      }
    , { id = 965
      , nationalDexNumber = 594
      , originalPokemonID = Nothing
      , fullName = "Alomomola"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 594 "Alomomola"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 966
      , nationalDexNumber = 595
      , originalPokemonID = Nothing
      , fullName = "Joltik"
      , typing = Double Bug Electric
      , ability = Nothing
      , imageUrl = imageUrl 595 "Joltik"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 967
      , nationalDexNumber = 596
      , originalPokemonID = Nothing
      , fullName = "Galvantula"
      , typing = Double Bug Electric
      , ability = Nothing
      , imageUrl = imageUrl 596 "Galvantula"
      , evolutionData = EvolvesFrom [ 966 ] "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 968
      , nationalDexNumber = 597
      , originalPokemonID = Nothing
      , fullName = "Ferroseed"
      , typing = Double Grass Steel
      , ability = Nothing
      , imageUrl = imageUrl 597 "Ferroseed"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 969
      , nationalDexNumber = 598
      , originalPokemonID = Nothing
      , fullName = "Ferrothorn"
      , typing = Double Grass Steel
      , ability = Nothing
      , imageUrl = imageUrl 598 "Ferrothorn"
      , evolutionData = EvolvesFrom [ 968 ] "Level 40"
      , transformationData = DoesNotTransform
      }
    , { id = 970
      , nationalDexNumber = 599
      , originalPokemonID = Nothing
      , fullName = "Klink"
      , typing = Single Steel
      , ability = Nothing
      , imageUrl = imageUrl 599 "Klink"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 972
      , nationalDexNumber = 600
      , originalPokemonID = Nothing
      , fullName = "Klang"
      , typing = Single Steel
      , ability = Nothing
      , imageUrl = imageUrl 600 "Klang"
      , evolutionData = EvolvesFrom [ 970 ] "Level 38"
      , transformationData = DoesNotTransform
      }
    , { id = 973
      , nationalDexNumber = 601
      , originalPokemonID = Nothing
      , fullName = "Klinklang"
      , typing = Single Steel
      , ability = Nothing
      , imageUrl = imageUrl 601 "Klinklang"
      , evolutionData = EvolvesFrom [ 972 ] "Level 49"
      , transformationData = DoesNotTransform
      }
    , { id = 974
      , nationalDexNumber = 602
      , originalPokemonID = Nothing
      , fullName = "Tynamo"
      , typing = Single Electric
      , ability = Just Levitate
      , imageUrl = imageUrl 602 "Tynamo"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 975
      , nationalDexNumber = 603
      , originalPokemonID = Nothing
      , fullName = "Eelektrik"
      , typing = Single Electric
      , ability = Just Levitate
      , imageUrl = imageUrl 603 "Eelektrik"
      , evolutionData = EvolvesFrom [ 974 ] "Level 39"
      , transformationData = DoesNotTransform
      }
    , { id = 976
      , nationalDexNumber = 604
      , originalPokemonID = Nothing
      , fullName = "Eelektross"
      , typing = Single Electric
      , ability = Just Levitate
      , imageUrl = imageUrl 604 "Eelektross"
      , evolutionData = EvolvesFrom [ 975 ] "Use Thunder Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 977
      , nationalDexNumber = 605
      , originalPokemonID = Nothing
      , fullName = "Elgyem"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrl 605 "Elgyem"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 978
      , nationalDexNumber = 606
      , originalPokemonID = Nothing
      , fullName = "Beheeyem"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrl 606 "Beheeyem"
      , evolutionData = EvolvesFrom [ 977 ] "Level 42"
      , transformationData = DoesNotTransform
      }
    , { id = 979
      , nationalDexNumber = 607
      , originalPokemonID = Nothing
      , fullName = "Litwick"
      , typing = Double Ghost Fire
      , ability = Just FlashFire
      , imageUrl = imageUrl 607 "Litwick"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 980
      , nationalDexNumber = 608
      , originalPokemonID = Nothing
      , fullName = "Lampent"
      , typing = Double Ghost Fire
      , ability = Just FlashFire
      , imageUrl = imageUrl 608 "Lampent"
      , evolutionData = EvolvesFrom [ 979 ] "Level 41"
      , transformationData = DoesNotTransform
      }
    , { id = 981
      , nationalDexNumber = 609
      , originalPokemonID = Nothing
      , fullName = "Chandelure"
      , typing = Double Ghost Fire
      , ability = Just FlashFire
      , imageUrl = imageUrl 609 "Chandelure"
      , evolutionData = EvolvesFrom [ 980 ] "Use Dusk Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 983
      , nationalDexNumber = 610
      , originalPokemonID = Nothing
      , fullName = "Axew"
      , typing = Single Dragon
      , ability = Nothing
      , imageUrl = imageUrl 610 "Axew"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 984
      , nationalDexNumber = 611
      , originalPokemonID = Nothing
      , fullName = "Fraxure"
      , typing = Single Dragon
      , ability = Nothing
      , imageUrl = imageUrl 611 "Fraxure"
      , evolutionData = EvolvesFrom [ 983 ] "Level 38"
      , transformationData = DoesNotTransform
      }
    , { id = 985
      , nationalDexNumber = 612
      , originalPokemonID = Nothing
      , fullName = "Haxorus"
      , typing = Single Dragon
      , ability = Nothing
      , imageUrl = imageUrl 612 "Haxorus"
      , evolutionData = EvolvesFrom [ 984 ] "Level 48"
      , transformationData = DoesNotTransform
      }
    , { id = 986
      , nationalDexNumber = 613
      , originalPokemonID = Nothing
      , fullName = "Cubchoo"
      , typing = Single Ice
      , ability = Nothing
      , imageUrl = imageUrl 613 "Cubchoo"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 987
      , nationalDexNumber = 614
      , originalPokemonID = Nothing
      , fullName = "Beartic"
      , typing = Single Ice
      , ability = Nothing
      , imageUrl = imageUrl 614 "Beartic"
      , evolutionData = EvolvesFrom [ 986 ] "Level 37"
      , transformationData = DoesNotTransform
      }
    , { id = 988
      , nationalDexNumber = 615
      , originalPokemonID = Nothing
      , fullName = "Cryogonal"
      , typing = Single Ice
      , ability = Just Levitate
      , imageUrl = imageUrl 615 "Cryogonal"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 989
      , nationalDexNumber = 616
      , originalPokemonID = Nothing
      , fullName = "Shelmet"
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imageUrl 616 "Shelmet"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 990
      , nationalDexNumber = 617
      , originalPokemonID = Nothing
      , fullName = "Accelgor"
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imageUrl 617 "Accelgor"
      , evolutionData = EvolvesFrom [ 989 ] "Trade for Karrablast"
      , transformationData = DoesNotTransform
      }
    , { id = 991
      , nationalDexNumber = 618
      , originalPokemonID = Nothing
      , fullName = "Stunfisk"
      , typing = Double Ground Electric
      , ability = Nothing
      , imageUrl = imageUrl 618 "Stunfisk"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1553
      , nationalDexNumber = 618
      , originalPokemonID = Just 991
      , fullName = nameWithForm "Stunfisk" Galarian
      , typing = Double Ground Steel
      , ability = Nothing
      , imageUrl = imageUrlWithForm 618 "Stunfisk" Galarian
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 992
      , nationalDexNumber = 619
      , originalPokemonID = Nothing
      , fullName = "Mienfoo"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrl 619 "Mienfoo"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 994
      , nationalDexNumber = 620
      , originalPokemonID = Nothing
      , fullName = "Mienshao"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrl 620 "Mienshao"
      , evolutionData = EvolvesFrom [ 992 ] "Level 50"
      , transformationData = DoesNotTransform
      }
    , { id = 995
      , nationalDexNumber = 621
      , originalPokemonID = Nothing
      , fullName = "Druddigon"
      , typing = Single Dragon
      , ability = Nothing
      , imageUrl = imageUrl 621 "Druddigon"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 996
      , nationalDexNumber = 622
      , originalPokemonID = Nothing
      , fullName = "Golett"
      , typing = Double Ground Ghost
      , ability = Nothing
      , imageUrl = imageUrl 622 "Golett"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 997
      , nationalDexNumber = 623
      , originalPokemonID = Nothing
      , fullName = "Golurk"
      , typing = Double Ground Ghost
      , ability = Nothing
      , imageUrl = imageUrl 623 "Golurk"
      , evolutionData = EvolvesFrom [ 996 ] "Level 43"
      , transformationData = DoesNotTransform
      }
    , { id = 998
      , nationalDexNumber = 624
      , originalPokemonID = Nothing
      , fullName = "Pawniard"
      , typing = Double Dark Steel
      , ability = Nothing
      , imageUrl = imageUrl 624 "Pawniard"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 999
      , nationalDexNumber = 625
      , originalPokemonID = Nothing
      , fullName = "Bisharp"
      , typing = Double Dark Steel
      , ability = Nothing
      , imageUrl = imageUrl 625 "Bisharp"
      , evolutionData = EvolvesFrom [ 998 ] "Level 52"
      , transformationData = DoesNotTransform
      }
    , { id = 1711
      , nationalDexNumber = 983
      , originalPokemonID = Nothing
      , fullName = "Kingambit"
      , typing = Double Dark Steel
      , ability = Nothing
      , imageUrl = imageUrl 983 "Kingambit"
      , evolutionData = EvolvesFrom [ 999 ] "Level after defeating three Bisharp that lead a pack of Pawniard"
      , transformationData = DoesNotTransform
      }
    , { id = 1000
      , nationalDexNumber = 626
      , originalPokemonID = Nothing
      , fullName = "Bouffalant"
      , typing = Single Normal
      , ability = Just SapSipper
      , imageUrl = imageUrl 626 "Bouffalant"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1001
      , nationalDexNumber = 627
      , originalPokemonID = Nothing
      , fullName = "Rufflet"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrl 627 "Rufflet"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1002
      , nationalDexNumber = 628
      , originalPokemonID = Nothing
      , fullName = "Braviary"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrl 628 "Braviary"
      , evolutionData = EvolvesFrom [ 1001 ] "Level 54"
      , transformationData = DoesNotTransform
      }
    , { id = 1628
      , nationalDexNumber = 628
      , originalPokemonID = Just 1002
      , fullName = nameWithForm "Braviary" Hisuian
      , typing = Double Psychic Flying
      , ability = Nothing
      , imageUrl = imageUrlWithForm 628 "Braviary" Hisuian
      , evolutionData = EvolvesFrom [ 1001 ] "Level 54 in Legends: Arceus"
      , transformationData = DoesNotTransform
      }
    , { id = 1003
      , nationalDexNumber = 629
      , originalPokemonID = Nothing
      , fullName = "Vullaby"
      , typing = Double Dark Flying
      , ability = Nothing
      , imageUrl = imageUrl 629 "Vullaby"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1005
      , nationalDexNumber = 630
      , originalPokemonID = Nothing
      , fullName = "Mandibuzz"
      , typing = Double Dark Flying
      , ability = Nothing
      , imageUrl = imageUrl 630 "Mandibuzz"
      , evolutionData = EvolvesFrom [ 1003 ] "Level 54"
      , transformationData = DoesNotTransform
      }
    , { id = 1006
      , nationalDexNumber = 631
      , originalPokemonID = Nothing
      , fullName = "Heatmor"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrl 631 "Heatmor"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1007
      , nationalDexNumber = 632
      , originalPokemonID = Nothing
      , fullName = "Durant"
      , typing = Double Bug Steel
      , ability = Nothing
      , imageUrl = imageUrl 632 "Durant"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1008
      , nationalDexNumber = 633
      , originalPokemonID = Nothing
      , fullName = "Deino"
      , typing = Double Dark Dragon
      , ability = Nothing
      , imageUrl = imageUrl 633 "Deino"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1009
      , nationalDexNumber = 634
      , originalPokemonID = Nothing
      , fullName = "Zweilous"
      , typing = Double Dark Dragon
      , ability = Nothing
      , imageUrl = imageUrl 634 "Zweilous"
      , evolutionData = EvolvesFrom [ 1008 ] "Level 50"
      , transformationData = DoesNotTransform
      }
    , { id = 1010
      , nationalDexNumber = 635
      , originalPokemonID = Nothing
      , fullName = "Hydreigon"
      , typing = Double Dark Dragon
      , ability = Just Levitate
      , imageUrl = imageUrl 635 "Hydreigon"
      , evolutionData = EvolvesFrom [ 1009 ] "Level 64"
      , transformationData = DoesNotTransform
      }
    , { id = 1011
      , nationalDexNumber = 636
      , originalPokemonID = Nothing
      , fullName = "Larvesta"
      , typing = Double Bug Fire
      , ability = Nothing
      , imageUrl = imageUrl 636 "Larvesta"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1012
      , nationalDexNumber = 637
      , originalPokemonID = Nothing
      , fullName = "Volcarona"
      , typing = Double Bug Fire
      , ability = Nothing
      , imageUrl = imageUrl 637 "Volcarona"
      , evolutionData = EvolvesFrom [ 1011 ] "Level 59"
      , transformationData = DoesNotTransform
      }
    , { id = 1013
      , nationalDexNumber = 638
      , originalPokemonID = Nothing
      , fullName = "Cobalion"
      , typing = Double Steel Fighting
      , ability = Nothing
      , imageUrl = imageUrl 638 "Cobalion"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1014
      , nationalDexNumber = 639
      , originalPokemonID = Nothing
      , fullName = "Terrakion"
      , typing = Double Rock Fighting
      , ability = Nothing
      , imageUrl = imageUrl 639 "Terrakion"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1016
      , nationalDexNumber = 640
      , originalPokemonID = Nothing
      , fullName = "Virizion"
      , typing = Double Grass Fighting
      , ability = Nothing
      , imageUrl = imageUrl 640 "Virizion"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1017
      , nationalDexNumber = 641
      , originalPokemonID = Nothing
      , fullName = "Tornadus"
      , typing = Single Flying
      , ability = Nothing
      , imageUrl = imageUrl 641 "Tornadus"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 11 "Use the Reveal Glass"
      }
    , { id = 1415
      , nationalDexNumber = 641
      , originalPokemonID = Just 1017
      , fullName = nameWithForm "Tornadus" <| Unique "Therian" ""
      , typing = Single Flying
      , ability = Nothing
      , imageUrl = imageUrlWithForm 641 "Tornadus" <| Unique "" "Therian"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 11 "Use the Reveal Glass"
      }
    , { id = 1018
      , nationalDexNumber = 642
      , originalPokemonID = Nothing
      , fullName = "Thundurus"
      , typing = Double Electric Flying
      , ability = Nothing
      , imageUrl = imageUrl 642 "Thundurus"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 12 "Use the Reveal Glass"
      }
    , { id = 1416
      , nationalDexNumber = 642
      , originalPokemonID = Just 1018
      , fullName = nameWithForm "Thundurus" <| Unique "Therian" ""
      , typing = Double Electric Flying
      , ability = Just VoltAbsorb
      , imageUrl = imageUrlWithForm 642 "Thundurus" <| Unique "" "Therian"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 12 "Use the Reveal Glass"
      }
    , { id = 1019
      , nationalDexNumber = 643
      , originalPokemonID = Nothing
      , fullName = "Reshiram"
      , typing = Double Dragon Fire
      , ability = Nothing
      , imageUrl = imageUrl 643 "Reshiram"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1020
      , nationalDexNumber = 644
      , originalPokemonID = Nothing
      , fullName = "Zekrom"
      , typing = Double Dragon Electric
      , ability = Nothing
      , imageUrl = imageUrl 644 "Zekrom"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1021
      , nationalDexNumber = 645
      , originalPokemonID = Nothing
      , fullName = "Landorus"
      , typing = Double Ground Flying
      , ability = Nothing
      , imageUrl = imageUrl 645 "Landorus"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 13 "Use the Reveal Glass"
      }
    , { id = 1417
      , nationalDexNumber = 645
      , originalPokemonID = Just 1021
      , fullName = nameWithForm "Landorus" <| Unique "Therian" ""
      , typing = Double Ground Flying
      , ability = Nothing
      , imageUrl = imageUrlWithForm 645 "Landorus" <| Unique "" "Therian"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 13 "Use the Reveal Glass"
      }
    , { id = 1022
      , nationalDexNumber = 646
      , originalPokemonID = Nothing
      , fullName = "Kyurem"
      , typing = Double Dragon Ice
      , ability = Nothing
      , imageUrl = imageUrl 646 "Kyurem"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 14 "Undo DNA Splicing"
      }
    , { id = 1421
      , nationalDexNumber = 646
      , originalPokemonID = Just 1022
      , fullName = nameWithForm "Kyurem" <| Unique "White" ""
      , typing = Double Dragon Ice
      , ability = Nothing
      , imageUrl = imageUrlWithForm 646 "Kyurem" <| Unique "" "White"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 14 "DNA Splice with Reshiram"
      }
    , { id = 1423
      , nationalDexNumber = 646
      , originalPokemonID = Just 1022
      , fullName = nameWithForm "Kyurem" <| Unique "Black" ""
      , typing = Double Dragon Ice
      , ability = Nothing
      , imageUrl = imageUrlWithForm 646 "Kyurem" <| Unique "" "Black"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 14 "DNA Splice with Zekrom"
      }
    , { id = 1023
      , nationalDexNumber = 647
      , originalPokemonID = Nothing
      , fullName = "Keldeo"
      , typing = Double Water Fighting
      , ability = Nothing
      , imageUrl = imageUrl 647 "Keldeo"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1024
      , nationalDexNumber = 648
      , originalPokemonID = Nothing
      , fullName = "Meloetta"
      , typing = Double Normal Psychic
      , ability = Nothing
      , imageUrl = imageUrl 648 "Meloetta"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 15 "Use the move Relic Song"
      }
    , { id = 1570
      , nationalDexNumber = 648
      , originalPokemonID = Just 1024
      , fullName = nameWithForm "Meloetta" <| Unique "Pirouette" ""
      , typing = Double Normal Fighting
      , ability = Nothing
      , imageUrl = imageUrlWithForm 648 "Meloetta" <| Unique "" "Pirouette"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 15 "Use the move Relic Song"
      }
    , { id = 1025
      , nationalDexNumber = 649
      , originalPokemonID = Nothing
      , fullName = "Genesect"
      , typing = Double Bug Steel
      , ability = Nothing
      , imageUrl = imageUrl 649 "Genesect"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1066
      , nationalDexNumber = 650
      , originalPokemonID = Nothing
      , fullName = "Chespin"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrl 650 "Chespin"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1067
      , nationalDexNumber = 651
      , originalPokemonID = Nothing
      , fullName = "Quilladin"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrl 651 "Quilladin"
      , evolutionData = EvolvesFrom [ 1066 ] "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 1068
      , nationalDexNumber = 652
      , originalPokemonID = Nothing
      , fullName = "Chesnaught"
      , typing = Double Grass Fighting
      , ability = Nothing
      , imageUrl = imageUrl 652 "Chesnaught"
      , evolutionData = EvolvesFrom [ 1067 ] "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 1069
      , nationalDexNumber = 653
      , originalPokemonID = Nothing
      , fullName = "Fennekin"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrl 653 "Fennekin"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1070
      , nationalDexNumber = 654
      , originalPokemonID = Nothing
      , fullName = "Braixen"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrl 654 "Braixen"
      , evolutionData = EvolvesFrom [ 1069 ] "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 1071
      , nationalDexNumber = 655
      , originalPokemonID = Nothing
      , fullName = "Delphox"
      , typing = Double Fire Psychic
      , ability = Nothing
      , imageUrl = imageUrl 655 "Delphox"
      , evolutionData = EvolvesFrom [ 1070 ] "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 1072
      , nationalDexNumber = 656
      , originalPokemonID = Nothing
      , fullName = "Froakie"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 656 "Froakie"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1073
      , nationalDexNumber = 657
      , originalPokemonID = Nothing
      , fullName = "Frogadier"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 657 "Frogadier"
      , evolutionData = EvolvesFrom [ 1072 ] "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 1074
      , nationalDexNumber = 658
      , originalPokemonID = Nothing
      , fullName = "Greninja"
      , typing = Double Water Dark
      , ability = Nothing
      , imageUrl = imageUrl 658 "Greninja"
      , evolutionData = EvolvesFrom [ 1073 ] "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 1075
      , nationalDexNumber = 659
      , originalPokemonID = Nothing
      , fullName = "Bunnelby"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 659 "Bunnelby"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1077
      , nationalDexNumber = 660
      , originalPokemonID = Nothing
      , fullName = "Diggersby"
      , typing = Double Normal Ground
      , ability = Nothing
      , imageUrl = imageUrl 660 "Diggersby"
      , evolutionData = EvolvesFrom [ 1075 ] "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 1078
      , nationalDexNumber = 661
      , originalPokemonID = Nothing
      , fullName = "Fletchling"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrl 661 "Fletchling"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1079
      , nationalDexNumber = 662
      , originalPokemonID = Nothing
      , fullName = "Fletchinder"
      , typing = Double Fire Flying
      , ability = Nothing
      , imageUrl = imageUrl 662 "Fletchinder"
      , evolutionData = EvolvesFrom [ 1078 ] "Level 17"
      , transformationData = DoesNotTransform
      }
    , { id = 1080
      , nationalDexNumber = 663
      , originalPokemonID = Nothing
      , fullName = "Talonflame"
      , typing = Double Fire Flying
      , ability = Nothing
      , imageUrl = imageUrl 663 "Talonflame"
      , evolutionData = EvolvesFrom [ 1079 ] "Level 35"
      , transformationData = DoesNotTransform
      }
    , { id = 1081
      , nationalDexNumber = 664
      , originalPokemonID = Nothing
      , fullName = "Scatterbug"
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imageUrl 664 "Scatterbug"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1082
      , nationalDexNumber = 665
      , originalPokemonID = Nothing
      , fullName = "Spewpa"
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imageUrl 665 "Spewpa"
      , evolutionData = EvolvesFrom [ 1081 ] "Level 9"
      , transformationData = DoesNotTransform
      }
    , { id = 1083
      , nationalDexNumber = 666
      , originalPokemonID = Nothing
      , fullName = "Vivillon"
      , typing = Double Bug Flying
      , ability = Nothing
      , imageUrl = imageUrl 666 "Vivillon"
      , evolutionData = EvolvesFrom [ 1082 ] "Level 12"
      , transformationData = DoesNotTransform
      }
    , { id = 1084
      , nationalDexNumber = 667
      , originalPokemonID = Nothing
      , fullName = "Litleo"
      , typing = Double Fire Normal
      , ability = Nothing
      , imageUrl = imageUrl 667 "Litleo"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1085
      , nationalDexNumber = 668
      , originalPokemonID = Nothing
      , fullName = "Pyroar"
      , typing = Double Fire Normal
      , ability = Nothing
      , imageUrl = imageUrl 668 "Pyroar"
      , evolutionData = EvolvesFrom [ 1084 ] "Level 35"
      , transformationData = DoesNotTransform
      }
    , { id = 1086
      , nationalDexNumber = 669
      , originalPokemonID = Nothing
      , fullName = "Flabébé"
      , typing = Single Fairy
      , ability = Nothing
      , imageUrl = imageUrl 669 "Flabebe"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1088
      , nationalDexNumber = 670
      , originalPokemonID = Nothing
      , fullName = "Floette"
      , typing = Single Fairy
      , ability = Nothing
      , imageUrl = imageUrl 670 "Floette"
      , evolutionData = EvolvesFrom [ 1086 ] "Level 19"
      , transformationData = DoesNotTransform
      }
    , { id = 1089
      , nationalDexNumber = 671
      , originalPokemonID = Nothing
      , fullName = "Florges"
      , typing = Single Fairy
      , ability = Nothing
      , imageUrl = imageUrl 671 "Florges"
      , evolutionData = EvolvesFrom [ 1088 ] "Use Shiny Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 1090
      , nationalDexNumber = 672
      , originalPokemonID = Nothing
      , fullName = "Skiddo"
      , typing = Single Grass
      , ability = Just SapSipper
      , imageUrl = imageUrl 672 "Skiddo"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1091
      , nationalDexNumber = 673
      , originalPokemonID = Nothing
      , fullName = "Gogoat"
      , typing = Single Grass
      , ability = Just SapSipper
      , imageUrl = imageUrl 673 "Gogoat"
      , evolutionData = EvolvesFrom [ 1090 ] "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 1092
      , nationalDexNumber = 674
      , originalPokemonID = Nothing
      , fullName = "Pancham"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrl 674 "Pancham"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1093
      , nationalDexNumber = 675
      , originalPokemonID = Nothing
      , fullName = "Pangoro"
      , typing = Double Fighting Dark
      , ability = Nothing
      , imageUrl = imageUrl 675 "Pangoro"
      , evolutionData = EvolvesFrom [ 1092 ] "Level 32 With Dark-Type Pokemon In Party"
      , transformationData = DoesNotTransform
      }
    , { id = 1094
      , nationalDexNumber = 676
      , originalPokemonID = Nothing
      , fullName = "Furfrou"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 676 "Furfrou"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1095
      , nationalDexNumber = 677
      , originalPokemonID = Nothing
      , fullName = "Espurr"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrl 677 "Espurr"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1096
      , nationalDexNumber = 678
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Meowstic" <| Unique "Male" ""
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrlWithForm 678 "Meowstic" <| Unique "" "Male"
      , evolutionData = EvolvesFrom [ 1095 ] "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 1580
      , nationalDexNumber = 678
      , originalPokemonID = Just 1096
      , fullName = nameWithForm "Meowstic" <| Unique "Female" ""
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrlWithForm 678 "Meowstic" <| Unique "" "Female"
      , evolutionData = EvolvesFrom [ 1095 ] "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 25
      , nationalDexNumber = 679
      , originalPokemonID = Nothing
      , fullName = "Honedge"
      , typing = Double Steel Ghost
      , ability = Nothing
      , imageUrl = imageUrl 679 "Honedge"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 26
      , nationalDexNumber = 680
      , originalPokemonID = Nothing
      , fullName = "Doublade"
      , typing = Double Steel Ghost
      , ability = Nothing
      , imageUrl = imageUrl 680 "Doublade"
      , evolutionData = EvolvesFrom [ 25 ] "Level 35"
      , transformationData = DoesNotTransform
      }
    , { id = 1098
      , nationalDexNumber = 681
      , originalPokemonID = Nothing
      , fullName = "Aegislash"
      , typing = Double Steel Ghost
      , ability = Nothing
      , imageUrl = imageUrlWithForm 681 "Aegislash" <| Unique "" "Shield"
      , evolutionData = EvolvesFrom [ 26 ] "Use Dusk Stone"
      , transformationData = Transforms 16 "Use the move King's Shield"
      }
    , { id = 1370
      , nationalDexNumber = 681
      , originalPokemonID = Just 1098
      , fullName = nameWithForm "Aegislash" <| Unique "" "Blade"
      , typing = Double Steel Ghost
      , ability = Nothing
      , imageUrl = imageUrlWithForm 681 "Aegislash" <| Unique "" "Blade"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 16 "Use a damaging move"
      }
    , { id = 1101
      , nationalDexNumber = 682
      , originalPokemonID = Nothing
      , fullName = "Spritzee"
      , typing = Single Fairy
      , ability = Nothing
      , imageUrl = imageUrl 682 "Spritzee"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1102
      , nationalDexNumber = 683
      , originalPokemonID = Nothing
      , fullName = "Aromatisse"
      , typing = Single Fairy
      , ability = Nothing
      , imageUrl = imageUrl 683 "Aromatisse"
      , evolutionData = EvolvesFrom [ 1101 ] "Trade holding Sachet"
      , transformationData = DoesNotTransform
      }
    , { id = 1103
      , nationalDexNumber = 684
      , originalPokemonID = Nothing
      , fullName = "Swirlix"
      , typing = Single Fairy
      , ability = Nothing
      , imageUrl = imageUrl 684 "Swirlix"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1104
      , nationalDexNumber = 685
      , originalPokemonID = Nothing
      , fullName = "Slurpuff"
      , typing = Single Fairy
      , ability = Nothing
      , imageUrl = imageUrl 685 "Slurpuff"
      , evolutionData = EvolvesFrom [ 1103 ] "Trade holding Whipped Dream"
      , transformationData = DoesNotTransform
      }
    , { id = 1105
      , nationalDexNumber = 686
      , originalPokemonID = Nothing
      , fullName = "Inkay"
      , typing = Double Dark Psychic
      , ability = Nothing
      , imageUrl = imageUrl 686 "Inkay"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1106
      , nationalDexNumber = 687
      , originalPokemonID = Nothing
      , fullName = "Malamar"
      , typing = Double Dark Psychic
      , ability = Nothing
      , imageUrl = imageUrl 687 "Malamar"
      , evolutionData = EvolvesFrom [ 1105 ] "Level 30 With System Or Controller Upside-Down"
      , transformationData = DoesNotTransform
      }
    , { id = 1107
      , nationalDexNumber = 688
      , originalPokemonID = Nothing
      , fullName = "Binacle"
      , typing = Double Rock Water
      , ability = Nothing
      , imageUrl = imageUrl 688 "Binacle"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1108
      , nationalDexNumber = 689
      , originalPokemonID = Nothing
      , fullName = "Barbaracle"
      , typing = Double Rock Water
      , ability = Nothing
      , imageUrl = imageUrl 689 "Barbaracle"
      , evolutionData = EvolvesFrom [ 1107 ] "Level 39"
      , transformationData = DoesNotTransform
      }
    , { id = 1110
      , nationalDexNumber = 690
      , originalPokemonID = Nothing
      , fullName = "Skrelp"
      , typing = Double Poison Water
      , ability = Nothing
      , imageUrl = imageUrl 690 "Skrelp"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1111
      , nationalDexNumber = 691
      , originalPokemonID = Nothing
      , fullName = "Dragalge"
      , typing = Double Poison Dragon
      , ability = Nothing
      , imageUrl = imageUrl 691 "Dragalge"
      , evolutionData = EvolvesFrom [ 1110 ] "Level 48"
      , transformationData = DoesNotTransform
      }
    , { id = 1112
      , nationalDexNumber = 692
      , originalPokemonID = Nothing
      , fullName = "Clauncher"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 692 "Clauncher"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1113
      , nationalDexNumber = 693
      , originalPokemonID = Nothing
      , fullName = "Clawitzer"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 693 "Clawitzer"
      , evolutionData = EvolvesFrom [ 1112 ] "Level 37"
      , transformationData = DoesNotTransform
      }
    , { id = 1122
      , nationalDexNumber = 694
      , originalPokemonID = Nothing
      , fullName = "Helioptile"
      , typing = Double Electric Normal
      , ability = Just DrySkin
      , imageUrl = imageUrl 694 "Helioptile"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1123
      , nationalDexNumber = 695
      , originalPokemonID = Nothing
      , fullName = "Heliolisk"
      , typing = Double Electric Normal
      , ability = Just DrySkin
      , imageUrl = imageUrl 695 "Heliolisk"
      , evolutionData = EvolvesFrom [ 1122 ] "Use Sun Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 1124
      , nationalDexNumber = 696
      , originalPokemonID = Nothing
      , fullName = "Tyrunt"
      , typing = Double Rock Dragon
      , ability = Nothing
      , imageUrl = imageUrl 696 "Tyrunt"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1125
      , nationalDexNumber = 697
      , originalPokemonID = Nothing
      , fullName = "Tyrantrum"
      , typing = Double Rock Dragon
      , ability = Nothing
      , imageUrl = imageUrl 697 "Tyrantrum"
      , evolutionData = EvolvesFrom [ 1124 ] "Level 39 during the day"
      , transformationData = DoesNotTransform
      }
    , { id = 1126
      , nationalDexNumber = 698
      , originalPokemonID = Nothing
      , fullName = "Amaura"
      , typing = Double Rock Ice
      , ability = Nothing
      , imageUrl = imageUrl 698 "Amaura"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1127
      , nationalDexNumber = 699
      , originalPokemonID = Nothing
      , fullName = "Aurorus"
      , typing = Double Rock Ice
      , ability = Nothing
      , imageUrl = imageUrl 699 "Aurorus"
      , evolutionData = EvolvesFrom [ 1126 ] "Level 39 at night"
      , transformationData = DoesNotTransform
      }
    , { id = 1130
      , nationalDexNumber = 701
      , originalPokemonID = Nothing
      , fullName = "Hawlucha"
      , typing = Double Fighting Flying
      , ability = Nothing
      , imageUrl = imageUrl 701 "Hawlucha"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1131
      , nationalDexNumber = 702
      , originalPokemonID = Nothing
      , fullName = "Dedenne"
      , typing = Double Electric Fairy
      , ability = Nothing
      , imageUrl = imageUrl 702 "Dedenne"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1132
      , nationalDexNumber = 703
      , originalPokemonID = Nothing
      , fullName = "Carbink"
      , typing = Double Rock Fairy
      , ability = Nothing
      , imageUrl = imageUrl 703 "Carbink"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1133
      , nationalDexNumber = 704
      , originalPokemonID = Nothing
      , fullName = "Goomy"
      , typing = Single Dragon
      , ability = Just SapSipper
      , imageUrl = imageUrl 704 "Goomy"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1134
      , nationalDexNumber = 705
      , originalPokemonID = Nothing
      , fullName = "Sliggoo"
      , typing = Single Dragon
      , ability = Just SapSipper
      , imageUrl = imageUrl 705 "Sliggoo"
      , evolutionData = EvolvesFrom [ 1133 ] "Level 40"
      , transformationData = DoesNotTransform
      }
    , { id = 1135
      , nationalDexNumber = 706
      , originalPokemonID = Nothing
      , fullName = "Goodra"
      , typing = Single Dragon
      , ability = Just SapSipper
      , imageUrl = imageUrl 706 "Goodra"
      , evolutionData = EvolvesFrom [ 1134 ] "Level 50 When Raining or Foggy outside battle"
      , transformationData = DoesNotTransform
      }
    , { id = 1629
      , nationalDexNumber = 705
      , originalPokemonID = Just 1134
      , fullName = nameWithForm "Sliggoo" Hisuian
      , typing = Double Dragon Steel
      , ability = Just SapSipper
      , imageUrl = imageUrlWithForm 705 "Sliggoo" Hisuian
      , evolutionData = EvolvesFrom [ 1133 ] "Level 40 in Legends: Arceus"
      , transformationData = DoesNotTransform
      }
    , { id = 1630
      , nationalDexNumber = 706
      , originalPokemonID = Just 1135
      , fullName = nameWithForm "Goodra" Hisuian
      , typing = Double Dragon Steel
      , ability = Just SapSipper
      , imageUrl = imageUrlWithForm 706 "Goodra" Hisuian
      , evolutionData = EvolvesFrom [ 1629 ] "Level 50 When Raining or Foggy outside battle"
      , transformationData = DoesNotTransform
      }
    , { id = 1136
      , nationalDexNumber = 707
      , originalPokemonID = Nothing
      , fullName = "Klefki"
      , typing = Double Steel Fairy
      , ability = Nothing
      , imageUrl = imageUrl 707 "Klefki"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1137
      , nationalDexNumber = 708
      , originalPokemonID = Nothing
      , fullName = "Phantump"
      , typing = Double Ghost Grass
      , ability = Nothing
      , imageUrl = imageUrl 708 "Phantump"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1138
      , nationalDexNumber = 709
      , originalPokemonID = Nothing
      , fullName = "Trevenant"
      , typing = Double Ghost Grass
      , ability = Nothing
      , imageUrl = imageUrl 709 "Trevenant"
      , evolutionData = EvolvesFrom [ 1137 ] "Trade"
      , transformationData = DoesNotTransform
      }
    , { id = 1140
      , nationalDexNumber = 710
      , originalPokemonID = Nothing
      , fullName = "Pumpkaboo"
      , typing = Double Ghost Grass
      , ability = Nothing
      , imageUrl = imageUrl 710 "Pumpkaboo"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1141
      , nationalDexNumber = 711
      , originalPokemonID = Nothing
      , fullName = "Gourgeist"
      , typing = Double Ghost Grass
      , ability = Nothing
      , imageUrl = imageUrl 711 "Gourgeist"
      , evolutionData = EvolvesFrom [ 1140 ] "Trade"
      , transformationData = DoesNotTransform
      }
    , { id = 1571
      , nationalDexNumber = 710
      , originalPokemonID = Just 1140
      , fullName = "Pumpkaboo"
      , typing = Double Ghost Grass
      , ability = Nothing
      , imageUrl = imageUrl 710 "Pumpkaboo"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1142
      , nationalDexNumber = 712
      , originalPokemonID = Nothing
      , fullName = "Bergmite"
      , typing = Single Ice
      , ability = Nothing
      , imageUrl = imageUrl 712 "Bergmite"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1143
      , nationalDexNumber = 713
      , originalPokemonID = Nothing
      , fullName = "Avalugg"
      , typing = Single Ice
      , ability = Nothing
      , imageUrl = imageUrl 713 "Avalugg"
      , evolutionData = EvolvesFrom [ 1142 ] "Level 37"
      , transformationData = DoesNotTransform
      }
    , { id = 1631
      , nationalDexNumber = 713
      , originalPokemonID = Just 1143
      , fullName = nameWithForm "Avalugg" Hisuian
      , typing = Double Ice Rock
      , ability = Nothing
      , imageUrl = imageUrlWithForm 713 "Avalugg" Hisuian
      , evolutionData = EvolvesFrom [ 1142 ] "Level 37 in Legends: Arceus"
      , transformationData = DoesNotTransform
      }
    , { id = 1144
      , nationalDexNumber = 714
      , originalPokemonID = Nothing
      , fullName = "Noibat"
      , typing = Double Flying Dragon
      , ability = Nothing
      , imageUrl = imageUrl 714 "Noibat"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1145
      , nationalDexNumber = 715
      , originalPokemonID = Nothing
      , fullName = "Noivern"
      , typing = Double Flying Dragon
      , ability = Nothing
      , imageUrl = imageUrl 715 "Noivern"
      , evolutionData = EvolvesFrom [ 1144 ] "Level 48"
      , transformationData = DoesNotTransform
      }
    , { id = 1146
      , nationalDexNumber = 716
      , originalPokemonID = Nothing
      , fullName = "Xerneas"
      , typing = Single Fairy
      , ability = Nothing
      , imageUrl = imageUrl 716 "Xerneas"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1147
      , nationalDexNumber = 717
      , originalPokemonID = Nothing
      , fullName = "Yveltal"
      , typing = Double Dark Flying
      , ability = Nothing
      , imageUrl = imageUrl 717 "Yveltal"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1577
      , nationalDexNumber = 718
      , originalPokemonID = Just 1148
      , fullName = nameWithForm "Zygarde" <| Unique "" "10%"
      , typing = Double Dragon Ground
      , ability = Nothing
      , imageUrl = imageUrlWithForm 718 "Zygarde" <| Unique "" "10Percent"
      , evolutionData = EvolvesFrom [] "Collect 10% of Zygarde Cells"
      , transformationData = DoesNotTransform
      }
    , { id = 1148
      , nationalDexNumber = 718
      , originalPokemonID = Nothing
      , fullName = "Zygarde"
      , typing = Double Dragon Ground
      , ability = Nothing
      , imageUrl = imageUrl 718 "Zygarde"
      , evolutionData = EvolvesFrom [ 1577 ] "Collect 50% of Zygarde Cells"
      , transformationData = Transforms 17 "At the end of battle"
      }
    , { id = 1578
      , nationalDexNumber = 718
      , originalPokemonID = Just 1148
      , fullName = nameWithForm "Zygarde" <| Unique "" "Complete"
      , typing = Double Dragon Ground
      , ability = Nothing
      , imageUrl = imageUrlWithForm 718 "Zygarde" <| Unique "" "Complete"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 17 "If HP is below half"
      }
    , { id = 1149
      , nationalDexNumber = 719
      , originalPokemonID = Nothing
      , fullName = "Diancie"
      , typing = Double Rock Fairy
      , ability = Nothing
      , imageUrl = imageUrl 719 "Diancie"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1594
      , nationalDexNumber = 719
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Diancie" Mega
      , typing = Double Rock Fairy
      , ability = Nothing
      , imageUrl = imageUrlWithForm 719 "Diancie" Mega
      , evolutionData = EvolvesFrom [ 1149 ] "Holding Diancite"
      , transformationData = DoesNotTransform
      }
    , { id = 1151
      , nationalDexNumber = 720
      , originalPokemonID = Nothing
      , fullName = "Hoopa"
      , typing = Double Psychic Ghost
      , ability = Nothing
      , imageUrl = imageUrl 720 "Hoopa"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 18 "After 3 days"
      }
    , { id = 1579
      , nationalDexNumber = 720
      , originalPokemonID = Just 1151
      , fullName = nameWithForm "Hoopa" <| Unique "" "Unbound"
      , typing = Double Psychic Dark
      , ability = Nothing
      , imageUrl = imageUrlWithForm 720 "Hoopa" <| Unique "" "Unbound"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 18 "Use Prison Bottle"
      }
    , { id = 1152
      , nationalDexNumber = 721
      , originalPokemonID = Nothing
      , fullName = "Volcanion"
      , typing = Double Fire Water
      , ability = Just WaterAbsorb
      , imageUrl = imageUrl 721 "Volcanion"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1246
      , nationalDexNumber = 722
      , originalPokemonID = Nothing
      , fullName = "Rowlet"
      , typing = Double Grass Flying
      , ability = Nothing
      , imageUrl = imageUrl 722 "Rowlet"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1247
      , nationalDexNumber = 723
      , originalPokemonID = Nothing
      , fullName = "Dartrix"
      , typing = Double Grass Flying
      , ability = Nothing
      , imageUrl = imageUrl 723 "Dartrix"
      , evolutionData = EvolvesFrom [ 1246 ] "Level 17"
      , transformationData = DoesNotTransform
      }
    , { id = 1248
      , nationalDexNumber = 724
      , originalPokemonID = Nothing
      , fullName = "Decidueye"
      , typing = Double Grass Ghost
      , ability = Nothing
      , imageUrl = imageUrl 724 "Decidueye"
      , evolutionData = EvolvesFrom [ 1247 ] "Level 34"
      , transformationData = DoesNotTransform
      }
    , { id = 1632
      , nationalDexNumber = 724
      , originalPokemonID = Just 1248
      , fullName = nameWithForm "Decidueye" Hisuian
      , typing = Double Grass Fighting
      , ability = Nothing
      , imageUrl = imageUrlWithForm 724 "Decidueye" Hisuian
      , evolutionData = EvolvesFrom [ 1247 ] "Level 36 in Legends: Arceus"
      , transformationData = DoesNotTransform
      }
    , { id = 1249
      , nationalDexNumber = 725
      , originalPokemonID = Nothing
      , fullName = "Litten"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrl 725 "Litten"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1250
      , nationalDexNumber = 726
      , originalPokemonID = Nothing
      , fullName = "Torracat"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrl 726 "Torracat"
      , evolutionData = EvolvesFrom [ 1249 ] "Level 17"
      , transformationData = DoesNotTransform
      }
    , { id = 1251
      , nationalDexNumber = 727
      , originalPokemonID = Nothing
      , fullName = "Incineroar"
      , typing = Double Fire Dark
      , ability = Nothing
      , imageUrl = imageUrl 727 "Incineroar"
      , evolutionData = EvolvesFrom [ 1250 ] "Level 34"
      , transformationData = DoesNotTransform
      }
    , { id = 1252
      , nationalDexNumber = 728
      , originalPokemonID = Nothing
      , fullName = "Popplio"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 728 "Popplio"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1253
      , nationalDexNumber = 729
      , originalPokemonID = Nothing
      , fullName = "Brionne"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 729 "Brionne"
      , evolutionData = EvolvesFrom [ 1252 ] "Level 17"
      , transformationData = DoesNotTransform
      }
    , { id = 1255
      , nationalDexNumber = 730
      , originalPokemonID = Nothing
      , fullName = "Primarina"
      , typing = Double Water Fairy
      , ability = Nothing
      , imageUrl = imageUrl 730 "Primarina"
      , evolutionData = EvolvesFrom [ 1253 ] "Level 34"
      , transformationData = DoesNotTransform
      }
    , { id = 1256
      , nationalDexNumber = 731
      , originalPokemonID = Nothing
      , fullName = "Pikipek"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrl 731 "Pikipek"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1257
      , nationalDexNumber = 732
      , originalPokemonID = Nothing
      , fullName = "Trumbeak"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrl 732 "Trumbeak"
      , evolutionData = EvolvesFrom [ 1256 ] "Level 14"
      , transformationData = DoesNotTransform
      }
    , { id = 1258
      , nationalDexNumber = 733
      , originalPokemonID = Nothing
      , fullName = "Toucannon"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrl 733 "Toucannon"
      , evolutionData = EvolvesFrom [ 1257 ] "Level 28"
      , transformationData = DoesNotTransform
      }
    , { id = 1259
      , nationalDexNumber = 734
      , originalPokemonID = Nothing
      , fullName = "Yungoos"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 734 "Yungoos"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1260
      , nationalDexNumber = 735
      , originalPokemonID = Nothing
      , fullName = "Gumshoos"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 735 "Gumshoos"
      , evolutionData = EvolvesFrom [ 1259 ] "Level 20 During The Day"
      , transformationData = DoesNotTransform
      }
    , { id = 1261
      , nationalDexNumber = 736
      , originalPokemonID = Nothing
      , fullName = "Grubbin"
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imageUrl 736 "Grubbin"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1262
      , nationalDexNumber = 737
      , originalPokemonID = Nothing
      , fullName = "Charjabug"
      , typing = Double Bug Electric
      , ability = Nothing
      , imageUrl = imageUrl 737 "Charjabug"
      , evolutionData = EvolvesFrom [ 1261 ] "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 1263
      , nationalDexNumber = 738
      , originalPokemonID = Nothing
      , fullName = "Vikavolt"
      , typing = Double Bug Electric
      , ability = Just Levitate
      , imageUrl = imageUrl 738 "Vikavolt"
      , evolutionData = EvolvesFrom [ 1262 ] "Use Thunder Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 1264
      , nationalDexNumber = 739
      , originalPokemonID = Nothing
      , fullName = "Crabrawler"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrl 739 "Crabrawler"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1266
      , nationalDexNumber = 740
      , originalPokemonID = Nothing
      , fullName = "Crabominable"
      , typing = Double Fighting Ice
      , ability = Nothing
      , imageUrl = imageUrl 740 "Crabominable"
      , evolutionData = EvolvesFrom [ 1264 ] "Level at Mount Lanakila in Alola"
      , transformationData = DoesNotTransform
      }
    , { id = 1267
      , nationalDexNumber = 741
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Oricorio" <| Unique "" "Baile"
      , typing = Double Fire Flying
      , ability = Nothing
      , imageUrl = imageUrl 741 "Oricorio"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 19 "Use Red Nectar"
      }
    , { id = 1582
      , nationalDexNumber = 741
      , originalPokemonID = Just 1267
      , fullName = nameWithForm "Oricorio" <| Unique "" "Pom-Pom"
      , typing = Double Electric Flying
      , ability = Nothing
      , imageUrl = imageUrlWithForm 741 "Oricorio" <| Unique "" "Pom-Pom"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 19 "Use Yellow Nectar"
      }
    , { id = 1583
      , nationalDexNumber = 741
      , originalPokemonID = Just 1267
      , fullName = nameWithForm "Oricorio" <| Unique "" "Pa'u"
      , typing = Double Psychic Flying
      , ability = Nothing
      , imageUrl = imageUrlWithForm 741 "Oricorio" <| Unique "" "Pau"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 19 "Use Pink Nectar"
      }
    , { id = 1584
      , nationalDexNumber = 741
      , originalPokemonID = Just 1267
      , fullName = nameWithForm "Oricorio" <| Unique "" "Sensu"
      , typing = Double Ghost Flying
      , ability = Nothing
      , imageUrl = imageUrlWithForm 741 "Oricorio" <| Unique "" "Sensu"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 19 "Use Purple Nectar"
      }
    , { id = 1268
      , nationalDexNumber = 742
      , originalPokemonID = Nothing
      , fullName = "Cutiefly"
      , typing = Double Bug Fairy
      , ability = Nothing
      , imageUrl = imageUrl 742 "Cutiefly"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1269
      , nationalDexNumber = 743
      , originalPokemonID = Nothing
      , fullName = "Ribombee"
      , typing = Double Bug Fairy
      , ability = Nothing
      , imageUrl = imageUrl 743 "Ribombee"
      , evolutionData = EvolvesFrom [ 1268 ] "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 1270
      , nationalDexNumber = 744
      , originalPokemonID = Nothing
      , fullName = "Rockruff"
      , typing = Single Rock
      , ability = Nothing
      , imageUrl = imageUrl 744 "Rockruff"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1271
      , nationalDexNumber = 745
      , originalPokemonID = Nothing
      , fullName = "Lycanroc"
      , typing = Single Rock
      , ability = Nothing
      , imageUrl = imageUrl 745 "Lycanroc"
      , evolutionData = EvolvesFrom [ 1270 ] "Level 25 during the day"
      , transformationData = DoesNotTransform
      }
    , { id = 1403
      , nationalDexNumber = 745
      , originalPokemonID = Just 1271
      , fullName = nameWithForm "Lycanroc" <| Unique "" "Dusk"
      , typing = Single Rock
      , ability = Nothing
      , imageUrl = imageUrlWithForm 745 "Lycanroc" <| Unique "" "Dusk"
      , evolutionData = EvolvesFrom [ 1270 ] "Level 25 with Own Tempo between 5-6pm"
      , transformationData = DoesNotTransform
      }
    , { id = 1402
      , nationalDexNumber = 745
      , originalPokemonID = Just 1271
      , fullName = nameWithForm "Lycanroc" <| Unique "" "Midnight"
      , typing = Single Rock
      , ability = Nothing
      , imageUrl = imageUrlWithForm 745 "Lycanroc" <| Unique "" "Midnight"
      , evolutionData = EvolvesFrom [ 1270 ] "Level 25 at night"
      , transformationData = DoesNotTransform
      }
    , { id = 1272
      , nationalDexNumber = 746
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Wishiwashi" <| Unique "Solo" ""
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 746 "Wishiwashi"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 20 "If HP below 25%"
      }
    , { id = 1581
      , nationalDexNumber = 746
      , originalPokemonID = Just 1272
      , fullName = nameWithForm "Wishiwashi" <| Unique "" "School"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlWithForm 746 "Wishiwashi" <| Unique "" "School"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 20 "If HP above 25% and level 20+"
      }
    , { id = 1273
      , nationalDexNumber = 747
      , originalPokemonID = Nothing
      , fullName = "Mareanie"
      , typing = Double Poison Water
      , ability = Nothing
      , imageUrl = imageUrl 747 "Mareanie"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1274
      , nationalDexNumber = 748
      , originalPokemonID = Nothing
      , fullName = "Toxapex"
      , typing = Double Poison Water
      , ability = Nothing
      , imageUrl = imageUrl 748 "Toxapex"
      , evolutionData = EvolvesFrom [ 1273 ] "Level 38"
      , transformationData = DoesNotTransform
      }
    , { id = 1275
      , nationalDexNumber = 749
      , originalPokemonID = Nothing
      , fullName = "Mudbray"
      , typing = Single Ground
      , ability = Nothing
      , imageUrl = imageUrl 749 "Mudbray"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1277
      , nationalDexNumber = 750
      , originalPokemonID = Nothing
      , fullName = "Mudsdale"
      , typing = Single Ground
      , ability = Nothing
      , imageUrl = imageUrl 750 "Mudsdale"
      , evolutionData = EvolvesFrom [ 1275 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 1278
      , nationalDexNumber = 751
      , originalPokemonID = Nothing
      , fullName = "Dewpider"
      , typing = Double Water Bug
      , ability = Just WaterBubble
      , imageUrl = imageUrl 751 "Dewpider"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1279
      , nationalDexNumber = 752
      , originalPokemonID = Nothing
      , fullName = "Araquanid"
      , typing = Double Water Bug
      , ability = Just WaterBubble
      , imageUrl = imageUrl 752 "Araquanid"
      , evolutionData = EvolvesFrom [ 1278 ] "Level 22"
      , transformationData = DoesNotTransform
      }
    , { id = 1280
      , nationalDexNumber = 753
      , originalPokemonID = Nothing
      , fullName = "Fomantis"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrl 753 "Fomantis"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1281
      , nationalDexNumber = 754
      , originalPokemonID = Nothing
      , fullName = "Lurantis"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrl 754 "Lurantis"
      , evolutionData = EvolvesFrom [ 1280 ] "Level 34 During The Day"
      , transformationData = DoesNotTransform
      }
    , { id = 1282
      , nationalDexNumber = 755
      , originalPokemonID = Nothing
      , fullName = "Morelull"
      , typing = Double Grass Fairy
      , ability = Nothing
      , imageUrl = imageUrl 755 "Morelull"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1283
      , nationalDexNumber = 756
      , originalPokemonID = Nothing
      , fullName = "Shiinotic"
      , typing = Double Grass Fairy
      , ability = Nothing
      , imageUrl = imageUrl 756 "Shiinotic"
      , evolutionData = EvolvesFrom [ 1282 ] "Level 24"
      , transformationData = DoesNotTransform
      }
    , { id = 1284
      , nationalDexNumber = 757
      , originalPokemonID = Nothing
      , fullName = "Salandit"
      , typing = Double Poison Fire
      , ability = Nothing
      , imageUrl = imageUrl 757 "Salandit"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1285
      , nationalDexNumber = 758
      , originalPokemonID = Nothing
      , fullName = "Salazzle"
      , typing = Double Poison Fire
      , ability = Nothing
      , imageUrl = imageUrl 758 "Salazzle"
      , evolutionData = EvolvesFrom [ 1284 ] "Level 33 When Female"
      , transformationData = DoesNotTransform
      }
    , { id = 1286
      , nationalDexNumber = 759
      , originalPokemonID = Nothing
      , fullName = "Stufful"
      , typing = Double Normal Fighting
      , ability = Nothing
      , imageUrl = imageUrl 759 "Stufful"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1288
      , nationalDexNumber = 760
      , originalPokemonID = Nothing
      , fullName = "Bewear"
      , typing = Double Normal Fighting
      , ability = Nothing
      , imageUrl = imageUrl 760 "Bewear"
      , evolutionData = EvolvesFrom [ 1286 ] "Level 27"
      , transformationData = DoesNotTransform
      }
    , { id = 1289
      , nationalDexNumber = 761
      , originalPokemonID = Nothing
      , fullName = "Bounsweet"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrl 761 "Bounsweet"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1290
      , nationalDexNumber = 762
      , originalPokemonID = Nothing
      , fullName = "Steenee"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrl 762 "Steenee"
      , evolutionData = EvolvesFrom [ 1289 ] "Level 18"
      , transformationData = DoesNotTransform
      }
    , { id = 1291
      , nationalDexNumber = 763
      , originalPokemonID = Nothing
      , fullName = "Tsareena"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrl 763 "Tsareena"
      , evolutionData = EvolvesFrom [ 1290 ] "Level while knowing Stomp"
      , transformationData = DoesNotTransform
      }
    , { id = 1292
      , nationalDexNumber = 764
      , originalPokemonID = Nothing
      , fullName = "Comfey"
      , typing = Single Fairy
      , ability = Nothing
      , imageUrl = imageUrl 764 "Comfey"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 743
      , nationalDexNumber = 765
      , originalPokemonID = Nothing
      , fullName = "Oranguru"
      , typing = Double Normal Psychic
      , ability = Nothing
      , imageUrl = imageUrl 765 "Oranguru"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1293
      , nationalDexNumber = 766
      , originalPokemonID = Nothing
      , fullName = "Passimian"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrl 766 "Passimian"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1294
      , nationalDexNumber = 767
      , originalPokemonID = Nothing
      , fullName = "Wimpod"
      , typing = Double Bug Water
      , ability = Nothing
      , imageUrl = imageUrl 767 "Wimpod"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1295
      , nationalDexNumber = 768
      , originalPokemonID = Nothing
      , fullName = "Golisopod"
      , typing = Double Bug Water
      , ability = Nothing
      , imageUrl = imageUrl 768 "Golisopod"
      , evolutionData = EvolvesFrom [ 1294 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 1296
      , nationalDexNumber = 769
      , originalPokemonID = Nothing
      , fullName = "Sandygast"
      , typing = Double Ghost Ground
      , ability = Nothing
      , imageUrl = imageUrl 769 "Sandygast"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1298
      , nationalDexNumber = 770
      , originalPokemonID = Nothing
      , fullName = "Palossand"
      , typing = Double Ghost Ground
      , ability = Nothing
      , imageUrl = imageUrl 770 "Palossand"
      , evolutionData = EvolvesFrom [ 1296 ] "Level 42"
      , transformationData = DoesNotTransform
      }
    , { id = 1299
      , nationalDexNumber = 771
      , originalPokemonID = Nothing
      , fullName = "Pyukumuku"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 771 "Pyukumuku"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1300
      , nationalDexNumber = 772
      , originalPokemonID = Nothing
      , fullName = "Type: Null"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 772 "Type: Null"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1301
      , nationalDexNumber = 773
      , originalPokemonID = Nothing
      , fullName = "Silvally"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 773 "Silvally"
      , evolutionData = EvolvesFrom [ 1300 ] "Level with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 1302
      , nationalDexNumber = 774
      , originalPokemonID = Nothing
      , fullName = "Minior"
      , typing = Double Rock Flying
      , ability = Nothing
      , imageUrl = imageUrl 774 "Minior"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 21 "If HP is above half"
      }
    , { id = 1564
      , nationalDexNumber = 774
      , originalPokemonID = Just 1302
      , fullName = nameWithForm "Minior" <| Unique "" "Core"
      , typing = Double Rock Flying
      , ability = Nothing
      , imageUrl = imageUrlWithForm 774 "Minior" <| Unique "" "Core"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 21 "If HP is below half"
      }
    , { id = 1303
      , nationalDexNumber = 775
      , originalPokemonID = Nothing
      , fullName = "Komala"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 775 "Komala"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1304
      , nationalDexNumber = 776
      , originalPokemonID = Nothing
      , fullName = "Turtonator"
      , typing = Double Fire Dragon
      , ability = Nothing
      , imageUrl = imageUrl 776 "Turtonator"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1305
      , nationalDexNumber = 777
      , originalPokemonID = Nothing
      , fullName = "Togedemaru"
      , typing = Double Electric Steel
      , ability = Just LightningRod
      , imageUrl = imageUrl 777 "Togedemaru"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1306
      , nationalDexNumber = 778
      , originalPokemonID = Nothing
      , fullName = "Mimikyu"
      , typing = Double Ghost Fairy
      , ability = Nothing
      , imageUrl = imageUrl 778 "Mimikyu"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1307
      , nationalDexNumber = 779
      , originalPokemonID = Nothing
      , fullName = "Bruxish"
      , typing = Double Water Psychic
      , ability = Nothing
      , imageUrl = imageUrl 779 "Bruxish"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1309
      , nationalDexNumber = 780
      , originalPokemonID = Nothing
      , fullName = "Drampa"
      , typing = Double Normal Dragon
      , ability = Just SapSipper
      , imageUrl = imageUrl 780 "Drampa"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1310
      , nationalDexNumber = 781
      , originalPokemonID = Nothing
      , fullName = "Dhelmise"
      , typing = Double Ghost Grass
      , ability = Nothing
      , imageUrl = imageUrl 781 "Dhelmise"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1311
      , nationalDexNumber = 782
      , originalPokemonID = Nothing
      , fullName = "Jangmo-o"
      , typing = Single Dragon
      , ability = Nothing
      , imageUrl = imageUrl 782 "Jangmo-o"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1312
      , nationalDexNumber = 783
      , originalPokemonID = Nothing
      , fullName = "Hakamo-o"
      , typing = Double Dragon Fighting
      , ability = Nothing
      , imageUrl = imageUrl 783 "Hakamo-o"
      , evolutionData = EvolvesFrom [ 1311 ] "Level 35"
      , transformationData = DoesNotTransform
      }
    , { id = 1313
      , nationalDexNumber = 784
      , originalPokemonID = Nothing
      , fullName = "Kommo-o"
      , typing = Double Dragon Fighting
      , ability = Nothing
      , imageUrl = imageUrl 784 "Kommo-o"
      , evolutionData = EvolvesFrom [ 1312 ] "Level 45"
      , transformationData = DoesNotTransform
      }
    , { id = 1314
      , nationalDexNumber = 785
      , originalPokemonID = Nothing
      , fullName = "Tapu Koko"
      , typing = Double Electric Fairy
      , ability = Nothing
      , imageUrl = imageUrl 785 "Tapu Koko"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1315
      , nationalDexNumber = 786
      , originalPokemonID = Nothing
      , fullName = "Tapu Lele"
      , typing = Double Psychic Fairy
      , ability = Nothing
      , imageUrl = imageUrl 786 "Tapu Lele"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1316
      , nationalDexNumber = 787
      , originalPokemonID = Nothing
      , fullName = "Tapu Bulu"
      , typing = Double Grass Fairy
      , ability = Nothing
      , imageUrl = imageUrl 787 "Tapu Bulu"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1317
      , nationalDexNumber = 788
      , originalPokemonID = Nothing
      , fullName = "Tapu Fini"
      , typing = Double Water Fairy
      , ability = Nothing
      , imageUrl = imageUrl 788 "Tapu Fini"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1318
      , nationalDexNumber = 789
      , originalPokemonID = Nothing
      , fullName = "Cosmog"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrl 789 "Cosmog"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1320
      , nationalDexNumber = 790
      , originalPokemonID = Nothing
      , fullName = "Cosmoem"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrl 790 "Cosmoem"
      , evolutionData = EvolvesFrom [ 1318 ] "Level 43"
      , transformationData = DoesNotTransform
      }
    , { id = 1321
      , nationalDexNumber = 791
      , originalPokemonID = Nothing
      , fullName = "Solgaleo"
      , typing = Double Psychic Steel
      , ability = Nothing
      , imageUrl = imageUrl 791 "Solgaleo"
      , evolutionData = EvolvesFrom [ 1320 ] "Level 53 In Sun Or Ultra Sun"
      , transformationData = DoesNotTransform
      }
    , { id = 1322
      , nationalDexNumber = 792
      , originalPokemonID = Nothing
      , fullName = "Lunala"
      , typing = Double Psychic Ghost
      , ability = Nothing
      , imageUrl = imageUrl 792 "Lunala"
      , evolutionData = EvolvesFrom [ 1320 ] "Level 53 In Moon Or Ultra Moon"
      , transformationData = DoesNotTransform
      }
    , { id = 1323
      , nationalDexNumber = 793
      , originalPokemonID = Nothing
      , fullName = "Nihilego"
      , typing = Double Rock Poison
      , ability = Nothing
      , imageUrl = imageUrl 793 "Nihilego"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1324
      , nationalDexNumber = 794
      , originalPokemonID = Nothing
      , fullName = "Buzzwole"
      , typing = Double Bug Fighting
      , ability = Nothing
      , imageUrl = imageUrl 794 "Buzzwole"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1325
      , nationalDexNumber = 795
      , originalPokemonID = Nothing
      , fullName = "Pheromosa"
      , typing = Double Bug Fighting
      , ability = Nothing
      , imageUrl = imageUrl 795 "Pheromosa"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1326
      , nationalDexNumber = 796
      , originalPokemonID = Nothing
      , fullName = "Xurkitree"
      , typing = Single Electric
      , ability = Nothing
      , imageUrl = imageUrl 796 "Xurkitree"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1327
      , nationalDexNumber = 797
      , originalPokemonID = Nothing
      , fullName = "Celesteela"
      , typing = Double Steel Flying
      , ability = Nothing
      , imageUrl = imageUrl 797 "Celesteela"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1328
      , nationalDexNumber = 798
      , originalPokemonID = Nothing
      , fullName = "Kartana"
      , typing = Double Grass Steel
      , ability = Nothing
      , imageUrl = imageUrl 798 "Kartana"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1329
      , nationalDexNumber = 799
      , originalPokemonID = Nothing
      , fullName = "Guzzlord"
      , typing = Double Dark Dragon
      , ability = Nothing
      , imageUrl = imageUrl 799 "Guzzlord"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1331
      , nationalDexNumber = 800
      , originalPokemonID = Nothing
      , fullName = "Necrozma"
      , typing = Single Psychic
      , ability = Just PrismArmor
      , imageUrl = imageUrl 800 "Necrozma"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 22 "Undo fusion"
      }
    , { id = 1404
      , nationalDexNumber = 800
      , originalPokemonID = Just 1331
      , fullName = nameWithForm "Necrozma" <| Unique "Dusk Mane" ""
      , typing = Double Psychic Steel
      , ability = Just PrismArmor
      , imageUrl = imageUrlWithForm 800 "Necrozma" <| Unique "" "Dusk_Mane"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 22 "Fuze with Solgaleo"
      }
    , { id = 1405
      , nationalDexNumber = 800
      , originalPokemonID = Just 1331
      , fullName = nameWithForm "Necrozma" <| Unique "Dawn Wings" ""
      , typing = Double Psychic Ghost
      , ability = Just PrismArmor
      , imageUrl = imageUrlWithForm 800 "Necrozma" <| Unique "" "Dawn_Wings"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 22 "Fuze with Lunala"
      }
    , { id = 1406
      , nationalDexNumber = 800
      , originalPokemonID = Just 1331
      , fullName = nameWithForm "Necrozma" <| Unique "Ultra" ""
      , typing = Double Psychic Dragon
      , ability = Nothing
      , imageUrl = imageUrlWithForm 800 "Necrozma" <| Unique "" "Ultra"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 22 "Use Ultra Burst"
      }
    , { id = 1332
      , nationalDexNumber = 801
      , originalPokemonID = Nothing
      , fullName = "Magearna"
      , typing = Double Steel Fairy
      , ability = Nothing
      , imageUrl = imageUrl 801 "Magearna"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1333
      , nationalDexNumber = 802
      , originalPokemonID = Nothing
      , fullName = "Marshadow"
      , typing = Double Fighting Ghost
      , ability = Nothing
      , imageUrl = imageUrl 802 "Marshadow"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1334
      , nationalDexNumber = 803
      , originalPokemonID = Nothing
      , fullName = "Poipole"
      , typing = Single Poison
      , ability = Nothing
      , imageUrl = imageUrl 803 "Poipole"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1335
      , nationalDexNumber = 804
      , originalPokemonID = Nothing
      , fullName = "Naganadel"
      , typing = Double Poison Dragon
      , ability = Nothing
      , imageUrl = imageUrl 804 "Naganadel"
      , evolutionData = EvolvesFrom [ 1334 ] "Level while knowing Dragon Pulse"
      , transformationData = DoesNotTransform
      }
    , { id = 1336
      , nationalDexNumber = 805
      , originalPokemonID = Nothing
      , fullName = "Stakataka"
      , typing = Double Rock Steel
      , ability = Nothing
      , imageUrl = imageUrl 805 "Stakataka"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1337
      , nationalDexNumber = 806
      , originalPokemonID = Nothing
      , fullName = "Blacephalon"
      , typing = Double Fire Ghost
      , ability = Nothing
      , imageUrl = imageUrl 806 "Blacephalon"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1338
      , nationalDexNumber = 807
      , originalPokemonID = Nothing
      , fullName = "Zeraora"
      , typing = Single Electric
      , ability = Just VoltAbsorb
      , imageUrl = imageUrl 807 "Zeraora"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1339
      , nationalDexNumber = 808
      , originalPokemonID = Nothing
      , fullName = "Meltan"
      , typing = Single Steel
      , ability = Nothing
      , imageUrl = imageUrl 808 "Meltan"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1340
      , nationalDexNumber = 809
      , originalPokemonID = Nothing
      , fullName = "Melmetal"
      , typing = Single Steel
      , ability = Nothing
      , imageUrl = imageUrl 809 "Melmetal"
      , evolutionData = EvolvesFrom [ 1339 ] "400 Meltan Candy (Pokemon GO only)"
      , transformationData = DoesNotTransform
      }
    , { id = 1439
      , nationalDexNumber = 810
      , originalPokemonID = Nothing
      , fullName = "Grookey"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrl 810 "Grookey"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1440
      , nationalDexNumber = 811
      , originalPokemonID = Nothing
      , fullName = "Thwackey"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrl 811 "Thwackey"
      , evolutionData = EvolvesFrom [ 1439 ] "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 1441
      , nationalDexNumber = 812
      , originalPokemonID = Nothing
      , fullName = "Rillaboom"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrl 812 "Rillaboom"
      , evolutionData = EvolvesFrom [ 1440 ] "Level 35"
      , transformationData = DoesNotTransform
      }
    , { id = 1442
      , nationalDexNumber = 813
      , originalPokemonID = Nothing
      , fullName = "Scorbunny"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrl 813 "Scorbunny"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1443
      , nationalDexNumber = 814
      , originalPokemonID = Nothing
      , fullName = "Raboot"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrl 814 "Raboot"
      , evolutionData = EvolvesFrom [ 1442 ] "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 1444
      , nationalDexNumber = 815
      , originalPokemonID = Nothing
      , fullName = "Cinderace"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrl 815 "Cinderace"
      , evolutionData = EvolvesFrom [ 1443 ] "Level 35"
      , transformationData = DoesNotTransform
      }
    , { id = 1445
      , nationalDexNumber = 816
      , originalPokemonID = Nothing
      , fullName = "Sobble"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 816 "Sobble"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1446
      , nationalDexNumber = 817
      , originalPokemonID = Nothing
      , fullName = "Drizzile"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 817 "Drizzile"
      , evolutionData = EvolvesFrom [ 1445 ] "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 1447
      , nationalDexNumber = 818
      , originalPokemonID = Nothing
      , fullName = "Inteleon"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 818 "Inteleon"
      , evolutionData = EvolvesFrom [ 1446 ] "Level 35"
      , transformationData = DoesNotTransform
      }
    , { id = 1462
      , nationalDexNumber = 819
      , originalPokemonID = Nothing
      , fullName = "Skwovet"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 819 "Skwovet"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1463
      , nationalDexNumber = 820
      , originalPokemonID = Nothing
      , fullName = "Greedent"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 820 "Greedent"
      , evolutionData = EvolvesFrom [ 1462 ] "Level 24"
      , transformationData = DoesNotTransform
      }
    , { id = 1469
      , nationalDexNumber = 821
      , originalPokemonID = Nothing
      , fullName = "Rookidee"
      , typing = Single Flying
      , ability = Nothing
      , imageUrl = imageUrl 821 "Rookidee"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1470
      , nationalDexNumber = 822
      , originalPokemonID = Nothing
      , fullName = "Corvisquire"
      , typing = Single Flying
      , ability = Nothing
      , imageUrl = imageUrl 822 "Corvisquire"
      , evolutionData = EvolvesFrom [ 1469 ] "Level 18"
      , transformationData = DoesNotTransform
      }
    , { id = 1471
      , nationalDexNumber = 823
      , originalPokemonID = Nothing
      , fullName = "Corviknight"
      , typing = Double Flying Steel
      , ability = Nothing
      , imageUrl = imageUrl 823 "Corviknight"
      , evolutionData = EvolvesFrom [ 1470 ] "Level 38"
      , transformationData = DoesNotTransform
      }
    , { id = 1485
      , nationalDexNumber = 824
      , originalPokemonID = Nothing
      , fullName = "Blipbug"
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imageUrl 824 "Blipbug"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1486
      , nationalDexNumber = 825
      , originalPokemonID = Nothing
      , fullName = "Dottler"
      , typing = Double Bug Psychic
      , ability = Nothing
      , imageUrl = imageUrl 825 "Dottler"
      , evolutionData = EvolvesFrom [ 1485 ] "Level 10"
      , transformationData = DoesNotTransform
      }
    , { id = 1487
      , nationalDexNumber = 826
      , originalPokemonID = Nothing
      , fullName = "Orbeetle"
      , typing = Double Bug Psychic
      , ability = Nothing
      , imageUrl = imageUrl 826 "Orbeetle"
      , evolutionData = EvolvesFrom [ 1486 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 1488
      , nationalDexNumber = 827
      , originalPokemonID = Nothing
      , fullName = "Nickit"
      , typing = Single Dark
      , ability = Nothing
      , imageUrl = imageUrl 827 "Nickit"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1489
      , nationalDexNumber = 828
      , originalPokemonID = Nothing
      , fullName = "Thievul"
      , typing = Single Dark
      , ability = Nothing
      , imageUrl = imageUrl 828 "Thievul"
      , evolutionData = EvolvesFrom [ 1488 ] "Level 18"
      , transformationData = DoesNotTransform
      }
    , { id = 1490
      , nationalDexNumber = 829
      , originalPokemonID = Nothing
      , fullName = "Gossifleur"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrl 829 "Gossifleur"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1491
      , nationalDexNumber = 830
      , originalPokemonID = Nothing
      , fullName = "Eldegoss"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrl 830 "Eldegoss"
      , evolutionData = EvolvesFrom [ 1490 ] "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 1492
      , nationalDexNumber = 831
      , originalPokemonID = Nothing
      , fullName = "Wooloo"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 831 "Wooloo"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1493
      , nationalDexNumber = 832
      , originalPokemonID = Nothing
      , fullName = "Dubwool"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 832 "Dubwool"
      , evolutionData = EvolvesFrom [ 1492 ] "Level 24"
      , transformationData = DoesNotTransform
      }
    , { id = 1494
      , nationalDexNumber = 833
      , originalPokemonID = Nothing
      , fullName = "Chewtle"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 833 "Chewtle"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1495
      , nationalDexNumber = 834
      , originalPokemonID = Nothing
      , fullName = "Drednaw"
      , typing = Double Water Rock
      , ability = Nothing
      , imageUrl = imageUrl 834 "Drednaw"
      , evolutionData = EvolvesFrom [ 1494 ] "Level 22"
      , transformationData = DoesNotTransform
      }
    , { id = 1496
      , nationalDexNumber = 835
      , originalPokemonID = Nothing
      , fullName = "Yamper"
      , typing = Single Electric
      , ability = Nothing
      , imageUrl = imageUrl 835 "Yamper"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1497
      , nationalDexNumber = 836
      , originalPokemonID = Nothing
      , fullName = "Boltund"
      , typing = Single Electric
      , ability = Nothing
      , imageUrl = imageUrl 836 "Boltund"
      , evolutionData = EvolvesFrom [ 1496 ] "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 1498
      , nationalDexNumber = 837
      , originalPokemonID = Nothing
      , fullName = "Rolycoly"
      , typing = Single Rock
      , ability = Nothing
      , imageUrl = imageUrl 837 "Rolycoly"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1499
      , nationalDexNumber = 838
      , originalPokemonID = Nothing
      , fullName = "Carkol"
      , typing = Double Rock Fire
      , ability = Nothing
      , imageUrl = imageUrl 838 "Carkol"
      , evolutionData = EvolvesFrom [ 1498 ] "Level 18"
      , transformationData = DoesNotTransform
      }
    , { id = 1500
      , nationalDexNumber = 839
      , originalPokemonID = Nothing
      , fullName = "Coalossal"
      , typing = Double Rock Fire
      , ability = Nothing
      , imageUrl = imageUrl 839 "Coalossal"
      , evolutionData = EvolvesFrom [ 1499 ] "Level 34"
      , transformationData = DoesNotTransform
      }
    , { id = 1501
      , nationalDexNumber = 840
      , originalPokemonID = Nothing
      , fullName = "Applin"
      , typing = Double Grass Dragon
      , ability = Nothing
      , imageUrl = imageUrl 840 "Applin"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1502
      , nationalDexNumber = 841
      , originalPokemonID = Nothing
      , fullName = "Flapple"
      , typing = Double Grass Dragon
      , ability = Nothing
      , imageUrl = imageUrl 841 "Flapple"
      , evolutionData = EvolvesFrom [ 1501 ] "Use Tart Apple"
      , transformationData = DoesNotTransform
      }
    , { id = 1503
      , nationalDexNumber = 842
      , originalPokemonID = Nothing
      , fullName = "Appletun"
      , typing = Double Grass Dragon
      , ability = Nothing
      , imageUrl = imageUrl 842 "Appletun"
      , evolutionData = EvolvesFrom [ 1501 ] "Use Sweet Apple"
      , transformationData = DoesNotTransform
      }
    , { id = 1504
      , nationalDexNumber = 843
      , originalPokemonID = Nothing
      , fullName = "Silicobra"
      , typing = Single Ground
      , ability = Nothing
      , imageUrl = imageUrl 843 "Silicobra"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1505
      , nationalDexNumber = 844
      , originalPokemonID = Nothing
      , fullName = "Sandaconda"
      , typing = Single Ground
      , ability = Nothing
      , imageUrl = imageUrl 844 "Sandaconda"
      , evolutionData = EvolvesFrom [ 1504 ] "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 1506
      , nationalDexNumber = 845
      , originalPokemonID = Nothing
      , fullName = "Cramorant"
      , typing = Double Flying Water
      , ability = Nothing
      , imageUrl = imageUrl 845 "Cramorant"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1507
      , nationalDexNumber = 846
      , originalPokemonID = Nothing
      , fullName = "Arrokuda"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 846 "Arrokuda"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1508
      , nationalDexNumber = 847
      , originalPokemonID = Nothing
      , fullName = "Barraskewda"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 847 "Barraskewda"
      , evolutionData = EvolvesFrom [ 1507 ] "Level 26"
      , transformationData = DoesNotTransform
      }
    , { id = 14
      , nationalDexNumber = 848
      , originalPokemonID = Nothing
      , fullName = "Toxel"
      , typing = Double Electric Poison
      , ability = Nothing
      , imageUrl = imageUrl 848 "Toxel"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 15
      , nationalDexNumber = 849
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Toxtricity" <| Unique "Amped" ""
      , typing = Double Electric Poison
      , ability = Nothing
      , imageUrl = imageUrlWithForm 849 "Toxtricity" <| Unique "" "Amped"
      , evolutionData = EvolvesFrom [ 14 ] "Level 30 With An Adamant, Brave, Docile, Hardy, Hasty, Impish, Jolly, Lax, Naive, Naughty, Quirky, Rash, or Sassy Nature"
      , transformationData = DoesNotTransform
      }
    , { id = 16
      , nationalDexNumber = 849
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Toxtricity" <| Unique "Low Key" ""
      , typing = Double Electric Poison
      , ability = Nothing
      , imageUrl = imageUrlWithForm 849 "Toxtricity" <| Unique "" "Low_Key"
      , evolutionData = EvolvesFrom [ 14 ] "Level 30 With A Bashful, Bold, Calm, Careful, Gentle, Lonely, Mild, Modest, Quiet, Relaxed, Serious, or Timid Nature"
      , transformationData = DoesNotTransform
      }
    , { id = 1509
      , nationalDexNumber = 850
      , originalPokemonID = Nothing
      , fullName = "Sizzlipede"
      , typing = Double Fire Bug
      , ability = Just FlashFire
      , imageUrl = imageUrl 850 "Sizzlipede"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1510
      , nationalDexNumber = 851
      , originalPokemonID = Nothing
      , fullName = "Centiskorch"
      , typing = Double Fire Bug
      , ability = Just FlashFire
      , imageUrl = imageUrl 851 "Centiskorch"
      , evolutionData = EvolvesFrom [ 1509 ] "Level 28"
      , transformationData = DoesNotTransform
      }
    , { id = 1511
      , nationalDexNumber = 852
      , originalPokemonID = Nothing
      , fullName = "Clobbopus"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrl 852 "Clobbopus"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1512
      , nationalDexNumber = 853
      , originalPokemonID = Nothing
      , fullName = "Grapploct"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrl 853 "Grapploct"
      , evolutionData = EvolvesFrom [ 1511 ] "Level while knowing Taunt"
      , transformationData = DoesNotTransform
      }
    , { id = 1513
      , nationalDexNumber = 854
      , originalPokemonID = Nothing
      , fullName = "Sinistea"
      , typing = Single Ghost
      , ability = Nothing
      , imageUrl = imageUrl 854 "Sinistea"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1514
      , nationalDexNumber = 855
      , originalPokemonID = Nothing
      , fullName = "Polteageist"
      , typing = Single Ghost
      , ability = Nothing
      , imageUrl = imageUrl 855 "Polteageist"
      , evolutionData = EvolvesFrom [ 1513 ] "Use Cracked Pot when Phony or Chipped Pot when Authentic"
      , transformationData = DoesNotTransform
      }
    , { id = 1515
      , nationalDexNumber = 856
      , originalPokemonID = Nothing
      , fullName = "Hatenna"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrl 856 "Hatenna"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1516
      , nationalDexNumber = 857
      , originalPokemonID = Nothing
      , fullName = "Hattrem"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrl 857 "Hattrem"
      , evolutionData = EvolvesFrom [ 1515 ] "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 1517
      , nationalDexNumber = 858
      , originalPokemonID = Nothing
      , fullName = "Hatterene"
      , typing = Double Psychic Fairy
      , ability = Nothing
      , imageUrl = imageUrl 858 "Hatterene"
      , evolutionData = EvolvesFrom [ 1516 ] "Level 42"
      , transformationData = DoesNotTransform
      }
    , { id = 1518
      , nationalDexNumber = 859
      , originalPokemonID = Nothing
      , fullName = "Impidimp"
      , typing = Double Dark Fairy
      , ability = Nothing
      , imageUrl = imageUrl 859 "Impidimp"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1519
      , nationalDexNumber = 860
      , originalPokemonID = Nothing
      , fullName = "Morgrem"
      , typing = Double Dark Fairy
      , ability = Nothing
      , imageUrl = imageUrl 860 "Morgrem"
      , evolutionData = EvolvesFrom [ 1518 ] "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 1520
      , nationalDexNumber = 861
      , originalPokemonID = Nothing
      , fullName = "Grimmsnarl"
      , typing = Double Dark Fairy
      , ability = Nothing
      , imageUrl = imageUrl 861 "Grimmsnarl"
      , evolutionData = EvolvesFrom [ 1519 ] "Level 42"
      , transformationData = DoesNotTransform
      }
    , { id = 1483
      , nationalDexNumber = 868
      , originalPokemonID = Nothing
      , fullName = "Milcery"
      , typing = Single Fairy
      , ability = Nothing
      , imageUrl = imageUrl 868 "Milcery"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1484
      , nationalDexNumber = 869
      , originalPokemonID = Nothing
      , fullName = "Alcremie"
      , typing = Single Fairy
      , ability = Nothing
      , imageUrl = imageUrl 869 "Alcremie"
      , evolutionData = EvolvesFrom [ 1483 ] "While holding a Sweet and its Trainer spins"
      , transformationData = DoesNotTransform
      }
    , { id = 1378
      , nationalDexNumber = 870
      , originalPokemonID = Nothing
      , fullName = "Falinks"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrl 870 "Falinks"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1425
      , nationalDexNumber = 871
      , originalPokemonID = Nothing
      , fullName = "Pincurchin"
      , typing = Single Electric
      , ability = Just LightningRod
      , imageUrl = imageUrl 871 "Pincurchin"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1481
      , nationalDexNumber = 872
      , originalPokemonID = Nothing
      , fullName = "Snom"
      , typing = Double Ice Bug
      , ability = Nothing
      , imageUrl = imageUrl 872 "Snom"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1482
      , nationalDexNumber = 873
      , originalPokemonID = Nothing
      , fullName = "Frosmoth"
      , typing = Double Ice Bug
      , ability = Nothing
      , imageUrl = imageUrl 873 "Frosmoth"
      , evolutionData = EvolvesFrom [ 1481 ] "Level during the night with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 1412
      , nationalDexNumber = 874
      , originalPokemonID = Nothing
      , fullName = "Stonjourner"
      , typing = Single Rock
      , ability = Nothing
      , imageUrl = imageUrl 874 "Stonjourner"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1479
      , nationalDexNumber = 875
      , originalPokemonID = Nothing
      , fullName = "Eiscue"
      , typing = Single Ice
      , ability = Nothing
      , imageUrl = imageUrl 875 "Eiscue"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 23 "When a hailstorm starts"
      }
    , { id = 1480
      , nationalDexNumber = 875
      , originalPokemonID = Just 1479
      , fullName = nameWithForm "Eiscue" <| Unique "" "Noice"
      , typing = Single Ice
      , ability = Nothing
      , imageUrl = imageUrlWithForm 875 "Eiscue" <| Unique "" "Noice"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 23 "When hit by a physical move"
      }
    , { id = 1477
      , nationalDexNumber = 876
      , originalPokemonID = Nothing
      , fullName = nameWithForm "Indeedee" <| Unique "" "Male"
      , typing = Double Psychic Normal
      , ability = Nothing
      , imageUrl = imageUrlWithForm 876 "Indeedee" <| Unique "" "Male"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1478
      , nationalDexNumber = 876
      , originalPokemonID = Just 1477
      , fullName = nameWithForm "Indeedee" <| Unique "" "Female"
      , typing = Double Psychic Normal
      , ability = Nothing
      , imageUrl = imageUrlWithForm 876 "Indeedee" <| Unique "" "Female"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1426
      , nationalDexNumber = 877
      , originalPokemonID = Nothing
      , fullName = "Morpeko"
      , typing = Double Electric Dark
      , ability = Nothing
      , imageUrl = imageUrlWithForm 877 "Morpeko" <| Unique "" "Full"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1475
      , nationalDexNumber = 878
      , originalPokemonID = Nothing
      , fullName = "Cufant"
      , typing = Single Steel
      , ability = Nothing
      , imageUrl = imageUrl 878 "Cufant"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1476
      , nationalDexNumber = 879
      , originalPokemonID = Nothing
      , fullName = "Copperajah"
      , typing = Single Steel
      , ability = Nothing
      , imageUrl = imageUrl 879 "Copperajah"
      , evolutionData = EvolvesFrom [ 1475 ] "Level 34"
      , transformationData = DoesNotTransform
      }
    , { id = 1418
      , nationalDexNumber = 880
      , originalPokemonID = Nothing
      , fullName = "Dracozolt"
      , typing = Double Electric Dragon
      , ability = Just VoltAbsorb
      , imageUrl = imageUrl 880 "Dracozolt"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1420
      , nationalDexNumber = 881
      , originalPokemonID = Nothing
      , fullName = "Arctozolt"
      , typing = Double Electric Ice
      , ability = Just VoltAbsorb
      , imageUrl = imageUrl 881 "Arctozolt"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1419
      , nationalDexNumber = 882
      , originalPokemonID = Nothing
      , fullName = "Dracovish"
      , typing = Double Water Dragon
      , ability = Just WaterAbsorb
      , imageUrl = imageUrl 882 "Dracovish"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1422
      , nationalDexNumber = 883
      , originalPokemonID = Nothing
      , fullName = "Arctovish"
      , typing = Double Water Ice
      , ability = Just WaterAbsorb
      , imageUrl = imageUrl 883 "Arctovish"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1424
      , nationalDexNumber = 884
      , originalPokemonID = Nothing
      , fullName = "Duraludon"
      , typing = Double Steel Dragon
      , ability = Nothing
      , imageUrl = imageUrl 884 "Duraludon"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1472
      , nationalDexNumber = 885
      , originalPokemonID = Nothing
      , fullName = "Dreepy"
      , typing = Double Dragon Ghost
      , ability = Nothing
      , imageUrl = imageUrl 885 "Dreepy"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1473
      , nationalDexNumber = 886
      , originalPokemonID = Nothing
      , fullName = "Drakloak"
      , typing = Double Dragon Ghost
      , ability = Nothing
      , imageUrl = imageUrl 886 "Drakloak"
      , evolutionData = EvolvesFrom [ 1472 ] "Level 50"
      , transformationData = DoesNotTransform
      }
    , { id = 1474
      , nationalDexNumber = 887
      , originalPokemonID = Nothing
      , fullName = "Dragapult"
      , typing = Double Dragon Ghost
      , ability = Nothing
      , imageUrl = imageUrl 887 "Dragapult"
      , evolutionData = EvolvesFrom [ 1473 ] "Level 60"
      , transformationData = DoesNotTransform
      }
    , { id = 1448
      , nationalDexNumber = 888
      , originalPokemonID = Nothing
      , fullName = "Zacian"
      , typing = Single Fairy
      , ability = Nothing
      , imageUrl = imageUrl 888 "Zacian"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 24 "If not holding Rusted Sword"
      }
    , { id = 1449
      , nationalDexNumber = 888
      , originalPokemonID = Just 1448
      , fullName = nameWithForm "Zacian" <| Unique "Crowned" ""
      , typing = Double Fairy Steel
      , ability = Nothing
      , imageUrl = imageUrlWithForm 888 "Zacian" <| Unique "" "Hero"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 24 "While holding Rusted Sword"
      }
    , { id = 1450
      , nationalDexNumber = 889
      , originalPokemonID = Nothing
      , fullName = "Zamazenta"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrl 889 "Zamazenta"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 25 "If not holding Rusted Shield"
      }
    , { id = 1451
      , nationalDexNumber = 889
      , originalPokemonID = Just 1450
      , fullName = nameWithForm "Zamazenta" <| Unique "Crowned" ""
      , typing = Double Fighting Steel
      , ability = Nothing
      , imageUrl = imageUrlWithForm 889 "Zamazenta" <| Unique "" "Hero"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 25 "While holding Rusted Shield"
      }
    , { id = 1452
      , nationalDexNumber = 890
      , originalPokemonID = Nothing
      , fullName = "Eternatus"
      , typing = Double Poison Dragon
      , ability = Nothing
      , imageUrl = imageUrl 890 "Eternatus"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 26 "When owned by the player"
      }
    , { id = 1461
      , nationalDexNumber = 890
      , originalPokemonID = Just 1452
      , fullName = nameWithForm "Eternatus" <| Unique "Eternamax" ""
      , typing = Double Poison Dragon
      , ability = Nothing
      , imageUrl = imageUrlWithForm 890 "Eternatus" <| Unique "" "Eternamax"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 26 "During the final battle in Pokémon Sword and Shield"
      }
    , { id = 38
      , nationalDexNumber = 891
      , originalPokemonID = Nothing
      , fullName = "Kubfu"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrl 891 "Kubfu"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 39
      , nationalDexNumber = 892
      , originalPokemonID = Nothing
      , fullName = "Urshifu"
      , typing = Double Fighting Dark
      , ability = Nothing
      , imageUrl = imageUrlWithForm 892 "Urshifu" <| Unique "" "Single_Strike"
      , evolutionData = EvolvesFrom [ 38 ] "Conquer the Tower of Darkness in Galar's Isle of Armor"
      , transformationData = DoesNotTransform
      }
    , { id = 40
      , nationalDexNumber = 892
      , originalPokemonID = Just 39
      , fullName = nameWithForm "Urshifu" <| Unique "Rapid Strike" ""
      , typing = Double Fighting Water
      , ability = Nothing
      , imageUrl = imageUrlWithForm 892 "Urshifu" <| Unique "" "Rapid_Strike"
      , evolutionData = EvolvesFrom [ 38 ] "Conquer the Tower of Waters in Galar's Isle of Armor"
      , transformationData = DoesNotTransform
      }
    , { id = 1597
      , nationalDexNumber = 893
      , originalPokemonID = Nothing
      , fullName = "Zarude"
      , typing = Double Dark Grass
      , ability = Nothing
      , imageUrl = imageUrl 893 "Zarude"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1598
      , nationalDexNumber = 894
      , originalPokemonID = Nothing
      , fullName = "Regieleki"
      , typing = Single Electric
      , ability = Nothing
      , imageUrl = imageUrl 894 "Regieleki"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1599
      , nationalDexNumber = 895
      , originalPokemonID = Nothing
      , fullName = "Regidrago"
      , typing = Single Dragon
      , ability = Nothing
      , imageUrl = imageUrl 895 "Regidrago"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1602
      , nationalDexNumber = 896
      , originalPokemonID = Nothing
      , fullName = "Glastrier"
      , typing = Single Ice
      , ability = Nothing
      , imageUrl = imageUrl 896 "Glastrier"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1603
      , nationalDexNumber = 897
      , originalPokemonID = Nothing
      , fullName = "Spectrier"
      , typing = Single Ghost
      , ability = Nothing
      , imageUrl = imageUrl 897 "Spectrier"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 41
      , nationalDexNumber = 898
      , originalPokemonID = Nothing
      , fullName = "Calyrex"
      , typing = Double Psychic Grass
      , ability = Nothing
      , imageUrl = imageUrl 898 "Calyrex"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 27 "Undo fusion"
      }
    , { id = 1604
      , nationalDexNumber = 898
      , originalPokemonID = Just 41
      , fullName = nameWithForm "Calyrex" <| Unique "Ice Rider" ""
      , typing = Double Psychic Ice
      , ability = Nothing
      , imageUrl = imageUrlWithForm 898 "Calyrex" <| Unique "" "Ice_Rider"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 27 "Fuse with Glastrier"
      }
    , { id = 1605
      , nationalDexNumber = 898
      , originalPokemonID = Just 41
      , fullName = nameWithForm "Calyrex" <| Unique "Shadow Rider" ""
      , typing = Double Psychic Ghost
      , ability = Nothing
      , imageUrl = imageUrlWithForm 898 "Calyrex" <| Unique "" "Shadow_Rider"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 27 "Fuse with Spectrier"
      }
    , { id = 1617
      , nationalDexNumber = 905
      , originalPokemonID = Nothing
      , fullName = "Enamorus"
      , typing = Double Fairy Flying
      , ability = Nothing
      , imageUrl = imageUrl 905 "Enamorus"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 28 "Use the Reveal Glass"
      }
    , { id = 1618
      , nationalDexNumber = 905
      , originalPokemonID = Just 1617
      , fullName = nameWithForm "Enamorus" <| Unique "Therian" ""
      , typing = Double Fairy Flying
      , ability = Nothing
      , imageUrl = imageUrlWithForm 905 "Enamorus" <| Unique "" "Therian"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 28 "Use the Reveal Glass"
      }
    , { id = 1633
      , nationalDexNumber = 906
      , originalPokemonID = Nothing
      , fullName = "Sprigatito"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrl 906 "Sprigatito"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1634
      , nationalDexNumber = 907
      , originalPokemonID = Nothing
      , fullName = "Floragato"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrl 907 "Floragato"
      , evolutionData = EvolvesFrom [ 1633 ] "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 1635
      , nationalDexNumber = 908
      , originalPokemonID = Nothing
      , fullName = "Meowscarada"
      , typing = Double Grass Dark
      , ability = Nothing
      , imageUrl = imageUrl 908 "Meowscarada"
      , evolutionData = EvolvesFrom [ 1634 ] "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 1636
      , nationalDexNumber = 909
      , originalPokemonID = Nothing
      , fullName = "Fuecoco"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrl 909 "Fuecoco"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1637
      , nationalDexNumber = 910
      , originalPokemonID = Nothing
      , fullName = "Crocalor"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrl 910 "Crocalor"
      , evolutionData = EvolvesFrom [ 1636 ] "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 1638
      , nationalDexNumber = 911
      , originalPokemonID = Nothing
      , fullName = "Skeledirge"
      , typing = Double Fire Ghost
      , ability = Nothing
      , imageUrl = imageUrl 911 "Skeledirge"
      , evolutionData = EvolvesFrom [ 1637 ] "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 1639
      , nationalDexNumber = 912
      , originalPokemonID = Nothing
      , fullName = "Quaxly"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 912 "Quaxly"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1640
      , nationalDexNumber = 913
      , originalPokemonID = Nothing
      , fullName = "Quaxwell"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 913 "Quaxwell"
      , evolutionData = EvolvesFrom [ 1639 ] "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 1641
      , nationalDexNumber = 914
      , originalPokemonID = Nothing
      , fullName = "Quaquaval"
      , typing = Double Water Fighting
      , ability = Nothing
      , imageUrl = imageUrl 914 "Quaquaval"
      , evolutionData = EvolvesFrom [ 1640 ] "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 1642
      , nationalDexNumber = 915
      , originalPokemonID = Nothing
      , fullName = "Lechonk"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 915 "Lechonk"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1643
      , nationalDexNumber = 916
      , originalPokemonID = Nothing
      , fullName = "Oinkologne"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 916 "Oinkologne"
      , evolutionData = EvolvesFrom [ 1642 ] "Level 18"
      , transformationData = DoesNotTransform
      }
    , { id = 1644
      , nationalDexNumber = 917
      , originalPokemonID = Nothing
      , fullName = "Tarountula"
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imageUrl 917 "Tarountula"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1645
      , nationalDexNumber = 918
      , originalPokemonID = Nothing
      , fullName = "Spidops"
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imageUrl 918 "Spidops"
      , evolutionData = EvolvesFrom [ 1644 ] "Level 15"
      , transformationData = DoesNotTransform
      }
    , { id = 1646
      , nationalDexNumber = 919
      , originalPokemonID = Nothing
      , fullName = "Nymble"
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imageUrl 919 "Nymble"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1647
      , nationalDexNumber = 920
      , originalPokemonID = Nothing
      , fullName = "Lokix"
      , typing = Double Bug Dark
      , ability = Nothing
      , imageUrl = imageUrl 920 "Lokix"
      , evolutionData = EvolvesFrom [ 1646 ] "Level 24"
      , transformationData = DoesNotTransform
      }
    , { id = 1648
      , nationalDexNumber = 921
      , originalPokemonID = Nothing
      , fullName = "Pawmi"
      , typing = Single Electric
      , ability = Nothing
      , imageUrl = imageUrl 921 "Pawmi"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1649
      , nationalDexNumber = 922
      , originalPokemonID = Nothing
      , fullName = "Pawmo"
      , typing = Double Electric Fighting
      , ability = Just VoltAbsorb
      , imageUrl = imageUrl 922 "Pawmo"
      , evolutionData = EvolvesFrom [ 1648 ] "Level 18"
      , transformationData = DoesNotTransform
      }
    , { id = 1650
      , nationalDexNumber = 923
      , originalPokemonID = Nothing
      , fullName = "Pawmot"
      , typing = Double Electric Fighting
      , ability = Just VoltAbsorb
      , imageUrl = imageUrl 923 "Pawmot"
      , evolutionData = EvolvesFrom [ 1649 ] "While outside of its Poké Ball after walking 1000 steps using the Let's Go feature"
      , transformationData = DoesNotTransform
      }
    , { id = 1653
      , nationalDexNumber = 924
      , originalPokemonID = Nothing
      , fullName = "Tandemaus"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 924 "Tandemaus"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1654
      , nationalDexNumber = 925
      , originalPokemonID = Nothing
      , fullName = "Maushold"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrl 925 "Maushold"
      , evolutionData = EvolvesFrom [ 1653 ] "Level 25 while battling"
      , transformationData = DoesNotTransform
      }
    , { id = 1655
      , nationalDexNumber = 926
      , originalPokemonID = Nothing
      , fullName = "Fidough"
      , typing = Single Fairy
      , ability = Nothing
      , imageUrl = imageUrl 926 "Fidough"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1656
      , nationalDexNumber = 927
      , originalPokemonID = Nothing
      , fullName = "Dachsbun"
      , typing = Single Fairy
      , ability = Nothing
      , imageUrl = imageUrl 927 "Dachsbun"
      , evolutionData = EvolvesFrom [ 1655 ] "Level 26"
      , transformationData = DoesNotTransform
      }
    , { id = 1657
      , nationalDexNumber = 928
      , originalPokemonID = Nothing
      , fullName = "Smoliv"
      , typing = Double Grass Normal
      , ability = Nothing
      , imageUrl = imageUrl 928 "Smoliv"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1658
      , nationalDexNumber = 929
      , originalPokemonID = Nothing
      , fullName = "Dolliv"
      , typing = Double Grass Normal
      , ability = Nothing
      , imageUrl = imageUrl 929 "Dolliv"
      , evolutionData = EvolvesFrom [ 1657 ] "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 1659
      , nationalDexNumber = 930
      , originalPokemonID = Nothing
      , fullName = "Arboliva"
      , typing = Double Grass Normal
      , ability = Nothing
      , imageUrl = imageUrl 930 "Arboliva"
      , evolutionData = EvolvesFrom [ 1658 ] "Level 35"
      , transformationData = DoesNotTransform
      }
    , { id = 1660
      , nationalDexNumber = 931
      , originalPokemonID = Nothing
      , fullName = "Squawkabilly"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrl 931 "Squawkabilly"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1661
      , nationalDexNumber = 932
      , originalPokemonID = Nothing
      , fullName = "Nacli"
      , typing = Single Rock
      , ability = Nothing
      , imageUrl = imageUrl 932 "Nacli"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1662
      , nationalDexNumber = 933
      , originalPokemonID = Nothing
      , fullName = "Naclstack"
      , typing = Single Rock
      , ability = Nothing
      , imageUrl = imageUrl 933 "Naclstack"
      , evolutionData = EvolvesFrom [ 1661 ] "Level 24"
      , transformationData = DoesNotTransform
      }
    , { id = 1663
      , nationalDexNumber = 934
      , originalPokemonID = Nothing
      , fullName = "Garganacl"
      , typing = Single Rock
      , ability = Nothing
      , imageUrl = imageUrl 934 "Garganacl"
      , evolutionData = EvolvesFrom [ 1662 ] "Level 38"
      , transformationData = DoesNotTransform
      }
    , { id = 1665
      , nationalDexNumber = 935
      , originalPokemonID = Nothing
      , fullName = "Charcadet"
      , typing = Single Fire
      , ability = Just FlashFire
      , imageUrl = imageUrl 935 "Charcadet"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1666
      , nationalDexNumber = 936
      , originalPokemonID = Nothing
      , fullName = "Armarouge"
      , typing = Double Fire Psychic
      , ability = Just FlashFire
      , imageUrl = imageUrl 936 "Armarouge"
      , evolutionData = EvolvesFrom [ 1665 ] "Use Auspicious Armor"
      , transformationData = DoesNotTransform
      }
    , { id = 1667
      , nationalDexNumber = 937
      , originalPokemonID = Nothing
      , fullName = "Ceruledge"
      , typing = Double Fire Ghost
      , ability = Just FlashFire
      , imageUrl = imageUrl 937 "Ceruledge"
      , evolutionData = EvolvesFrom [ 1665 ] "Use Malicious Armor"
      , transformationData = DoesNotTransform
      }
    , { id = 1668
      , nationalDexNumber = 938
      , originalPokemonID = Nothing
      , fullName = "Tadbulb"
      , typing = Single Electric
      , ability = Nothing
      , imageUrl = imageUrl 938 "Tadbulb"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1669
      , nationalDexNumber = 939
      , originalPokemonID = Nothing
      , fullName = "Bellibolt"
      , typing = Single Electric
      , ability = Nothing
      , imageUrl = imageUrl 939 "Bellibolt"
      , evolutionData = EvolvesFrom [ 1668 ] "Use Thunder Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 1670
      , nationalDexNumber = 940
      , originalPokemonID = Nothing
      , fullName = "Wattrel"
      , typing = Double Electric Flying
      , ability = Just VoltAbsorb
      , imageUrl = imageUrl 940 "Wattrel"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1671
      , nationalDexNumber = 941
      , originalPokemonID = Nothing
      , fullName = "Kilowattrel"
      , typing = Double Electric Flying
      , ability = Just VoltAbsorb
      , imageUrl = imageUrl 941 "Kilowattrel"
      , evolutionData = EvolvesFrom [ 1670 ] "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 1674
      , nationalDexNumber = 942
      , originalPokemonID = Nothing
      , fullName = "Maschiff"
      , typing = Single Dark
      , ability = Nothing
      , imageUrl = imageUrl 942 "Maschiff"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1675
      , nationalDexNumber = 943
      , originalPokemonID = Nothing
      , fullName = "Mabosstiff"
      , typing = Single Dark
      , ability = Nothing
      , imageUrl = imageUrl 943 "Mabosstiff"
      , evolutionData = EvolvesFrom [ 1674 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 1676
      , nationalDexNumber = 944
      , originalPokemonID = Nothing
      , fullName = "Shroodle"
      , typing = Double Poison Normal
      , ability = Nothing
      , imageUrl = imageUrl 944 "Shroodle"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1677
      , nationalDexNumber = 945
      , originalPokemonID = Nothing
      , fullName = "Grafaiai"
      , typing = Double Poison Normal
      , ability = Nothing
      , imageUrl = imageUrl 945 "Grafaiai"
      , evolutionData = EvolvesFrom [ 1676 ] "Level 28"
      , transformationData = DoesNotTransform
      }
    , { id = 1681
      , nationalDexNumber = 946
      , originalPokemonID = Nothing
      , fullName = "Bramblin"
      , typing = Double Grass Ghost
      , ability = Nothing
      , imageUrl = imageUrl 946 "Bramblin"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1682
      , nationalDexNumber = 947
      , originalPokemonID = Nothing
      , fullName = "Brambleghast"
      , typing = Double Grass Ghost
      , ability = Nothing
      , imageUrl = imageUrl 947 "Brambleghast"
      , evolutionData = EvolvesFrom [ 1681 ] "While outside of its Poké Ball after walking 1000 steps using the Let's Go feature"
      , transformationData = DoesNotTransform
      }
    , { id = 1683
      , nationalDexNumber = 948
      , originalPokemonID = Nothing
      , fullName = "Toedscool"
      , typing = Double Ground Grass
      , ability = Nothing
      , imageUrl = imageUrl 948 "Toedscool"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1684
      , nationalDexNumber = 949
      , originalPokemonID = Nothing
      , fullName = "Toedscruel"
      , typing = Double Ground Grass
      , ability = Nothing
      , imageUrl = imageUrl 949 "Toedscruel"
      , evolutionData = EvolvesFrom [ 1683 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 1685
      , nationalDexNumber = 950
      , originalPokemonID = Nothing
      , fullName = "Klawf"
      , typing = Single Rock
      , ability = Nothing
      , imageUrl = imageUrl 950 "Klawf"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1686
      , nationalDexNumber = 951
      , originalPokemonID = Nothing
      , fullName = "Capsakid"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrl 951 "Capsakid"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1687
      , nationalDexNumber = 952
      , originalPokemonID = Nothing
      , fullName = "Scovillain"
      , typing = Double Grass Fire
      , ability = Nothing
      , imageUrl = imageUrl 952 "Scovillain"
      , evolutionData = EvolvesFrom [ 1686 ] "Use Fire Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 1688
      , nationalDexNumber = 953
      , originalPokemonID = Nothing
      , fullName = "Rellor"
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imageUrl 953 "Rellor"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1689
      , nationalDexNumber = 954
      , originalPokemonID = Nothing
      , fullName = "Rabsca"
      , typing = Double Bug Psychic
      , ability = Nothing
      , imageUrl = imageUrl 954 "Rabsca"
      , evolutionData = EvolvesFrom [ 1688 ] "While outside of its Poké Ball after walking 1000 steps using the Let's Go feature"
      , transformationData = DoesNotTransform
      }
    , { id = 1690
      , nationalDexNumber = 955
      , originalPokemonID = Nothing
      , fullName = "Flittle"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrl 955 "Flittle"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1691
      , nationalDexNumber = 956
      , originalPokemonID = Nothing
      , fullName = "Espathra"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrl 956 "Espathra"
      , evolutionData = EvolvesFrom [ 1690 ] "Level 35"
      , transformationData = DoesNotTransform
      }
    , { id = 1692
      , nationalDexNumber = 957
      , originalPokemonID = Nothing
      , fullName = "Tinkatink"
      , typing = Double Fairy Steel
      , ability = Nothing
      , imageUrl = imageUrl 957 "Tinkatink"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1693
      , nationalDexNumber = 958
      , originalPokemonID = Nothing
      , fullName = "Tinkatuff"
      , typing = Double Fairy Steel
      , ability = Nothing
      , imageUrl = imageUrl 958 "Tinkatuff"
      , evolutionData = EvolvesFrom [ 1692 ] "Level 24"
      , transformationData = DoesNotTransform
      }
    , { id = 1694
      , nationalDexNumber = 959
      , originalPokemonID = Nothing
      , fullName = "Tinkaton"
      , typing = Double Fairy Steel
      , ability = Nothing
      , imageUrl = imageUrl 959 "Tinkaton"
      , evolutionData = EvolvesFrom [ 1693 ] "Level 38"
      , transformationData = DoesNotTransform
      }
    , { id = 1695
      , nationalDexNumber = 960
      , originalPokemonID = Nothing
      , fullName = "Wiglett"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 960 "Wiglett"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1696
      , nationalDexNumber = 961
      , originalPokemonID = Nothing
      , fullName = "Wugtrio"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 961 "Wugtrio"
      , evolutionData = EvolvesFrom [ 1695 ] "Level 26"
      , transformationData = DoesNotTransform
      }
    , { id = 1697
      , nationalDexNumber = 962
      , originalPokemonID = Nothing
      , fullName = "Bombirdier"
      , typing = Double Flying Dark
      , ability = Nothing
      , imageUrl = imageUrl 962 "Bombirdier"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1698
      , nationalDexNumber = 963
      , originalPokemonID = Nothing
      , fullName = "Finizen"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 963 "Finizen"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1699
      , nationalDexNumber = 964
      , originalPokemonID = Nothing
      , fullName = "Palafin"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 964 "Palafin"
      , evolutionData = EvolvesFrom [ 1698 ] "Level 38 while in the Union Circle with another player"
      , transformationData = DoesNotTransform
      }
    , { id = 1700
      , nationalDexNumber = 965
      , originalPokemonID = Nothing
      , fullName = "Varoom"
      , typing = Double Steel Poison
      , ability = Nothing
      , imageUrl = imageUrl 965 "Varoom"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1701
      , nationalDexNumber = 966
      , originalPokemonID = Nothing
      , fullName = "Revavroom"
      , typing = Double Steel Poison
      , ability = Nothing
      , imageUrl = imageUrl 966 "Revavroom"
      , evolutionData = EvolvesFrom [ 1700 ] "Level 40"
      , transformationData = DoesNotTransform
      }
    , { id = 1702
      , nationalDexNumber = 967
      , originalPokemonID = Nothing
      , fullName = "Cyclizar"
      , typing = Double Dragon Normal
      , ability = Nothing
      , imageUrl = imageUrl 967 "Cyclizar"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1703
      , nationalDexNumber = 968
      , originalPokemonID = Nothing
      , fullName = "Orthworm"
      , typing = Single Steel
      , ability = Nothing
      , imageUrl = imageUrl 968 "Orthworm"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1704
      , nationalDexNumber = 969
      , originalPokemonID = Nothing
      , fullName = "Glimmet"
      , typing = Double Rock Poison
      , ability = Nothing
      , imageUrl = imageUrl 969 "Glimmet"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1705
      , nationalDexNumber = 970
      , originalPokemonID = Nothing
      , fullName = "Glimmora"
      , typing = Double Rock Poison
      , ability = Nothing
      , imageUrl = imageUrl 970 "Glimmora"
      , evolutionData = EvolvesFrom [ 1704 ] "Level 35"
      , transformationData = DoesNotTransform
      }
    , { id = 1706
      , nationalDexNumber = 971
      , originalPokemonID = Nothing
      , fullName = "Greavard"
      , typing = Single Ghost
      , ability = Nothing
      , imageUrl = imageUrl 971 "Greavard"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1707
      , nationalDexNumber = 972
      , originalPokemonID = Nothing
      , fullName = "Houndstone"
      , typing = Single Ghost
      , ability = Nothing
      , imageUrl = imageUrl 972 "Houndstone"
      , evolutionData = EvolvesFrom [ 1706 ] "Level 30 at night"
      , transformationData = DoesNotTransform
      }
    , { id = 1708
      , nationalDexNumber = 973
      , originalPokemonID = Nothing
      , fullName = "Flamigo"
      , typing = Double Flying Fighting
      , ability = Nothing
      , imageUrl = imageUrl 973 "Flamigo"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1709
      , nationalDexNumber = 974
      , originalPokemonID = Nothing
      , fullName = "Cetoddle"
      , typing = Single Ice
      , ability = Just ThickFat
      , imageUrl = imageUrl 974 "Cetoddle"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1710
      , nationalDexNumber = 975
      , originalPokemonID = Nothing
      , fullName = "Cetitan"
      , typing = Single Ice
      , ability = Just ThickFat
      , imageUrl = imageUrl 975 "Cetitan"
      , evolutionData = EvolvesFrom [ 1709 ] "Use Ice Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 1712
      , nationalDexNumber = 976
      , originalPokemonID = Nothing
      , fullName = "Veluza"
      , typing = Double Water Psychic
      , ability = Nothing
      , imageUrl = imageUrl 976 "Veluza"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1713
      , nationalDexNumber = 977
      , originalPokemonID = Nothing
      , fullName = "Dondozo"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrl 977 "Dondozo"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1714
      , nationalDexNumber = 978
      , originalPokemonID = Nothing
      , fullName = "Tatsugiri"
      , typing = Double Dragon Water
      , ability = Nothing
      , imageUrl = imageUrl 978 "Tatsugiri"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1715
      , nationalDexNumber = 984
      , originalPokemonID = Nothing
      , fullName = "Great Tusk"
      , typing = Double Ground Fighting
      , ability = Nothing
      , imageUrl = imageUrl 984 "Great Tusk"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1716
      , nationalDexNumber = 985
      , originalPokemonID = Nothing
      , fullName = "Scream Tail"
      , typing = Double Fairy Psychic
      , ability = Nothing
      , imageUrl = imageUrl 985 "Scream Tail"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1717
      , nationalDexNumber = 986
      , originalPokemonID = Nothing
      , fullName = "Brute Bonnet"
      , typing = Double Grass Dark
      , ability = Nothing
      , imageUrl = imageUrl 986 "Brute Bonnet"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1718
      , nationalDexNumber = 987
      , originalPokemonID = Nothing
      , fullName = "Flutter Mane"
      , typing = Double Ghost Fairy
      , ability = Nothing
      , imageUrl = imageUrl 987 "Flutter Mane"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1719
      , nationalDexNumber = 988
      , originalPokemonID = Nothing
      , fullName = "Slither Wing"
      , typing = Double Bug Fighting
      , ability = Nothing
      , imageUrl = imageUrl 988 "Slither Wing"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1720
      , nationalDexNumber = 989
      , originalPokemonID = Nothing
      , fullName = "Sandy Shocks"
      , typing = Double Electric Ground
      , ability = Nothing
      , imageUrl = imageUrl 989 "Sandy Shocks"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1721
      , nationalDexNumber = 990
      , originalPokemonID = Nothing
      , fullName = "Iron Treads"
      , typing = Double Ground Steel
      , ability = Nothing
      , imageUrl = imageUrl 990 "Iron Treads"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1722
      , nationalDexNumber = 991
      , originalPokemonID = Nothing
      , fullName = "Iron Bundle"
      , typing = Double Ice Water
      , ability = Nothing
      , imageUrl = imageUrl 991 "Iron Bundle"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1723
      , nationalDexNumber = 992
      , originalPokemonID = Nothing
      , fullName = "Iron Hands"
      , typing = Double Fighting Electric
      , ability = Nothing
      , imageUrl = imageUrl 992 "Iron Hands"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1724
      , nationalDexNumber = 993
      , originalPokemonID = Nothing
      , fullName = "Iron Jugulis"
      , typing = Double Dark Flying
      , ability = Nothing
      , imageUrl = imageUrl 993 "Iron Jugulis"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1725
      , nationalDexNumber = 994
      , originalPokemonID = Nothing
      , fullName = "Iron Moth"
      , typing = Double Fire Poison
      , ability = Nothing
      , imageUrl = imageUrl 994 "Iron Moth"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1726
      , nationalDexNumber = 995
      , originalPokemonID = Nothing
      , fullName = "Iron Thorns"
      , typing = Double Rock Electric
      , ability = Nothing
      , imageUrl = imageUrl 995 "Iron Thorns"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1727
      , nationalDexNumber = 996
      , originalPokemonID = Nothing
      , fullName = "Frigibax"
      , typing = Double Dragon Ice
      , ability = Nothing
      , imageUrl = imageUrl 996 "Frigibax"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1728
      , nationalDexNumber = 997
      , originalPokemonID = Nothing
      , fullName = "Arctibax"
      , typing = Double Dragon Ice
      , ability = Nothing
      , imageUrl = imageUrl 997 "Arctibax"
      , evolutionData = EvolvesFrom [ 1727 ] "Level 35"
      , transformationData = DoesNotTransform
      }
    , { id = 1729
      , nationalDexNumber = 998
      , originalPokemonID = Nothing
      , fullName = "Baxcalibur"
      , typing = Double Dragon Ice
      , ability = Nothing
      , imageUrl = imageUrl 998 "Baxcalibur"
      , evolutionData = EvolvesFrom [ 1728 ] "Level 54"
      , transformationData = DoesNotTransform
      }
    , { id = 1730
      , nationalDexNumber = 999
      , originalPokemonID = Nothing
      , fullName = "Gimmighoul"
      , typing = Single Ghost
      , ability = Nothing
      , imageUrl = imageUrl 999 "Gimmighoul"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1731
      , nationalDexNumber = 1000
      , originalPokemonID = Nothing
      , fullName = "Gholdengo"
      , typing = Double Ghost Steel
      , ability = Nothing
      , imageUrl = imageUrl 1000 "Gholdengo"
      , evolutionData = EvolvesFrom [ 1730 ] "Level while having 999 Ghimmighoul Coins"
      , transformationData = DoesNotTransform
      }
    , { id = 1732
      , nationalDexNumber = 1001
      , originalPokemonID = Nothing
      , fullName = "Wo-Chien"
      , typing = Double Dark Grass
      , ability = Nothing
      , imageUrl = imageUrl 1001 "Wo-Chien"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1733
      , nationalDexNumber = 1002
      , originalPokemonID = Nothing
      , fullName = "Chien-Pao"
      , typing = Double Dark Ice
      , ability = Nothing
      , imageUrl = imageUrl 1002 "Chien-Pao"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1734
      , nationalDexNumber = 1003
      , originalPokemonID = Nothing
      , fullName = "Ting-Lu"
      , typing = Double Dark Ground
      , ability = Nothing
      , imageUrl = imageUrl 1003 "Ting-Lu"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1735
      , nationalDexNumber = 1004
      , originalPokemonID = Nothing
      , fullName = "Chi-Yu"
      , typing = Double Dark Fire
      , ability = Nothing
      , imageUrl = imageUrl 1004 "Chi-Yu"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1736
      , nationalDexNumber = 1005
      , originalPokemonID = Nothing
      , fullName = "Roaring Moon"
      , typing = Double Dragon Dark
      , ability = Nothing
      , imageUrl = imageUrl 1005 "Roaring Moon"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1737
      , nationalDexNumber = 1006
      , originalPokemonID = Nothing
      , fullName = "Iron Valiant"
      , typing = Double Fighting Fairy
      , ability = Nothing
      , imageUrl = imageUrl 1006 "Iron Valiant"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1738
      , nationalDexNumber = 1007
      , originalPokemonID = Nothing
      , fullName = "Koraidon"
      , typing = Double Fighting Dragon
      , ability = Nothing
      , imageUrl = imageUrl 1007 "Koraidon"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1739
      , nationalDexNumber = 1008
      , originalPokemonID = Nothing
      , fullName = "Miraidon"
      , typing = Double Electric Dragon
      , ability = Nothing
      , imageUrl = imageUrl 1008 "Miraidon"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    ]
