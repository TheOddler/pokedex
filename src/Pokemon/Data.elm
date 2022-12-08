module Pokemon.Data exposing (all, first)

import Ability exposing (Ability(..))
import Pokemon exposing (..)
import String.Extra as String
import StringHelpers as String
import Type exposing (..)


nameFromData : String -> String -> String -> String
nameFromData name alternateFormName override =
    case override of
        "" ->
            alternateFormName ++ " " ++ name

        _ ->
            override


imageIDCleanup : String -> String
imageIDCleanup =
    String.removeAll [ ".", "'", ":", "%" ]
        << String.replaceAll [ ( " ", "-" ), ( "♀", "f" ), ( "♂", "m" ) ]
        << String.removeAccents
        << String.toLower


imageUrlByName : String -> String
imageUrlByName name =
    imageUrlByGenerationAndID "home" <| imageIDCleanup name


imgUrlForAlternateForm : String -> String -> String
imgUrlForAlternateForm name alternateFormName =
    let
        imageGen =
            case alternateFormName of
                "Hisuian" ->
                    "legends-arceus"

                _ ->
                    "home"

        imageID =
            imageIDCleanup name ++ "-" ++ imageIDCleanup alternateFormName
    in
    imageUrlByGenerationAndID imageGen imageID


imageUrlByGenerationAndID : String -> String -> String
imageUrlByGenerationAndID imageGen imageID =
    "https://img.pokemondb.net/sprites/" ++ imageGen ++ "/normal/" ++ imageID ++ ".png"


first : Pokemon
first =
    { id = 1
    , originalPokemonID = Nothing
    , fullName = "Bulbasaur"
    , typing = Double Grass Poison
    , ability = Nothing
    , imageUrl = imageUrlByName "Bulbasaur"
    , evolutionData = DoesNotEvolve
    , transformationData = DoesNotTransform
    }


all : List Pokemon
all =
    [ first
    , { id = 2
      , originalPokemonID = Nothing
      , fullName = "Ivysaur"
      , typing = Double Grass Poison
      , ability = Nothing
      , imageUrl = imageUrlByName "Ivysaur"
      , evolutionData = EvolvesFrom [ 1 ] "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 3
      , originalPokemonID = Nothing
      , fullName = "Venusaur"
      , typing = Double Grass Poison
      , ability = Nothing
      , imageUrl = imageUrlByName "Venusaur"
      , evolutionData = EvolvesFrom [ 2 ] "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 4
      , originalPokemonID = Nothing
      , fullName = nameFromData "Venusaur" "Mega" ""
      , typing = Double Grass Poison
      , ability = Just ThickFat
      , imageUrl = imgUrlForAlternateForm "Venusaur" "Mega"
      , evolutionData = EvolvesFrom [ 3 ] "Holding Venusaurite"
      , transformationData = DoesNotTransform
      }
    , { id = 5
      , originalPokemonID = Nothing
      , fullName = "Charmander"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrlByName "Charmander"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 6
      , originalPokemonID = Nothing
      , fullName = "Charmeleon"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrlByName "Charmeleon"
      , evolutionData = EvolvesFrom [ 5 ] "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 7
      , originalPokemonID = Nothing
      , fullName = "Charizard"
      , typing = Double Fire Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Charizard"
      , evolutionData = EvolvesFrom [ 6 ] "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 8
      , originalPokemonID = Nothing
      , fullName = nameFromData "Charizard" "Mega X" "Mega Charizard X"
      , typing = Double Fire Dragon
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Charizard" "Mega X"
      , evolutionData = EvolvesFrom [ 7 ] "Holding Charizardite X"
      , transformationData =
            DoesNotTransform
      }
    , { id = 9
      , originalPokemonID = Nothing
      , fullName = nameFromData "Charizard" "Mega Y" "Mega Charizard Y"
      , typing = Double Fire Flying
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Charizard" "Mega Y"
      , evolutionData = EvolvesFrom [ 7 ] "Holding Charizardite Y"
      , transformationData = DoesNotTransform
      }
    , { id = 10
      , originalPokemonID = Nothing
      , fullName = "Squirtle"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Squirtle"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 11
      , originalPokemonID = Nothing
      , fullName = "Wartortle"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Wartortle"
      , evolutionData = EvolvesFrom [ 10 ] "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 12
      , originalPokemonID = Nothing
      , fullName = "Blastoise"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Blastoise"
      , evolutionData = EvolvesFrom [ 11 ] "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 13
      , originalPokemonID = Nothing
      , fullName = nameFromData "Blastoise" "Mega" ""
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Blastoise" "Mega"
      , evolutionData = EvolvesFrom [ 12 ] "Holding Blastoisinite"
      , transformationData = DoesNotTransform
      }
    , { id = 32
      , originalPokemonID = Nothing
      , fullName = "Caterpie"
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imageUrlByName "Caterpie"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 23
      , originalPokemonID = Nothing
      , fullName = "Metapod"
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imageUrlByName "Metapod"
      , evolutionData = EvolvesFrom [ 32 ] "Level 7"
      , transformationData = DoesNotTransform
      }
    , { id = 24
      , originalPokemonID = Nothing
      , fullName = "Butterfree"
      , typing = Double Bug Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Butterfree"
      , evolutionData = EvolvesFrom [ 23 ] "Level 10"
      , transformationData = DoesNotTransform
      }
    , { id = 31
      , originalPokemonID = Nothing
      , fullName = "Weedle"
      , typing = Double Bug Poison
      , ability = Nothing
      , imageUrl = imageUrlByName "Weedle"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 30
      , originalPokemonID = Nothing
      , fullName = "Kakuna"
      , typing = Double Bug Poison
      , ability = Nothing
      , imageUrl = imageUrlByName "Kakuna"
      , evolutionData = EvolvesFrom [ 31 ] "Level 7"
      , transformationData = DoesNotTransform
      }
    , { id = 27
      , originalPokemonID = Nothing
      , fullName = "Beedrill"
      , typing = Double Bug Poison
      , ability = Nothing
      , imageUrl = imageUrlByName "Beedrill"
      , evolutionData = EvolvesFrom [ 30 ] "Level 10"
      , transformationData = DoesNotTransform
      }
    , { id = 28
      , originalPokemonID = Nothing
      , fullName = nameFromData "Beedrill" "Mega" ""
      , typing = Double Bug Poison
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Beedrill" "Mega"
      , evolutionData = EvolvesFrom [ 27 ] "Holding Beedrillite"
      , transformationData = DoesNotTransform
      }
    , { id = 76
      , originalPokemonID = Nothing
      , fullName = "Pidgey"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Pidgey"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 77
      , originalPokemonID = Nothing
      , fullName = "Pidgeotto"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Pidgeotto"
      , evolutionData = EvolvesFrom [ 76 ] "Level 18"
      , transformationData = DoesNotTransform
      }
    , { id = 78
      , originalPokemonID = Nothing
      , fullName = "Pidgeot"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Pidgeot"
      , evolutionData = EvolvesFrom [ 77 ] "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 79
      , originalPokemonID = Nothing
      , fullName = nameFromData "Pidgeot" "Mega" ""
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Pidgeot" "Mega"
      , evolutionData = EvolvesFrom [ 78 ] "Holding Pidgeotite"
      , transformationData = DoesNotTransform
      }
    , { id = 80
      , originalPokemonID = Nothing
      , fullName = "Rattata"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Rattata"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 82
      , originalPokemonID = Nothing
      , fullName = "Raticate"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Raticate"
      , evolutionData = EvolvesFrom [ 80 ] "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 81
      , originalPokemonID = Just 80
      , fullName = nameFromData "Rattata" "Alolan" ""
      , typing = Double Dark Normal
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Rattata" "Alolan"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 83
      , originalPokemonID = Just 82
      , fullName = nameFromData "Raticate" "Alolan" ""
      , typing = Double Dark Normal
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Raticate" "Alolan"
      , evolutionData = EvolvesFrom [ 81 ] "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 84
      , originalPokemonID = Nothing
      , fullName = "Spearow"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Spearow"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 85
      , originalPokemonID = Nothing
      , fullName = "Fearow"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Fearow"
      , evolutionData = EvolvesFrom [ 84 ] "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 86
      , originalPokemonID = Nothing
      , fullName = "Ekans"
      , typing = Single Poison
      , ability = Nothing
      , imageUrl = imageUrlByName "Ekans"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 87
      , originalPokemonID = Nothing
      , fullName = "Arbok"
      , typing = Single Poison
      , ability = Nothing
      , imageUrl = imageUrlByName "Arbok"
      , evolutionData = EvolvesFrom [ 86 ] "Level 22"
      , transformationData = DoesNotTransform
      }
    , { id = 88
      , originalPokemonID = Nothing
      , fullName = "Pichu"
      , typing = Single Electric
      , ability = Nothing
      , imageUrl = imageUrlByName "Pichu"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 89
      , originalPokemonID = Nothing
      , fullName = "Pikachu"
      , typing = Single Electric
      , ability = Nothing
      , imageUrl = imageUrlByName "Pikachu"
      , evolutionData = EvolvesFrom [ 88 ] "Level with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 90
      , originalPokemonID = Nothing
      , fullName = "Raichu"
      , typing = Single Electric
      , ability = Nothing
      , imageUrl = imageUrlByName "Raichu"
      , evolutionData = EvolvesFrom [ 89 ] "Use Thunder Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 91
      , originalPokemonID = Just 90
      , fullName = nameFromData "Raichu" "Alolan" ""
      , typing = Double Electric Psychic
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Raichu" "Alolan"
      , evolutionData = EvolvesFrom [ 89 ] "Use Thunder Stone in Alola"
      , transformationData = DoesNotTransform
      }
    , { id = 92
      , originalPokemonID = Nothing
      , fullName = "Sandshrew"
      , typing = Single Ground
      , ability = Nothing
      , imageUrl = imageUrlByName "Sandshrew"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 94
      , originalPokemonID = Nothing
      , fullName = "Sandslash"
      , typing = Single Ground
      , ability = Nothing
      , imageUrl = imageUrlByName "Sandslash"
      , evolutionData = EvolvesFrom [ 92 ] "Level 22"
      , transformationData = DoesNotTransform
      }
    , { id = 93
      , originalPokemonID = Just 92
      , fullName = nameFromData "Sandshrew" "Alolan" ""
      , typing = Double Ice Steel
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Sandshrew" "Alolan"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 95
      , originalPokemonID = Just 94
      , fullName = nameFromData "Sandslash" "Alolan" ""
      , typing = Double Ice Steel
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Sandslash" "Alolan"
      , evolutionData = EvolvesFrom [ 93 ] "Use Ice Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 33
      , originalPokemonID = Nothing
      , fullName = "Nidoran ♀"
      , typing = Single Poison
      , ability = Nothing
      , imageUrl = imageUrlByName "Nidoran ♀"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 17
      , originalPokemonID = Nothing
      , fullName = "Nidorina"
      , typing = Single Poison
      , ability = Nothing
      , imageUrl = imageUrlByName "Nidorina"
      , evolutionData = EvolvesFrom [ 33 ] "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 18
      , originalPokemonID = Nothing
      , fullName = "Nidoqueen"
      , typing = Double Poison Ground
      , ability = Nothing
      , imageUrl = imageUrlByName "Nidoqueen"
      , evolutionData = EvolvesFrom [ 17 ] "Use Moon Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 48
      , originalPokemonID = Nothing
      , fullName = "Nidoran ♂"
      , typing = Single Poison
      , ability = Nothing
      , imageUrl = imageUrlByName "Nidoran ♂"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 49
      , originalPokemonID = Nothing
      , fullName = "Nidorino"
      , typing = Single Poison
      , ability = Nothing
      , imageUrl = imageUrlByName "Nidorino"
      , evolutionData = EvolvesFrom [ 48 ] "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 50
      , originalPokemonID = Nothing
      , fullName = "Nidoking"
      , typing = Double Poison Ground
      , ability = Nothing
      , imageUrl = imageUrlByName "Nidoking"
      , evolutionData = EvolvesFrom [ 49 ] "Use Moon Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 51
      , originalPokemonID = Nothing
      , fullName = "Cleffa"
      , typing = Single Fairy
      , ability = Nothing
      , imageUrl = imageUrlByName "Cleffa"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 52
      , originalPokemonID = Nothing
      , fullName = "Clefairy"
      , typing = Single Fairy
      , ability = Nothing
      , imageUrl = imageUrlByName "Clefairy"
      , evolutionData = EvolvesFrom [ 51 ] "Level with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 53
      , originalPokemonID = Nothing
      , fullName = "Clefable"
      , typing = Single Fairy
      , ability = Nothing
      , imageUrl = imageUrlByName "Clefable"
      , evolutionData = EvolvesFrom [ 52 ] "Use Moon Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 54
      , originalPokemonID = Nothing
      , fullName = "Vulpix"
      , typing = Single Fire
      , ability = Just FlashFire
      , imageUrl = imageUrlByName "Vulpix"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 56
      , originalPokemonID = Nothing
      , fullName = "Ninetales"
      , typing = Single Fire
      , ability = Just FlashFire
      , imageUrl = imageUrlByName "Ninetales"
      , evolutionData = EvolvesFrom [ 54 ] "Use Fire Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 55
      , originalPokemonID = Just 54
      , fullName = nameFromData "Vulpix" "Alolan" ""
      , typing = Single Ice
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Vulpix" "Alolan"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 57
      , originalPokemonID = Just 56
      , fullName = nameFromData "Ninetales" "Alolan" ""
      , typing = Double Ice Fairy
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Ninetales" "Alolan"
      , evolutionData = EvolvesFrom [ 55 ] "Use Ice Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 58
      , originalPokemonID = Nothing
      , fullName = "Igglybuff"
      , typing = Double Normal Fairy
      , ability = Nothing
      , imageUrl = imageUrlByName "Igglybuff"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 59
      , originalPokemonID = Nothing
      , fullName = "Jigglypuff"
      , typing = Double Normal Fairy
      , ability = Nothing
      , imageUrl = imageUrlByName "Jigglypuff"
      , evolutionData = EvolvesFrom [ 58 ] "Level with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 60
      , originalPokemonID = Nothing
      , fullName = "Wigglytuff"
      , typing = Double Normal Fairy
      , ability = Nothing
      , imageUrl = imageUrlByName "Wigglytuff"
      , evolutionData = EvolvesFrom [ 59 ] "Use Moon Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 61
      , originalPokemonID = Nothing
      , fullName = "Zubat"
      , typing = Double Poison Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Zubat"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 62
      , originalPokemonID = Nothing
      , fullName = "Golbat"
      , typing = Double Poison Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Golbat"
      , evolutionData = EvolvesFrom [ 61 ] "Level 22"
      , transformationData = DoesNotTransform
      }
    , { id = 63
      , originalPokemonID = Nothing
      , fullName = "Crobat"
      , typing = Double Poison Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Crobat"
      , evolutionData = EvolvesFrom [ 62 ] "Level with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 64
      , originalPokemonID = Nothing
      , fullName = "Oddish"
      , typing = Double Grass Poison
      , ability = Nothing
      , imageUrl = imageUrlByName "Oddish"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 65
      , originalPokemonID = Nothing
      , fullName = "Gloom"
      , typing = Double Grass Poison
      , ability = Nothing
      , imageUrl = imageUrlByName "Gloom"
      , evolutionData = EvolvesFrom [ 64 ] "Level 21"
      , transformationData = DoesNotTransform
      }
    , { id = 66
      , originalPokemonID = Nothing
      , fullName = "Vileplume"
      , typing = Double Grass Poison
      , ability = Nothing
      , imageUrl = imageUrlByName "Vileplume"
      , evolutionData = EvolvesFrom [ 65 ] "Use Leaf Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 67
      , originalPokemonID = Nothing
      , fullName = "Bellossom"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Bellossom"
      , evolutionData = EvolvesFrom [ 65 ] "Use Sun Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 68
      , originalPokemonID = Nothing
      , fullName = "Paras"
      , typing = Double Bug Grass
      , ability = Just DrySkin
      , imageUrl = imageUrlByName "Paras"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 69
      , originalPokemonID = Nothing
      , fullName = "Parasect"
      , typing = Double Bug Grass
      , ability = Just DrySkin
      , imageUrl = imageUrlByName "Parasect"
      , evolutionData = EvolvesFrom [ 68 ] "Level 24"
      , transformationData = DoesNotTransform
      }
    , { id = 70
      , originalPokemonID = Nothing
      , fullName = "Venonat"
      , typing = Double Bug Poison
      , ability = Nothing
      , imageUrl = imageUrlByName "Venonat"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 71
      , originalPokemonID = Nothing
      , fullName = "Venomoth"
      , typing = Double Bug Poison
      , ability = Nothing
      , imageUrl = imageUrlByName "Venomoth"
      , evolutionData = EvolvesFrom [ 70 ] "Level 31"
      , transformationData = DoesNotTransform
      }
    , { id = 72
      , originalPokemonID = Nothing
      , fullName = "Diglett"
      , typing = Single Ground
      , ability = Nothing
      , imageUrl = imageUrlByName "Diglett"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 73
      , originalPokemonID = Nothing
      , fullName = "Dugtrio"
      , typing = Single Ground
      , ability = Nothing
      , imageUrl = imageUrlByName "Dugtrio"
      , evolutionData = EvolvesFrom [ 72 ] "Level 26"
      , transformationData = DoesNotTransform
      }
    , { id = 74
      , originalPokemonID = Just 72
      , fullName = nameFromData "Diglett" "Alolan" ""
      , typing = Double Ground Steel
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Diglett" "Alolan"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 75
      , originalPokemonID = Just 73
      , fullName = nameFromData "Dugtrio" "Alolan" ""
      , typing = Double Ground Steel
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Dugtrio" "Alolan"
      , evolutionData = EvolvesFrom [ 74 ] "Level 26 in Alola"
      , transformationData = DoesNotTransform
      }
    , { id = 96
      , originalPokemonID = Nothing
      , fullName = "Meowth"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Meowth"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 100
      , originalPokemonID = Nothing
      , fullName = "Persian"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Persian"
      , evolutionData = EvolvesFrom [ 96 ] "Level 28"
      , transformationData = DoesNotTransform
      }
    , { id = 97
      , originalPokemonID = Just 96
      , fullName = nameFromData "Meowth" "Alolan" ""
      , typing = Single Dark
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Meowth" "Alolan"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 101
      , originalPokemonID = Just 100
      , fullName = nameFromData "Persian" "Alolan" ""
      , typing = Single Dark
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Persian" "Alolan"
      , evolutionData = EvolvesFrom [ 97 ] "Level with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 98
      , originalPokemonID = Just 96
      , fullName = nameFromData "Meowth" "Galarian" ""
      , typing = Single Steel
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Meowth" "Galarian"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 99
      , originalPokemonID = Nothing
      , fullName = "Perrserker"
      , typing = Single Steel
      , ability = Nothing
      , imageUrl = imageUrlByName "Perrserker"
      , evolutionData = EvolvesFrom [ 98 ] "Level 28"
      , transformationData = DoesNotTransform
      }
    , { id = 102
      , originalPokemonID = Nothing
      , fullName = "Psyduck"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Psyduck"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 884
      , originalPokemonID = Nothing
      , fullName = "Golduck"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Golduck"
      , evolutionData = EvolvesFrom [ 102 ] "Level 33"
      , transformationData = DoesNotTransform
      }
    , { id = 895
      , originalPokemonID = Nothing
      , fullName = "Mankey"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrlByName "Mankey"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 924
      , originalPokemonID = Nothing
      , fullName = "Primeape"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrlByName "Primeape"
      , evolutionData = EvolvesFrom [ 895 ] "Level 28"
      , transformationData = DoesNotTransform
      }
    , { id = 1664
      , originalPokemonID = Nothing
      , fullName = "Annihilape"
      , typing = Double Fighting Ghost
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "annihilape"
      , evolutionData = EvolvesFrom [ 924 ] "After using Rage Fist 20 times"
      , transformationData = DoesNotTransform
      }
    , { id = 949
      , originalPokemonID = Nothing
      , fullName = "Growlithe"
      , typing = Single Fire
      , ability = Just FlashFire
      , imageUrl = imageUrlByName "Growlithe"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 960
      , originalPokemonID = Nothing
      , fullName = "Arcanine"
      , typing = Single Fire
      , ability = Just FlashFire
      , imageUrl = imageUrlByName "Arcanine"
      , evolutionData = EvolvesFrom [ 949 ] "Use Fire Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 1619
      , originalPokemonID = Just 949
      , fullName = nameFromData "Growlithe" "Hisuian" ""
      , typing = Double Fire Rock
      , ability = Just FlashFire
      , imageUrl = imgUrlForAlternateForm "Growlithe" "Hisuian"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1620
      , originalPokemonID = Just 960
      , fullName = nameFromData "Arcanine" "Hisuian" ""
      , typing = Double Fire Rock
      , ability = Just FlashFire
      , imageUrl = imgUrlForAlternateForm "Arcanine" "Hisuian"
      , evolutionData = EvolvesFrom [ 1619 ] "Use Fire Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 971
      , originalPokemonID = Nothing
      , fullName = "Poliwag"
      , typing = Single Water
      , ability = Just WaterAbsorb
      , imageUrl = imageUrlByName "Poliwag"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 982
      , originalPokemonID = Nothing
      , fullName = "Poliwhirl"
      , typing = Single Water
      , ability = Just WaterAbsorb
      , imageUrl = imageUrlByName "Poliwhirl"
      , evolutionData = EvolvesFrom [ 971 ] "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 993
      , originalPokemonID = Nothing
      , fullName = "Poliwrath"
      , typing = Double Water Fighting
      , ability = Just WaterAbsorb
      , imageUrl = imageUrlByName "Poliwrath"
      , evolutionData = EvolvesFrom [ 982 ] "Use Water Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 286
      , originalPokemonID = Nothing
      , fullName = "Politoed"
      , typing = Single Water
      , ability = Just WaterAbsorb
      , imageUrl = imageUrlByName "Politoed"
      , evolutionData = EvolvesFrom [ 982 ] "Trade holding King's Rock"
      , transformationData = DoesNotTransform
      }
    , { id = 1004
      , originalPokemonID = Nothing
      , fullName = "Abra"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Abra"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1015
      , originalPokemonID = Nothing
      , fullName = "Kadabra"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Kadabra"
      , evolutionData = EvolvesFrom [ 1004 ] "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 1026
      , originalPokemonID = Nothing
      , fullName = "Alakazam"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Alakazam"
      , evolutionData = EvolvesFrom [ 1015 ] "Trade"
      , transformationData = DoesNotTransform
      }
    , { id = 1360
      , originalPokemonID = Nothing
      , fullName = nameFromData "Alakazam" "Mega" ""
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Alakazam" "Mega"
      , evolutionData = EvolvesFrom [ 1026 ] "Holding Alakazite"
      , transformationData = DoesNotTransform
      }
    , { id = 1076
      , originalPokemonID = Nothing
      , fullName = "Machop"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrlByName "Machop"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1087
      , originalPokemonID = Nothing
      , fullName = "Machoke"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrlByName "Machoke"
      , evolutionData = EvolvesFrom [ 1076 ] "Level 28"
      , transformationData = DoesNotTransform
      }
    , { id = 1097
      , originalPokemonID = Nothing
      , fullName = "Machamp"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrlByName "Machamp"
      , evolutionData = EvolvesFrom [ 1087 ] "Trade"
      , transformationData = DoesNotTransform
      }
    , { id = 1109
      , originalPokemonID = Nothing
      , fullName = "Bellsprout"
      , typing = Double Grass Poison
      , ability = Nothing
      , imageUrl = imageUrlByName "Bellsprout"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1128
      , originalPokemonID = Nothing
      , fullName = "Weepinbell"
      , typing = Double Grass Poison
      , ability = Nothing
      , imageUrl = imageUrlByName "Weepinbell"
      , evolutionData = EvolvesFrom [ 1109 ] "Level 21"
      , transformationData = DoesNotTransform
      }
    , { id = 1139
      , originalPokemonID = Nothing
      , fullName = "Victreebel"
      , typing = Double Grass Poison
      , ability = Nothing
      , imageUrl = imageUrlByName "Victreebel"
      , evolutionData = EvolvesFrom [ 1128 ] "Use Leaf Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 1150
      , originalPokemonID = Nothing
      , fullName = "Tentacool"
      , typing = Double Water Poison
      , ability = Nothing
      , imageUrl = imageUrlByName "Tentacool"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1254
      , originalPokemonID = Nothing
      , fullName = "Tentacruel"
      , typing = Double Water Poison
      , ability = Nothing
      , imageUrl = imageUrlByName "Tentacruel"
      , evolutionData = EvolvesFrom [ 1150 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 1265
      , originalPokemonID = Nothing
      , fullName = "Geodude"
      , typing = Double Rock Ground
      , ability = Nothing
      , imageUrl = imageUrlByName "Geodude"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1276
      , originalPokemonID = Nothing
      , fullName = "Graveler"
      , typing = Double Rock Ground
      , ability = Nothing
      , imageUrl = imageUrlByName "Graveler"
      , evolutionData = EvolvesFrom [ 1265 ] "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 1287
      , originalPokemonID = Nothing
      , fullName = "Golem"
      , typing = Double Rock Ground
      , ability = Nothing
      , imageUrl = imageUrlByName "Golem"
      , evolutionData = EvolvesFrom [ 1276 ] "Trade"
      , transformationData = DoesNotTransform
      }
    , { id = 1361
      , originalPokemonID = Just 1265
      , fullName = nameFromData "Geodude" "Alolan" ""
      , typing = Double Rock Electric
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Geodude" "Alolan"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1362
      , originalPokemonID = Just 1276
      , fullName = nameFromData "Graveler" "Alolan" ""
      , typing = Double Rock Electric
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Graveler" "Alolan"
      , evolutionData = EvolvesFrom [ 1361 ] "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 1363
      , originalPokemonID = Just 1287
      , fullName = nameFromData "Golem" "Alolan" ""
      , typing = Double Rock Electric
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Golem" "Alolan"
      , evolutionData = EvolvesFrom [ 1362 ] "Trade"
      , transformationData = DoesNotTransform
      }
    , { id = 1297
      , originalPokemonID = Nothing
      , fullName = "Ponyta"
      , typing = Single Fire
      , ability = Just FlashFire
      , imageUrl = imageUrlByName "Ponyta"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1308
      , originalPokemonID = Nothing
      , fullName = "Rapidash"
      , typing = Single Fire
      , ability = Just FlashFire
      , imageUrl = imageUrlByName "Rapidash"
      , evolutionData = EvolvesFrom [ 1297 ] "Level 40"
      , transformationData = DoesNotTransform
      }
    , { id = 1364
      , originalPokemonID = Just 1297
      , fullName = nameFromData "Ponyta" "Galarian" ""
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Ponyta" "Galarian"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1365
      , originalPokemonID = Just 1308
      , fullName = nameFromData "Rapidash" "Galarian" ""
      , typing = Double Psychic Fairy
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Rapidash" "Galarian"
      , evolutionData = EvolvesFrom [ 1364 ] "Level 40"
      , transformationData = DoesNotTransform
      }
    , { id = 1319
      , originalPokemonID = Nothing
      , fullName = "Slowpoke"
      , typing = Double Water Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Slowpoke"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1330
      , originalPokemonID = Nothing
      , fullName = "Slowbro"
      , typing = Double Water Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Slowbro"
      , evolutionData = EvolvesFrom [ 1319 ] "Level 37"
      , transformationData = DoesNotTransform
      }
    , { id = 1367
      , originalPokemonID = Nothing
      , fullName = nameFromData "Slowbro" "Mega" ""
      , typing = Double Water Psychic
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Slowbro" "Mega"
      , evolutionData = EvolvesFrom [ 1330 ] "Holding Slowbronite"
      , transformationData = DoesNotTransform
      }
    , { id = 299
      , originalPokemonID = Nothing
      , fullName = "Slowking"
      , typing = Double Water Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Slowking"
      , evolutionData = EvolvesFrom [ 1319 ] "Trade holding King's Rock"
      , transformationData = DoesNotTransform
      }
    , { id = 1366
      , originalPokemonID = Just 1319
      , fullName = nameFromData "Slowpoke" "Galarian" ""
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Slowpoke" "Galarian"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1368
      , originalPokemonID = Just 1330
      , fullName = nameFromData "Slowbro" "Galarian" ""
      , typing = Double Poison Psychic
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Slowbro" "Galarian"
      , evolutionData = EvolvesFrom [ 1366 ] "Use Galarica Cuff"
      , transformationData = DoesNotTransform
      }
    , { id = 1388
      , originalPokemonID = Just 299
      , fullName = nameFromData "Slowking" "Galarian" ""
      , typing = Double Poison Psychic
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Slowking" "Galarian"
      , evolutionData = EvolvesFrom [ 1366 ] "Use Galarica Wreath"
      , transformationData = DoesNotTransform
      }
    , { id = 46
      , originalPokemonID = Nothing
      , fullName = "Magnemite"
      , typing = Double Electric Steel
      , ability = Nothing
      , imageUrl = imageUrlByName "Magnemite"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1341
      , originalPokemonID = Nothing
      , fullName = "Magneton"
      , typing = Double Electric Steel
      , ability = Nothing
      , imageUrl = imageUrlByName "Magneton"
      , evolutionData = EvolvesFrom [ 46 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 684
      , originalPokemonID = Nothing
      , fullName = "Magnezone"
      , typing = Double Electric Steel
      , ability = Nothing
      , imageUrl = imageUrlByName "Magnezone"
      , evolutionData = EvolvesFrom [ 1341 ] "Use Thunder Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 1342
      , originalPokemonID = Nothing
      , fullName = "Farfetch'd"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Farfetch'd"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1369
      , originalPokemonID = Just 1342
      , fullName = nameFromData "Farfetch'd" "Galarian" ""
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Farfetch'd" "Galarian"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1398
      , originalPokemonID = Nothing
      , fullName = "Sirfetch'd"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrlByName "Sirfetch'd"
      , evolutionData = EvolvesFrom [ 1369 ] "Land three Critical Hits in one battle"
      , transformationData = DoesNotTransform
      }
    , { id = 1343
      , originalPokemonID = Nothing
      , fullName = "Doduo"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Doduo"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1344
      , originalPokemonID = Nothing
      , fullName = "Dodrio"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Dodrio"
      , evolutionData = EvolvesFrom [ 1343 ] "Level 31"
      , transformationData = DoesNotTransform
      }
    , { id = 1345
      , originalPokemonID = Nothing
      , fullName = "Seel"
      , typing = Single Water
      , ability = Just ThickFat
      , imageUrl = imageUrlByName "Seel"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1346
      , originalPokemonID = Nothing
      , fullName = "Dewgong"
      , typing = Double Water Ice
      , ability = Just ThickFat
      , imageUrl = imageUrlByName "Dewgong"
      , evolutionData = EvolvesFrom [ 1345 ] "Level 34"
      , transformationData = DoesNotTransform
      }
    , { id = 1347
      , originalPokemonID = Nothing
      , fullName = "Grimer"
      , typing = Single Poison
      , ability = Nothing
      , imageUrl = imageUrlByName "Grimer"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1348
      , originalPokemonID = Nothing
      , fullName = "Muk"
      , typing = Single Poison
      , ability = Nothing
      , imageUrl = imageUrlByName "Muk"
      , evolutionData = EvolvesFrom [ 1347 ] "Level 38"
      , transformationData = DoesNotTransform
      }
    , { id = 1372
      , originalPokemonID = Just 1347
      , fullName = nameFromData "Grimer" "Alolan" ""
      , typing = Double Poison Dark
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Grimer" "Alolan"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1373
      , originalPokemonID = Just 1348
      , fullName = nameFromData "Muk" "Alolan" ""
      , typing = Double Poison Dark
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Muk" "Alolan"
      , evolutionData = EvolvesFrom [ 1372 ] "Level 38"
      , transformationData = DoesNotTransform
      }
    , { id = 1349
      , originalPokemonID = Nothing
      , fullName = "Shellder"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Shellder"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1350
      , originalPokemonID = Nothing
      , fullName = "Cloyster"
      , typing = Double Water Ice
      , ability = Nothing
      , imageUrl = imageUrlByName "Cloyster"
      , evolutionData = EvolvesFrom [ 1349 ] "Use Water Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 1351
      , originalPokemonID = Nothing
      , fullName = "Gastly"
      , typing = Double Ghost Poison
      , ability = Just Levitate
      , imageUrl = imageUrlByName "Gastly"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1352
      , originalPokemonID = Nothing
      , fullName = "Haunter"
      , typing = Double Ghost Poison
      , ability = Just Levitate
      , imageUrl = imageUrlByName "Haunter"
      , evolutionData = EvolvesFrom [ 1351 ] "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 1353
      , originalPokemonID = Nothing
      , fullName = "Gengar"
      , typing = Double Ghost Poison
      , ability = Nothing
      , imageUrl = imageUrlByName "Gengar"
      , evolutionData = EvolvesFrom [ 1352 ] "Trade"
      , transformationData = DoesNotTransform
      }
    , { id = 1371
      , originalPokemonID = Nothing
      , fullName = nameFromData "Gengar" "Mega" ""
      , typing = Double Ghost Poison
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Gengar" "Mega"
      , evolutionData = EvolvesFrom [ 1353 ] "Holding Gengarite"
      , transformationData = DoesNotTransform
      }
    , { id = 1354
      , originalPokemonID = Nothing
      , fullName = "Onix"
      , typing = Double Rock Ground
      , ability = Nothing
      , imageUrl = imageUrlByName "Onix"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 332
      , originalPokemonID = Nothing
      , fullName = "Steelix"
      , typing = Double Steel Ground
      , ability = Nothing
      , imageUrl = imageUrlByName "Steelix"
      , evolutionData = EvolvesFrom [ 1354 ] "Trade holding Metal Coat"
      , transformationData = DoesNotTransform
      }
    , { id = 1383
      , originalPokemonID = Nothing
      , fullName = nameFromData "Steelix" "Mega" ""
      , typing = Double Steel Ground
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Steelix" "Mega"
      , evolutionData = EvolvesFrom [ 332 ] "Holding Steelixite"
      , transformationData = DoesNotTransform
      }
    , { id = 1355
      , originalPokemonID = Nothing
      , fullName = "Drowzee"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Drowzee"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1356
      , originalPokemonID = Nothing
      , fullName = "Hypno"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Hypno"
      , evolutionData = EvolvesFrom [ 1355 ] "Level 26"
      , transformationData = DoesNotTransform
      }
    , { id = 1357
      , originalPokemonID = Nothing
      , fullName = "Krabby"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Krabby"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1358
      , originalPokemonID = Nothing
      , fullName = "Kingler"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Kingler"
      , evolutionData = EvolvesFrom [ 1357 ] "Level 28"
      , transformationData = DoesNotTransform
      }
    , { id = 47
      , originalPokemonID = Nothing
      , fullName = "Voltorb"
      , typing = Single Electric
      , ability = Nothing
      , imageUrl = imageUrlByName "Voltorb"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 105
      , originalPokemonID = Nothing
      , fullName = "Electrode"
      , typing = Single Electric
      , ability = Nothing
      , imageUrl = imageUrlByName "Electrode"
      , evolutionData = EvolvesFrom [ 47 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 1621
      , originalPokemonID = Just 47
      , fullName = nameFromData "Voltorb" "Hisuian" ""
      , typing = Double Electric Grass
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Voltorb" "Hisuian"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1622
      , originalPokemonID = Just 105
      , fullName = nameFromData "Electrode" "Hisuian" ""
      , typing = Double Electric Grass
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Electrode" "Hisuian"
      , evolutionData = EvolvesFrom [ 1621 ] "Use Leaf Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 136
      , originalPokemonID = Nothing
      , fullName = "Exeggcute"
      , typing = Double Grass Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Exeggcute"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 137
      , originalPokemonID = Nothing
      , fullName = "Exeggutor"
      , typing = Double Grass Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Exeggutor"
      , evolutionData = EvolvesFrom [ 136 ] "Use Leaf Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 1359
      , originalPokemonID = Just 137
      , fullName = nameFromData "Exeggutor" "Alolan" ""
      , typing = Double Grass Dragon
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Exeggutor" "Alolan"
      , evolutionData = EvolvesFrom [ 136 ] "Use Leaf Stone in Alola"
      , transformationData = DoesNotTransform
      }
    , { id = 138
      , originalPokemonID = Nothing
      , fullName = "Cubone"
      , typing = Single Ground
      , ability = Just LightningRod
      , imageUrl = imageUrlByName "Cubone"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 139
      , originalPokemonID = Nothing
      , fullName = "Marowak"
      , typing = Single Ground
      , ability = Just LightningRod
      , imageUrl = imageUrlByName "Marowak"
      , evolutionData = EvolvesFrom [ 138 ] "Level 28"
      , transformationData = DoesNotTransform
      }
    , { id = 1374
      , originalPokemonID = Just 139
      , fullName = nameFromData "Marowak" "Alolan" ""
      , typing = Double Ghost Fire
      , ability = Just LightningRod
      , imageUrl = imgUrlForAlternateForm "Marowak" "Alolan"
      , evolutionData = EvolvesFrom [ 138 ] "Level 28 at night"
      , transformationData = DoesNotTransform
      }
    , { id = 360
      , originalPokemonID = Nothing
      , fullName = "Tyrogue"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrlByName "Tyrogue"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 140
      , originalPokemonID = Nothing
      , fullName = "Hitmonlee"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrlByName "Hitmonlee"
      , evolutionData = EvolvesFrom [ 360 ] "Level 20 With Attack > Defense"
      , transformationData = DoesNotTransform
      }
    , { id = 141
      , originalPokemonID = Nothing
      , fullName = "Hitmonchan"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrlByName "Hitmonchan"
      , evolutionData = EvolvesFrom [ 360 ] "Level 20 With Attack < Defense"
      , transformationData = DoesNotTransform
      }
    , { id = 361
      , originalPokemonID = Nothing
      , fullName = "Hitmontop"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrlByName "Hitmontop"
      , evolutionData = EvolvesFrom [ 360 ] "Level 20 With Attack = Defense"
      , transformationData = DoesNotTransform
      }
    , { id = 142
      , originalPokemonID = Nothing
      , fullName = "Lickitung"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Lickitung"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 685
      , originalPokemonID = Nothing
      , fullName = "Lickilicky"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Lickilicky"
      , evolutionData = EvolvesFrom [ 142 ] "Level while knowing Rollout"
      , transformationData = DoesNotTransform
      }
    , { id = 143
      , originalPokemonID = Nothing
      , fullName = "Koffing"
      , typing = Single Poison
      , ability = Just Levitate
      , imageUrl = imageUrlByName "Koffing"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 144
      , originalPokemonID = Nothing
      , fullName = "Weezing"
      , typing = Single Poison
      , ability = Just Levitate
      , imageUrl = imageUrlByName "Weezing"
      , evolutionData = EvolvesFrom [ 143 ] "Level 35"
      , transformationData = DoesNotTransform
      }
    , { id = 1375
      , originalPokemonID = Just 144
      , fullName = nameFromData "Weezing" "Galarian" ""
      , typing = Double Poison Fairy
      , ability = Just Levitate
      , imageUrl = imgUrlForAlternateForm "Weezing" "Galarian"
      , evolutionData = EvolvesFrom [ 143 ] "Level 35 In Galar"
      , transformationData = DoesNotTransform
      }
    , { id = 145
      , originalPokemonID = Nothing
      , fullName = "Rhyhorn"
      , typing = Double Ground Rock
      , ability = Just LightningRod
      , imageUrl = imageUrlByName "Rhyhorn"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 146
      , originalPokemonID = Nothing
      , fullName = "Rhydon"
      , typing = Double Ground Rock
      , ability = Just LightningRod
      , imageUrl = imageUrlByName "Rhydon"
      , evolutionData = EvolvesFrom [ 145 ] "Level 42"
      , transformationData = DoesNotTransform
      }
    , { id = 686
      , originalPokemonID = Nothing
      , fullName = "Rhyperior"
      , typing = Double Ground Rock
      , ability = Just LightningRod
      , imageUrl = imageUrlByName "Rhyperior"
      , evolutionData = EvolvesFrom [ 146 ] "Trade holding Protector"
      , transformationData = DoesNotTransform
      }
    , { id = 638
      , originalPokemonID = Nothing
      , fullName = "Happiny"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Happiny"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 147
      , originalPokemonID = Nothing
      , fullName = "Chansey"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Chansey"
      , evolutionData = EvolvesFrom [ 638 ] "Level while holding an Oval Stone during the day"
      , transformationData = DoesNotTransform
      }
    , { id = 366
      , originalPokemonID = Nothing
      , fullName = "Blissey"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Blissey"
      , evolutionData = EvolvesFrom [ 147 ] "Level with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 148
      , originalPokemonID = Nothing
      , fullName = "Tangela"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Tangela"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 687
      , originalPokemonID = Nothing
      , fullName = "Tangrowth"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Tangrowth"
      , evolutionData = EvolvesFrom [ 148 ] "Level while knowing Ancient Power"
      , transformationData = DoesNotTransform
      }
    , { id = 211
      , originalPokemonID = Nothing
      , fullName = "Kangaskhan"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Kangaskhan"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1379
      , originalPokemonID = Nothing
      , fullName = nameFromData "Kangaskhan" "Mega" ""
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Kangaskhan" "Mega"
      , evolutionData = EvolvesFrom [ 211 ] "Holding Kangaskhanite"
      , transformationData = DoesNotTransform
      }
    , { id = 212
      , originalPokemonID = Nothing
      , fullName = "Horsea"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Horsea"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 213
      , originalPokemonID = Nothing
      , fullName = "Seadra"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Seadra"
      , evolutionData = EvolvesFrom [ 212 ] "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 354
      , originalPokemonID = Nothing
      , fullName = "Kingdra"
      , typing = Double Water Dragon
      , ability = Nothing
      , imageUrl = imageUrlByName "Kingdra"
      , evolutionData = EvolvesFrom [ 213 ] "Trade holding Dragon Scale"
      , transformationData = DoesNotTransform
      }
    , { id = 214
      , originalPokemonID = Nothing
      , fullName = "Goldeen"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Goldeen"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 215
      , originalPokemonID = Nothing
      , fullName = "Seaking"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Seaking"
      , evolutionData = EvolvesFrom [ 214 ] "Level 33"
      , transformationData = DoesNotTransform
      }
    , { id = 216
      , originalPokemonID = Nothing
      , fullName = "Staryu"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Staryu"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 217
      , originalPokemonID = Nothing
      , fullName = "Starmie"
      , typing = Double Water Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Starmie"
      , evolutionData = EvolvesFrom [ 216 ] "Use Water Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 637
      , originalPokemonID = Nothing
      , fullName = "Mime Jr."
      , typing = Double Psychic Fairy
      , ability = Nothing
      , imageUrl = imageUrlByName "Mime Jr."
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 218
      , originalPokemonID = Nothing
      , fullName = "Mr. Mime"
      , typing = Double Psychic Fairy
      , ability = Nothing
      , imageUrl = imageUrlByName "Mr. Mime"
      , evolutionData = EvolvesFrom [ 637 ] "Level while knowing Mimic"
      , transformationData = DoesNotTransform
      }
    , { id = 1377
      , originalPokemonID = Just 218
      , fullName = nameFromData "Mr. Mime" "Galarian" ""
      , typing = Double Ice Psychic
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Mr. Mime" "Galarian"
      , evolutionData = EvolvesFrom [ 637 ] "Level while knowing Mimic in Galar"
      , transformationData = DoesNotTransform
      }
    , { id = 1376
      , originalPokemonID = Nothing
      , fullName = "Mr. Rime"
      , typing = Double Ice Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Mr. Rime"
      , evolutionData = EvolvesFrom [ 1377 ] "Level 42"
      , transformationData = DoesNotTransform
      }
    , { id = 219
      , originalPokemonID = Nothing
      , fullName = "Scyther"
      , typing = Double Bug Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Scyther"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 336
      , originalPokemonID = Nothing
      , fullName = "Scizor"
      , typing = Double Bug Steel
      , ability = Nothing
      , imageUrl = imageUrlByName "Scizor"
      , evolutionData = EvolvesFrom [ 219 ] "Trade holding Metal Coat"
      , transformationData = DoesNotTransform
      }
    , { id = 1384
      , originalPokemonID = Nothing
      , fullName = nameFromData "Scizor" "Mega" ""
      , typing = Double Bug Steel
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Scizor" "Mega"
      , evolutionData = EvolvesFrom [ 336 ] "Holding Scizorite"
      , transformationData = DoesNotTransform
      }
    , { id = 1610
      , originalPokemonID = Nothing
      , fullName = "Kleavor"
      , typing = Double Bug Rock
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "legends-arceus" "kleavor"
      , evolutionData = EvolvesFrom [ 219 ] "Use Black Augurite"
      , transformationData = DoesNotTransform
      }
    , { id = 362
      , originalPokemonID = Nothing
      , fullName = "Smoochum"
      , typing = Double Ice Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Smoochum"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 220
      , originalPokemonID = Nothing
      , fullName = "Jynx"
      , typing = Double Ice Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Jynx"
      , evolutionData = EvolvesFrom [ 362 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 363
      , originalPokemonID = Nothing
      , fullName = "Elekid"
      , typing = Single Electric
      , ability = Nothing
      , imageUrl = imageUrlByName "Elekid"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 221
      , originalPokemonID = Nothing
      , fullName = "Electabuzz"
      , typing = Single Electric
      , ability = Nothing
      , imageUrl = imageUrlByName "Electabuzz"
      , evolutionData = EvolvesFrom [ 363 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 688
      , originalPokemonID = Nothing
      , fullName = "Electivire"
      , typing = Single Electric
      , ability = Just MotorDrive
      , imageUrl = imageUrlByName "Electivire"
      , evolutionData = EvolvesFrom [ 221 ] "Trade holding Electirizer"
      , transformationData = DoesNotTransform
      }
    , { id = 364
      , originalPokemonID = Nothing
      , fullName = "Magby"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrlByName "Magby"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 222
      , originalPokemonID = Nothing
      , fullName = "Magmar"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrlByName "Magmar"
      , evolutionData = EvolvesFrom [ 364 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 699
      , originalPokemonID = Nothing
      , fullName = "Magmortar"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrlByName "Magmortar"
      , evolutionData = EvolvesFrom [ 222 ] "Trade holding Magmarizer"
      , transformationData = DoesNotTransform
      }
    , { id = 223
      , originalPokemonID = Nothing
      , fullName = "Pinsir"
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imageUrlByName "Pinsir"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1380
      , originalPokemonID = Nothing
      , fullName = nameFromData "Pinsir" "Mega" ""
      , typing = Double Bug Flying
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Pinsir" "Mega"
      , evolutionData = EvolvesFrom [ 223 ] "Holding Pinsirite"
      , transformationData = DoesNotTransform
      }
    , { id = 224
      , originalPokemonID = Nothing
      , fullName = "Tauros"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Tauros"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 225
      , originalPokemonID = Nothing
      , fullName = "Magikarp"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Magikarp"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 226
      , originalPokemonID = Nothing
      , fullName = "Gyarados"
      , typing = Double Water Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Gyarados"
      , evolutionData = EvolvesFrom [ 225 ] "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 1381
      , originalPokemonID = Nothing
      , fullName = nameFromData "Gyarados" "Mega" ""
      , typing = Double Water Dark
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Gyarados" "Mega"
      , evolutionData = EvolvesFrom [ 226 ] "Holding Gyaradosite"
      , transformationData = DoesNotTransform
      }
    , { id = 227
      , originalPokemonID = Nothing
      , fullName = "Lapras"
      , typing = Double Water Ice
      , ability = Just WaterAbsorb
      , imageUrl = imageUrlByName "Lapras"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 29
      , originalPokemonID = Nothing
      , fullName = "Ditto"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Ditto"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 228
      , originalPokemonID = Nothing
      , fullName = "Eevee"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Eevee"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 229
      , originalPokemonID = Nothing
      , fullName = "Vaporeon"
      , typing = Single Water
      , ability = Just WaterAbsorb
      , imageUrl = imageUrlByName "Vaporeon"
      , evolutionData = EvolvesFrom [ 228 ] "Use Water Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 230
      , originalPokemonID = Nothing
      , fullName = "Jolteon"
      , typing = Single Electric
      , ability = Just VoltAbsorb
      , imageUrl = imageUrlByName "Jolteon"
      , evolutionData = EvolvesFrom [ 228 ] "Use Thunder Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 231
      , originalPokemonID = Nothing
      , fullName = "Flareon"
      , typing = Single Fire
      , ability = Just FlashFire
      , imageUrl = imageUrlByName "Flareon"
      , evolutionData = EvolvesFrom [ 228 ] "Use Fire Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 296
      , originalPokemonID = Nothing
      , fullName = "Espeon"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Espeon"
      , evolutionData = EvolvesFrom [ 228 ] "Level during the day with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 297
      , originalPokemonID = Nothing
      , fullName = "Umbreon"
      , typing = Single Dark
      , ability = Nothing
      , imageUrl = imageUrlByName "Umbreon"
      , evolutionData = EvolvesFrom [ 228 ] "Level during the night with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 715
      , originalPokemonID = Nothing
      , fullName = "Leafeon"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Leafeon"
      , evolutionData = EvolvesFrom [ 228 ] "Use Leaf Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 716
      , originalPokemonID = Nothing
      , fullName = "Glaceon"
      , typing = Single Ice
      , ability = Nothing
      , imageUrl = imageUrlByName "Glaceon"
      , evolutionData = EvolvesFrom [ 228 ] "Use Ice Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 1129
      , originalPokemonID = Nothing
      , fullName = "Sylveon"
      , typing = Single Fairy
      , ability = Nothing
      , imageUrl = imageUrlByName "Sylveon"
      , evolutionData = EvolvesFrom [ 228 ] "Level while knowing a Fairy move with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 232
      , originalPokemonID = Nothing
      , fullName = "Porygon"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Porygon"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 357
      , originalPokemonID = Nothing
      , fullName = "Porygon2"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Porygon2"
      , evolutionData = EvolvesFrom [ 232 ] "Trade holding Upgrade"
      , transformationData = DoesNotTransform
      }
    , { id = 719
      , originalPokemonID = Nothing
      , fullName = "Porygon-Z"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Porygon-Z"
      , evolutionData = EvolvesFrom [ 357 ] "Trade holding Dubious Disc"
      , transformationData = DoesNotTransform
      }
    , { id = 233
      , originalPokemonID = Nothing
      , fullName = "Omanyte"
      , typing = Double Rock Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Omanyte"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 234
      , originalPokemonID = Nothing
      , fullName = "Omastar"
      , typing = Double Rock Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Omastar"
      , evolutionData = EvolvesFrom [ 233 ] "Level 40"
      , transformationData = DoesNotTransform
      }
    , { id = 235
      , originalPokemonID = Nothing
      , fullName = "Kabuto"
      , typing = Double Rock Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Kabuto"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 236
      , originalPokemonID = Nothing
      , fullName = "Kabutops"
      , typing = Double Rock Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Kabutops"
      , evolutionData = EvolvesFrom [ 235 ] "Level 40"
      , transformationData = DoesNotTransform
      }
    , { id = 237
      , originalPokemonID = Nothing
      , fullName = "Aerodactyl"
      , typing = Double Rock Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Aerodactyl"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1382
      , originalPokemonID = Nothing
      , fullName = nameFromData "Aerodactyl" "Mega" ""
      , typing = Double Rock Flying
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Aerodactyl" "Mega"
      , evolutionData = EvolvesFrom [ 237 ] "Holding Aerodactylite"
      , transformationData = DoesNotTransform
      }
    , { id = 644
      , originalPokemonID = Nothing
      , fullName = "Munchlax"
      , typing = Single Normal
      , ability = Just ThickFat
      , imageUrl = imageUrlByName "Munchlax"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 238
      , originalPokemonID = Nothing
      , fullName = "Snorlax"
      , typing = Single Normal
      , ability = Just ThickFat
      , imageUrl = imageUrlByName "Snorlax"
      , evolutionData = EvolvesFrom [ 644 ] "Level with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 239
      , originalPokemonID = Nothing
      , fullName = "Articuno"
      , typing = Double Ice Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Articuno"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1385
      , originalPokemonID = Just 239
      , fullName = nameFromData "Articuno" "Galarian" ""
      , typing = Double Psychic Flying
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Articuno" "Galarian"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 240
      , originalPokemonID = Nothing
      , fullName = "Zapdos"
      , typing = Double Electric Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Zapdos"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1386
      , originalPokemonID = Just 240
      , fullName = nameFromData "Zapdos" "Galarian" ""
      , typing = Double Fighting Flying
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Zapdos" "Galarian"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 241
      , originalPokemonID = Nothing
      , fullName = "Moltres"
      , typing = Double Fire Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Moltres"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1387
      , originalPokemonID = Just 241
      , fullName = nameFromData "Moltres" "Galarian" ""
      , typing = Double Dark Flying
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Moltres" "Galarian"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 242
      , originalPokemonID = Nothing
      , fullName = "Dratini"
      , typing = Single Dragon
      , ability = Nothing
      , imageUrl = imageUrlByName "Dratini"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 243
      , originalPokemonID = Nothing
      , fullName = "Dragonair"
      , typing = Single Dragon
      , ability = Nothing
      , imageUrl = imageUrlByName "Dragonair"
      , evolutionData = EvolvesFrom [ 242 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 244
      , originalPokemonID = Nothing
      , fullName = "Dragonite"
      , typing = Double Dragon Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Dragonite"
      , evolutionData = EvolvesFrom [ 243 ] "Level 55"
      , transformationData = DoesNotTransform
      }
    , { id = 43
      , originalPokemonID = Nothing
      , fullName = "Mewtwo"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Mewtwo"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 44
      , originalPokemonID = Nothing
      , fullName = nameFromData "Mewtwo" "Mega X" "Mega Mewtwo X"
      , typing = Double Psychic Fighting
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Mewtwo" "Mega X"
      , evolutionData = EvolvesFrom [ 43 ] "Holding Mewtwonite X"
      , transformationData = DoesNotTransform
      }
    , { id = 45
      , originalPokemonID = Nothing
      , fullName = nameFromData "Mewtwo" "Mega Y" "Mega Mewtwo Y"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Mewtwo" "Mega Y"
      , evolutionData = EvolvesFrom [ 43 ] "Holding Mewtwonite Y"
      , transformationData = DoesNotTransform
      }
    , { id = 42
      , originalPokemonID = Nothing
      , fullName = "Mew"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Mew"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 245
      , originalPokemonID = Nothing
      , fullName = "Chikorita"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Chikorita"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 246
      , originalPokemonID = Nothing
      , fullName = "Bayleef"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Bayleef"
      , evolutionData = EvolvesFrom [ 245 ] "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 247
      , originalPokemonID = Nothing
      , fullName = "Meganium"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Meganium"
      , evolutionData = EvolvesFrom [ 246 ] "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 248
      , originalPokemonID = Nothing
      , fullName = "Cyndaquil"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrlByName "Cyndaquil"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 249
      , originalPokemonID = Nothing
      , fullName = "Quilava"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrlByName "Quilava"
      , evolutionData = EvolvesFrom [ 248 ] "Level 14 (17 in Legends: Arceus)"
      , transformationData = DoesNotTransform
      }
    , { id = 250
      , originalPokemonID = Nothing
      , fullName = "Typhlosion"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrlByName "Typhlosion"
      , evolutionData = EvolvesFrom [ 249 ] "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 1623
      , originalPokemonID = Just 250
      , fullName = nameFromData "Typhlosion" "Hisuian" ""
      , typing = Double Fire Ghost
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Typhlosion" "Hisuian"
      , evolutionData = EvolvesFrom [ 249 ] "Level 36 in Legends: Arceus"
      , transformationData = DoesNotTransform
      }
    , { id = 251
      , originalPokemonID = Nothing
      , fullName = "Totodile"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Totodile"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 252
      , originalPokemonID = Nothing
      , fullName = "Croconaw"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Croconaw"
      , evolutionData = EvolvesFrom [ 251 ] "Level 18"
      , transformationData = DoesNotTransform
      }
    , { id = 253
      , originalPokemonID = Nothing
      , fullName = "Feraligatr"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Feraligatr"
      , evolutionData = EvolvesFrom [ 252 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 254
      , originalPokemonID = Nothing
      , fullName = "Sentret"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Sentret"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 255
      , originalPokemonID = Nothing
      , fullName = "Furret"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Furret"
      , evolutionData = EvolvesFrom [ 254 ] "Level 15"
      , transformationData = DoesNotTransform
      }
    , { id = 256
      , originalPokemonID = Nothing
      , fullName = "Hoothoot"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Hoothoot"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 257
      , originalPokemonID = Nothing
      , fullName = "Noctowl"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Noctowl"
      , evolutionData = EvolvesFrom [ 256 ] "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 258
      , originalPokemonID = Nothing
      , fullName = "Ledyba"
      , typing = Double Bug Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Ledyba"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 259
      , originalPokemonID = Nothing
      , fullName = "Ledian"
      , typing = Double Bug Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Ledian"
      , evolutionData = EvolvesFrom [ 258 ] "Level 18"
      , transformationData = DoesNotTransform
      }
    , { id = 260
      , originalPokemonID = Nothing
      , fullName = "Spinarak"
      , typing = Double Bug Poison
      , ability = Nothing
      , imageUrl = imageUrlByName "Spinarak"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 261
      , originalPokemonID = Nothing
      , fullName = "Ariados"
      , typing = Double Bug Poison
      , ability = Nothing
      , imageUrl = imageUrlByName "Ariados"
      , evolutionData = EvolvesFrom [ 260 ] "Level 22"
      , transformationData = DoesNotTransform
      }
    , { id = 262
      , originalPokemonID = Nothing
      , fullName = "Chinchou"
      , typing = Double Water Electric
      , ability = Just VoltAbsorb
      , imageUrl = imageUrlByName "Chinchou"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 263
      , originalPokemonID = Nothing
      , fullName = "Lanturn"
      , typing = Double Water Electric
      , ability = Just VoltAbsorb
      , imageUrl = imageUrlByName "Lanturn"
      , evolutionData = EvolvesFrom [ 262 ] "Level 27"
      , transformationData = DoesNotTransform
      }
    , { id = 264
      , originalPokemonID = Nothing
      , fullName = "Togepi"
      , typing = Single Fairy
      , ability = Nothing
      , imageUrl = imageUrlByName "Togepi"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 265
      , originalPokemonID = Nothing
      , fullName = "Togetic"
      , typing = Double Fairy Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Togetic"
      , evolutionData = EvolvesFrom [ 264 ] "Level with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 713
      , originalPokemonID = Nothing
      , fullName = "Togekiss"
      , typing = Double Fairy Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Togekiss"
      , evolutionData = EvolvesFrom [ 265 ] "Use Shiny Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 266
      , originalPokemonID = Nothing
      , fullName = "Natu"
      , typing = Double Psychic Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Natu"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 267
      , originalPokemonID = Nothing
      , fullName = "Xatu"
      , typing = Double Psychic Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Xatu"
      , evolutionData = EvolvesFrom [ 266 ] "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 278
      , originalPokemonID = Nothing
      , fullName = "Mareep"
      , typing = Single Electric
      , ability = Nothing
      , imageUrl = imageUrlByName "Mareep"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 279
      , originalPokemonID = Nothing
      , fullName = "Flaaffy"
      , typing = Single Electric
      , ability = Nothing
      , imageUrl = imageUrlByName "Flaaffy"
      , evolutionData = EvolvesFrom [ 278 ] "Level 15"
      , transformationData = DoesNotTransform
      }
    , { id = 280
      , originalPokemonID = Nothing
      , fullName = "Ampharos"
      , typing = Single Electric
      , ability = Nothing
      , imageUrl = imageUrlByName "Ampharos"
      , evolutionData = EvolvesFrom [ 279 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 1394
      , originalPokemonID = Nothing
      , fullName = nameFromData "Ampharos" "Mega" ""
      , typing = Double Electric Dragon
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Ampharos" "Mega"
      , evolutionData = EvolvesFrom [ 280 ] "Holding Ampharosite"
      , transformationData = DoesNotTransform
      }
    , { id = 426
      , originalPokemonID = Nothing
      , fullName = "Azurill"
      , typing = Double Normal Fairy
      , ability = Just ThickFat
      , imageUrl = imageUrlByName "Azurill"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 283
      , originalPokemonID = Nothing
      , fullName = "Marill"
      , typing = Double Water Fairy
      , ability = Just ThickFat
      , imageUrl = imageUrlByName "Marill"
      , evolutionData = EvolvesFrom [ 426 ] "Level with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 284
      , originalPokemonID = Nothing
      , fullName = "Azumarill"
      , typing = Double Water Fairy
      , ability = Just ThickFat
      , imageUrl = imageUrlByName "Azumarill"
      , evolutionData = EvolvesFrom [ 283 ] "Level 18"
      , transformationData = DoesNotTransform
      }
    , { id = 636
      , originalPokemonID = Nothing
      , fullName = "Bonsly"
      , typing = Single Rock
      , ability = Nothing
      , imageUrl = imageUrlByName "Bonsly"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 285
      , originalPokemonID = Nothing
      , fullName = "Sudowoodo"
      , typing = Single Rock
      , ability = Nothing
      , imageUrl = imageUrlByName "Sudowoodo"
      , evolutionData = EvolvesFrom [ 636 ] "Level while knowing Mimic"
      , transformationData = DoesNotTransform
      }
    , { id = 287
      , originalPokemonID = Nothing
      , fullName = "Hoppip"
      , typing = Double Grass Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Hoppip"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 288
      , originalPokemonID = Nothing
      , fullName = "Skiploom"
      , typing = Double Grass Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Skiploom"
      , evolutionData = EvolvesFrom [ 287 ] "Level 18"
      , transformationData = DoesNotTransform
      }
    , { id = 289
      , originalPokemonID = Nothing
      , fullName = "Jumpluff"
      , typing = Double Grass Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Jumpluff"
      , evolutionData = EvolvesFrom [ 288 ] "Level 27"
      , transformationData = DoesNotTransform
      }
    , { id = 290
      , originalPokemonID = Nothing
      , fullName = "Aipom"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Aipom"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 622
      , originalPokemonID = Nothing
      , fullName = "Ambipom"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Ambipom"
      , evolutionData = EvolvesFrom [ 290 ] "Level while knowing Double Hit"
      , transformationData = DoesNotTransform
      }
    , { id = 291
      , originalPokemonID = Nothing
      , fullName = "Sunkern"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Sunkern"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 292
      , originalPokemonID = Nothing
      , fullName = "Sunflora"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Sunflora"
      , evolutionData = EvolvesFrom [ 291 ] "Use Sun Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 293
      , originalPokemonID = Nothing
      , fullName = "Yanma"
      , typing = Double Bug Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Yanma"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 714
      , originalPokemonID = Nothing
      , fullName = "Yanmega"
      , typing = Double Bug Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Yanmega"
      , evolutionData = EvolvesFrom [ 293 ] "Level while knowing Ancient Power"
      , transformationData = DoesNotTransform
      }
    , { id = 294
      , originalPokemonID = Nothing
      , fullName = "Wooper"
      , typing = Double Water Ground
      , ability = Just WaterAbsorb
      , imageUrl = imageUrlByName "Wooper"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 295
      , originalPokemonID = Nothing
      , fullName = "Quagsire"
      , typing = Double Water Ground
      , ability = Just WaterAbsorb
      , imageUrl = imageUrlByName "Quagsire"
      , evolutionData = EvolvesFrom [ 294 ] "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 1651
      , originalPokemonID = Just 294
      , fullName = nameFromData "Wooper" "Paldean" ""
      , typing = Double Poison Ground
      , ability = Just WaterAbsorb
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "wooper-paldean"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1652
      , originalPokemonID = Nothing
      , fullName = "Clodsire"
      , typing = Double Poison Ground
      , ability = Just WaterAbsorb
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "clodsire"
      , evolutionData = EvolvesFrom [ 1651 ] "Level 28"
      , transformationData = DoesNotTransform
      }
    , { id = 298
      , originalPokemonID = Nothing
      , fullName = "Murkrow"
      , typing = Double Dark Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Murkrow"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 628
      , originalPokemonID = Nothing
      , fullName = "Honchkrow"
      , typing = Double Dark Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Honchkrow"
      , evolutionData = EvolvesFrom [ 298 ] "Use Dusk Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 324
      , originalPokemonID = Nothing
      , fullName = "Misdreavus"
      , typing = Single Ghost
      , ability = Just Levitate
      , imageUrl = imageUrlByName "Misdreavus"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 627
      , originalPokemonID = Nothing
      , fullName = "Mismagius"
      , typing = Single Ghost
      , ability = Just Levitate
      , imageUrl = imageUrlByName "Mismagius"
      , evolutionData = EvolvesFrom [ 324 ] "Use Dusk Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 325
      , originalPokemonID = Nothing
      , fullName = "Unown"
      , typing = Single Psychic
      , ability = Just Levitate
      , imageUrl = imageUrlByName "Unown"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 488
      , originalPokemonID = Nothing
      , fullName = "Wynaut"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Wynaut"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 326
      , originalPokemonID = Nothing
      , fullName = "Wobbuffet"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Wobbuffet"
      , evolutionData = EvolvesFrom [ 488 ] "Level 15"
      , transformationData = DoesNotTransform
      }
    , { id = 327
      , originalPokemonID = Nothing
      , fullName = "Girafarig"
      , typing = Double Normal Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Girafarig"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1673
      , originalPokemonID = Nothing
      , fullName = "Farigiraf"
      , typing = Double Normal Psychic
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "farigiraf"
      , evolutionData = EvolvesFrom [ 327 ] "Level while knowing Twin Beam"
      , transformationData = DoesNotTransform
      }
    , { id = 328
      , originalPokemonID = Nothing
      , fullName = "Pineco"
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imageUrlByName "Pineco"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 329
      , originalPokemonID = Nothing
      , fullName = "Forretress"
      , typing = Double Bug Steel
      , ability = Nothing
      , imageUrl = imageUrlByName "Forretress"
      , evolutionData = EvolvesFrom [ 328 ] "Level 31"
      , transformationData = DoesNotTransform
      }
    , { id = 330
      , originalPokemonID = Nothing
      , fullName = "Dunsparce"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Dunsparce"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1672
      , originalPokemonID = Nothing
      , fullName = "Dudunsparce"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "dudunsparce"
      , evolutionData = EvolvesFrom [ 330 ] "Level while knowing Hyper Drill"
      , transformationData = DoesNotTransform
      }
    , { id = 331
      , originalPokemonID = Nothing
      , fullName = "Gligar"
      , typing = Double Ground Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Gligar"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 717
      , originalPokemonID = Nothing
      , fullName = "Gliscor"
      , typing = Double Ground Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Gliscor"
      , evolutionData = EvolvesFrom [ 331 ] "Level while holding Razor Fang at night"
      , transformationData = DoesNotTransform
      }
    , { id = 333
      , originalPokemonID = Nothing
      , fullName = "Snubbull"
      , typing = Single Fairy
      , ability = Nothing
      , imageUrl = imageUrlByName "Snubbull"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 334
      , originalPokemonID = Nothing
      , fullName = "Granbull"
      , typing = Single Fairy
      , ability = Nothing
      , imageUrl = imageUrlByName "Granbull"
      , evolutionData = EvolvesFrom [ 333 ] "Level 23"
      , transformationData = DoesNotTransform
      }
    , { id = 335
      , originalPokemonID = Nothing
      , fullName = "Qwilfish"
      , typing = Double Water Poison
      , ability = Nothing
      , imageUrl = imageUrlByName "Qwilfish"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1615
      , originalPokemonID = Just 335
      , fullName = nameFromData "Qwilfish" "Hisuian" ""
      , typing = Double Dark Poison
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Qwilfish" "Hisuian"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1616
      , originalPokemonID = Nothing
      , fullName = "Overqwil"
      , typing = Double Dark Poison
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "legends-arceus" "overqwil"
      , evolutionData = EvolvesFrom [ 1615 ] "Use Barb Barrage in Strong Style 20 times"
      , transformationData = DoesNotTransform
      }
    , { id = 337
      , originalPokemonID = Nothing
      , fullName = "Shuckle"
      , typing = Double Bug Rock
      , ability = Nothing
      , imageUrl = imageUrlByName "Shuckle"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 338
      , originalPokemonID = Nothing
      , fullName = "Heracross"
      , typing = Double Bug Fighting
      , ability = Nothing
      , imageUrl = imageUrlByName "Heracross"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1395
      , originalPokemonID = Nothing
      , fullName = nameFromData "Heracross" "Mega" ""
      , typing = Double Bug Fighting
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Heracross" "Mega"
      , evolutionData = EvolvesFrom [ 338 ] "Holding Heracronite"
      , transformationData = DoesNotTransform
      }
    , { id = 339
      , originalPokemonID = Nothing
      , fullName = "Sneasel"
      , typing = Double Dark Ice
      , ability = Nothing
      , imageUrl = imageUrlByName "Sneasel"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 683
      , originalPokemonID = Nothing
      , fullName = "Weavile"
      , typing = Double Dark Ice
      , ability = Nothing
      , imageUrl = imageUrlByName "Weavile"
      , evolutionData = EvolvesFrom [ 339 ] "Level while holding Razor Claw at night"
      , transformationData = DoesNotTransform
      }
    , { id = 1613
      , originalPokemonID = Just 339
      , fullName = nameFromData "Sneasel" "Hisuian" ""
      , typing = Double Fighting Poison
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Sneasel" "Hisuian"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1614
      , originalPokemonID = Nothing
      , fullName = "Sneasler"
      , typing = Double Fighting Poison
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "legends-arceus" "sneasler"
      , evolutionData = EvolvesFrom [ 1613 ] "Use Razor Claw during the day"
      , transformationData = DoesNotTransform
      }
    , { id = 340
      , originalPokemonID = Nothing
      , fullName = "Teddiursa"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Teddiursa"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 341
      , originalPokemonID = Nothing
      , fullName = "Ursaring"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Ursaring"
      , evolutionData = EvolvesFrom [ 340 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 1611
      , originalPokemonID = Nothing
      , fullName = "Ursaluna"
      , typing = Double Normal Ground
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "legends-arceus" "ursaluna"
      , evolutionData = EvolvesFrom [ 341 ] "Use Peat Block under a full moon"
      , transformationData = DoesNotTransform
      }
    , { id = 342
      , originalPokemonID = Nothing
      , fullName = "Slugma"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrlByName "Slugma"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 343
      , originalPokemonID = Nothing
      , fullName = "Magcargo"
      , typing = Double Fire Rock
      , ability = Nothing
      , imageUrl = imageUrlByName "Magcargo"
      , evolutionData = EvolvesFrom [ 342 ] "Level 38"
      , transformationData = DoesNotTransform
      }
    , { id = 344
      , originalPokemonID = Nothing
      , fullName = "Swinub"
      , typing = Double Ice Ground
      , ability = Nothing
      , imageUrl = imageUrlByName "Swinub"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 345
      , originalPokemonID = Nothing
      , fullName = "Piloswine"
      , typing = Double Ice Ground
      , ability = Nothing
      , imageUrl = imageUrlByName "Piloswine"
      , evolutionData = EvolvesFrom [ 344 ] "Level 33"
      , transformationData = DoesNotTransform
      }
    , { id = 718
      , originalPokemonID = Nothing
      , fullName = "Mamoswine"
      , typing = Double Ice Ground
      , ability = Nothing
      , imageUrl = imageUrlByName "Mamoswine"
      , evolutionData = EvolvesFrom [ 345 ] "Level while knowing Ancient Power"
      , transformationData = DoesNotTransform
      }
    , { id = 346
      , originalPokemonID = Nothing
      , fullName = "Corsola"
      , typing = Double Water Rock
      , ability = Nothing
      , imageUrl = imageUrlByName "Corsola"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1389
      , originalPokemonID = Just 346
      , fullName = nameFromData "Corsola" "Galarian" ""
      , typing = Single Ghost
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Corsola" "Galarian"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1390
      , originalPokemonID = Nothing
      , fullName = "Cursola"
      , typing = Single Ghost
      , ability = Nothing
      , imageUrl = imageUrlByName "Cursola"
      , evolutionData = EvolvesFrom [ 1389 ] "Level 38"
      , transformationData = DoesNotTransform
      }
    , { id = 347
      , originalPokemonID = Nothing
      , fullName = "Remoraid"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Remoraid"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 348
      , originalPokemonID = Nothing
      , fullName = "Octillery"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Octillery"
      , evolutionData = EvolvesFrom [ 347 ] "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 349
      , originalPokemonID = Nothing
      , fullName = "Delibird"
      , typing = Double Ice Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Delibird"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 656
      , originalPokemonID = Nothing
      , fullName = "Mantyke"
      , typing = Double Water Flying
      , ability = Just WaterAbsorb
      , imageUrl = imageUrlByName "Mantyke"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 350
      , originalPokemonID = Nothing
      , fullName = "Mantine"
      , typing = Double Water Flying
      , ability = Just WaterAbsorb
      , imageUrl = imageUrlByName "Mantine"
      , evolutionData = EvolvesFrom [ 656 ] "Level with Remoraid in party"
      , transformationData = DoesNotTransform
      }
    , { id = 351
      , originalPokemonID = Nothing
      , fullName = "Skarmory"
      , typing = Double Steel Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Skarmory"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 352
      , originalPokemonID = Nothing
      , fullName = "Houndour"
      , typing = Double Dark Fire
      , ability = Just FlashFire
      , imageUrl = imageUrlByName "Houndour"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 353
      , originalPokemonID = Nothing
      , fullName = "Houndoom"
      , typing = Double Dark Fire
      , ability = Just FlashFire
      , imageUrl = imageUrlByName "Houndoom"
      , evolutionData = EvolvesFrom [ 352 ] "Level 24"
      , transformationData = DoesNotTransform
      }
    , { id = 1396
      , originalPokemonID = Nothing
      , fullName = nameFromData "Houndoom" "Mega" ""
      , typing = Double Dark Fire
      , ability = Just FlashFire
      , imageUrl = imgUrlForAlternateForm "Houndoom" "Mega"
      , evolutionData = EvolvesFrom [ 353 ] "Holding Houndoominite"
      , transformationData = DoesNotTransform
      }
    , { id = 355
      , originalPokemonID = Nothing
      , fullName = "Phanpy"
      , typing = Single Ground
      , ability = Nothing
      , imageUrl = imageUrlByName "Phanpy"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 356
      , originalPokemonID = Nothing
      , fullName = "Donphan"
      , typing = Single Ground
      , ability = Nothing
      , imageUrl = imageUrlByName "Donphan"
      , evolutionData = EvolvesFrom [ 355 ] "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 358
      , originalPokemonID = Nothing
      , fullName = "Stantler"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Stantler"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1609
      , originalPokemonID = Nothing
      , fullName = "Wyrdeer"
      , typing = Double Normal Psychic
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "legends-arceus" "wyrdeer"
      , evolutionData = EvolvesFrom [ 358 ] "Use Psyshield Bash 20 times in Agile Style"
      , transformationData = DoesNotTransform
      }
    , { id = 359
      , originalPokemonID = Nothing
      , fullName = "Smeargle"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Smeargle"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 365
      , originalPokemonID = Nothing
      , fullName = "Miltank"
      , typing = Single Normal
      , ability = Just ThickFat
      , imageUrl = imageUrlByName "Miltank"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 367
      , originalPokemonID = Nothing
      , fullName = "Raikou"
      , typing = Single Electric
      , ability = Nothing
      , imageUrl = imageUrlByName "Raikou"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 368
      , originalPokemonID = Nothing
      , fullName = "Entei"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrlByName "Entei"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 369
      , originalPokemonID = Nothing
      , fullName = "Suicune"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Suicune"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 370
      , originalPokemonID = Nothing
      , fullName = "Larvitar"
      , typing = Double Rock Ground
      , ability = Nothing
      , imageUrl = imageUrlByName "Larvitar"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 371
      , originalPokemonID = Nothing
      , fullName = "Pupitar"
      , typing = Double Rock Ground
      , ability = Nothing
      , imageUrl = imageUrlByName "Pupitar"
      , evolutionData = EvolvesFrom [ 370 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 372
      , originalPokemonID = Nothing
      , fullName = "Tyranitar"
      , typing = Double Rock Dark
      , ability = Nothing
      , imageUrl = imageUrlByName "Tyranitar"
      , evolutionData = EvolvesFrom [ 371 ] "Level 55"
      , transformationData = DoesNotTransform
      }
    , { id = 1397
      , originalPokemonID = Nothing
      , fullName = nameFromData "Tyranitar" "Mega" ""
      , typing = Double Rock Dark
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Tyranitar" "Mega"
      , evolutionData = EvolvesFrom [ 372 ] "Holding Tyranitarite"
      , transformationData = DoesNotTransform
      }
    , { id = 373
      , originalPokemonID = Nothing
      , fullName = "Lugia"
      , typing = Double Psychic Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Lugia"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 374
      , originalPokemonID = Nothing
      , fullName = "Ho-Oh"
      , typing = Double Fire Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Ho-Oh"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 375
      , originalPokemonID = Nothing
      , fullName = "Celebi"
      , typing = Double Psychic Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Celebi"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 380
      , originalPokemonID = Nothing
      , fullName = "Treecko"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Treecko"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 381
      , originalPokemonID = Nothing
      , fullName = "Grovyle"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Grovyle"
      , evolutionData = EvolvesFrom [ 380 ] "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 382
      , originalPokemonID = Nothing
      , fullName = "Sceptile"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Sceptile"
      , evolutionData = EvolvesFrom [ 381 ] "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 1428
      , originalPokemonID = Nothing
      , fullName = nameFromData "Sceptile" "Mega" ""
      , typing = Double Grass Dragon
      , ability = Just LightningRod
      , imageUrl = imgUrlForAlternateForm "Sceptile" "Mega"
      , evolutionData = EvolvesFrom [ 382 ] "Holding Sceptilite"
      , transformationData = DoesNotTransform
      }
    , { id = 383
      , originalPokemonID = Nothing
      , fullName = "Torchic"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrlByName "Torchic"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 384
      , originalPokemonID = Nothing
      , fullName = "Combusken"
      , typing = Double Fire Fighting
      , ability = Nothing
      , imageUrl = imageUrlByName "Combusken"
      , evolutionData = EvolvesFrom [ 383 ] "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 385
      , originalPokemonID = Nothing
      , fullName = "Blaziken"
      , typing = Double Fire Fighting
      , ability = Nothing
      , imageUrl = imageUrlByName "Blaziken"
      , evolutionData = EvolvesFrom [ 384 ] "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 1429
      , originalPokemonID = Nothing
      , fullName = nameFromData "Blaziken" "Mega" ""
      , typing = Double Fire Fighting
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Blaziken" "Mega"
      , evolutionData = EvolvesFrom [ 385 ] "Holding Blazikenite"
      , transformationData = DoesNotTransform
      }
    , { id = 386
      , originalPokemonID = Nothing
      , fullName = "Mudkip"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Mudkip"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 387
      , originalPokemonID = Nothing
      , fullName = "Marshtomp"
      , typing = Double Water Ground
      , ability = Nothing
      , imageUrl = imageUrlByName "Marshtomp"
      , evolutionData = EvolvesFrom [ 386 ] "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 388
      , originalPokemonID = Nothing
      , fullName = "Swampert"
      , typing = Double Water Ground
      , ability = Nothing
      , imageUrl = imageUrlByName "Swampert"
      , evolutionData = EvolvesFrom [ 387 ] "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 1430
      , originalPokemonID = Nothing
      , fullName = nameFromData "Swampert" "Mega" ""
      , typing = Double Water Ground
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Swampert" "Mega"
      , evolutionData = EvolvesFrom [ 388 ] "Holding Swampertite"
      , transformationData = DoesNotTransform
      }
    , { id = 389
      , originalPokemonID = Nothing
      , fullName = "Poochyena"
      , typing = Single Dark
      , ability = Nothing
      , imageUrl = imageUrlByName "Poochyena"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 390
      , originalPokemonID = Nothing
      , fullName = "Mightyena"
      , typing = Single Dark
      , ability = Nothing
      , imageUrl = imageUrlByName "Mightyena"
      , evolutionData = EvolvesFrom [ 389 ] "Level 18"
      , transformationData = DoesNotTransform
      }
    , { id = 391
      , originalPokemonID = Nothing
      , fullName = "Zigzagoon"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Zigzagoon"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 392
      , originalPokemonID = Nothing
      , fullName = "Linoone"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Linoone"
      , evolutionData = EvolvesFrom [ 391 ] "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 1466
      , originalPokemonID = Just 391
      , fullName = nameFromData "Zigzagoon" "Galarian" ""
      , typing = Double Dark Normal
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Zigzagoon" "Galarian"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1467
      , originalPokemonID = Just 392
      , fullName = nameFromData "Linoone" "Galarian" ""
      , typing = Double Dark Normal
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Linoone" "Galarian"
      , evolutionData = EvolvesFrom [ 1466 ] "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 1468
      , originalPokemonID = Nothing
      , fullName = "Obstagoon"
      , typing = Double Dark Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Obstagoon"
      , evolutionData = EvolvesFrom [ 1467 ] "Level 35 at night"
      , transformationData = DoesNotTransform
      }
    , { id = 393
      , originalPokemonID = Nothing
      , fullName = "Wurmple"
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imageUrlByName "Wurmple"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 394
      , originalPokemonID = Nothing
      , fullName = "Silcoon"
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imageUrlByName "Silcoon"
      , evolutionData = EvolvesFrom [ 393 ] "Level 7 (random)"
      , transformationData = DoesNotTransform
      }
    , { id = 395
      , originalPokemonID = Nothing
      , fullName = "Beautifly"
      , typing = Double Bug Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Beautifly"
      , evolutionData = EvolvesFrom [ 394 ] "Level 10"
      , transformationData = DoesNotTransform
      }
    , { id = 396
      , originalPokemonID = Nothing
      , fullName = "Cascoon"
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imageUrlByName "Cascoon"
      , evolutionData = EvolvesFrom [ 393 ] "Level 7 (random)"
      , transformationData = DoesNotTransform
      }
    , { id = 397
      , originalPokemonID = Nothing
      , fullName = "Dustox"
      , typing = Double Bug Poison
      , ability = Nothing
      , imageUrl = imageUrlByName "Dustox"
      , evolutionData = EvolvesFrom [ 396 ] "Level 10"
      , transformationData = DoesNotTransform
      }
    , { id = 398
      , originalPokemonID = Nothing
      , fullName = "Lotad"
      , typing = Double Water Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Lotad"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 399
      , originalPokemonID = Nothing
      , fullName = "Lombre"
      , typing = Double Water Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Lombre"
      , evolutionData = EvolvesFrom [ 398 ] "Level 14"
      , transformationData = DoesNotTransform
      }
    , { id = 400
      , originalPokemonID = Nothing
      , fullName = "Ludicolo"
      , typing = Double Water Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Ludicolo"
      , evolutionData = EvolvesFrom [ 399 ] "Use Water Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 401
      , originalPokemonID = Nothing
      , fullName = "Seedot"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Seedot"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 402
      , originalPokemonID = Nothing
      , fullName = "Nuzleaf"
      , typing = Double Grass Dark
      , ability = Nothing
      , imageUrl = imageUrlByName "Nuzleaf"
      , evolutionData = EvolvesFrom [ 401 ] "Level 14"
      , transformationData = DoesNotTransform
      }
    , { id = 403
      , originalPokemonID = Nothing
      , fullName = "Shiftry"
      , typing = Double Grass Dark
      , ability = Nothing
      , imageUrl = imageUrlByName "Shiftry"
      , evolutionData = EvolvesFrom [ 402 ] "Use Leaf Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 404
      , originalPokemonID = Nothing
      , fullName = "Taillow"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Taillow"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 405
      , originalPokemonID = Nothing
      , fullName = "Swellow"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Swellow"
      , evolutionData = EvolvesFrom [ 404 ] "Level 22"
      , transformationData = DoesNotTransform
      }
    , { id = 406
      , originalPokemonID = Nothing
      , fullName = "Wingull"
      , typing = Double Water Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Wingull"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 407
      , originalPokemonID = Nothing
      , fullName = "Pelipper"
      , typing = Double Water Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Pelipper"
      , evolutionData = EvolvesFrom [ 406 ] "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 408
      , originalPokemonID = Nothing
      , fullName = "Ralts"
      , typing = Double Psychic Fairy
      , ability = Nothing
      , imageUrl = imageUrlByName "Ralts"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 409
      , originalPokemonID = Nothing
      , fullName = "Kirlia"
      , typing = Double Psychic Fairy
      , ability = Nothing
      , imageUrl = imageUrlByName "Kirlia"
      , evolutionData = EvolvesFrom [ 408 ] "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 410
      , originalPokemonID = Nothing
      , fullName = "Gardevoir"
      , typing = Double Psychic Fairy
      , ability = Nothing
      , imageUrl = imageUrlByName "Gardevoir"
      , evolutionData = EvolvesFrom [ 409 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 1435
      , originalPokemonID = Nothing
      , fullName = nameFromData "Gardevoir" "Mega" ""
      , typing = Double Psychic Fairy
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Gardevoir" "Mega"
      , evolutionData = EvolvesFrom [ 410 ] "Holding Gardevoirite"
      , transformationData = DoesNotTransform
      }
    , { id = 720
      , originalPokemonID = Nothing
      , fullName = "Gallade"
      , typing = Double Psychic Fighting
      , ability = Nothing
      , imageUrl = imageUrlByName "Gallade"
      , evolutionData = EvolvesFrom [ 409 ] "Use Dawn Stone when male"
      , transformationData = DoesNotTransform
      }
    , { id = 1436
      , originalPokemonID = Nothing
      , fullName = nameFromData "Gallade" "Mega" ""
      , typing = Double Psychic Fighting
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Gallade" "Mega"
      , evolutionData = EvolvesFrom [ 720 ] "Holding Galladite"
      , transformationData = DoesNotTransform
      }
    , { id = 411
      , originalPokemonID = Nothing
      , fullName = "Surskit"
      , typing = Double Bug Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Surskit"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 412
      , originalPokemonID = Nothing
      , fullName = "Masquerain"
      , typing = Double Bug Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Masquerain"
      , evolutionData = EvolvesFrom [ 411 ] "Level 22"
      , transformationData = DoesNotTransform
      }
    , { id = 413
      , originalPokemonID = Nothing
      , fullName = "Shroomish"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Shroomish"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 414
      , originalPokemonID = Nothing
      , fullName = "Breloom"
      , typing = Double Grass Fighting
      , ability = Nothing
      , imageUrl = imageUrlByName "Breloom"
      , evolutionData = EvolvesFrom [ 413 ] "Level 23"
      , transformationData = DoesNotTransform
      }
    , { id = 415
      , originalPokemonID = Nothing
      , fullName = "Slakoth"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Slakoth"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 416
      , originalPokemonID = Nothing
      , fullName = "Vigoroth"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Vigoroth"
      , evolutionData = EvolvesFrom [ 415 ] "Level 18"
      , transformationData = DoesNotTransform
      }
    , { id = 417
      , originalPokemonID = Nothing
      , fullName = "Slaking"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Slaking"
      , evolutionData = EvolvesFrom [ 416 ] "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 418
      , originalPokemonID = Nothing
      , fullName = "Nincada"
      , typing = Double Bug Ground
      , ability = Nothing
      , imageUrl = imageUrlByName "Nincada"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 419
      , originalPokemonID = Nothing
      , fullName = "Ninjask"
      , typing = Double Bug Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Ninjask"
      , evolutionData = EvolvesFrom [ 418 ] "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 420
      , originalPokemonID = Nothing
      , fullName = "Shedinja"
      , typing = Double Bug Ghost
      , ability = Just WonderGuard
      , imageUrl = imageUrlByName "Shedinja"
      , evolutionData = EvolvesFrom [ 418 ] "Evolve while having a Pokeball and an empty space in party"
      , transformationData = DoesNotTransform
      }
    , { id = 421
      , originalPokemonID = Nothing
      , fullName = "Whismur"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Whismur"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 422
      , originalPokemonID = Nothing
      , fullName = "Loudred"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Loudred"
      , evolutionData = EvolvesFrom [ 421 ] "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 423
      , originalPokemonID = Nothing
      , fullName = "Exploud"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Exploud"
      , evolutionData = EvolvesFrom [ 422 ] "Level 40"
      , transformationData = DoesNotTransform
      }
    , { id = 424
      , originalPokemonID = Nothing
      , fullName = "Makuhita"
      , typing = Single Fighting
      , ability = Just ThickFat
      , imageUrl = imageUrlByName "Makuhita"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 425
      , originalPokemonID = Nothing
      , fullName = "Hariyama"
      , typing = Single Fighting
      , ability = Just ThickFat
      , imageUrl = imageUrlByName "Hariyama"
      , evolutionData = EvolvesFrom [ 424 ] "Level 24"
      , transformationData = DoesNotTransform
      }
    , { id = 427
      , originalPokemonID = Nothing
      , fullName = "Nosepass"
      , typing = Single Rock
      , ability = Nothing
      , imageUrl = imageUrlByName "Nosepass"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 726
      , originalPokemonID = Nothing
      , fullName = "Probopass"
      , typing = Double Rock Steel
      , ability = Nothing
      , imageUrl = imageUrlByName "Probopass"
      , evolutionData = EvolvesFrom [ 427 ] "Level near a Special Magnetic Field"
      , transformationData = DoesNotTransform
      }
    , { id = 428
      , originalPokemonID = Nothing
      , fullName = "Skitty"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Skitty"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 429
      , originalPokemonID = Nothing
      , fullName = "Delcatty"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Delcatty"
      , evolutionData = EvolvesFrom [ 428 ] "Use Moon Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 430
      , originalPokemonID = Nothing
      , fullName = "Sableye"
      , typing = Double Dark Ghost
      , ability = Nothing
      , imageUrl = imageUrlByName "Sableye"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1558
      , originalPokemonID = Nothing
      , fullName = nameFromData "Sableye" "Mega" ""
      , typing = Double Dark Ghost
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Sableye" "Mega"
      , evolutionData = EvolvesFrom [ 430 ] "Holding Sablenite"
      , transformationData = DoesNotTransform
      }
    , { id = 431
      , originalPokemonID = Nothing
      , fullName = "Mawile"
      , typing = Double Steel Fairy
      , ability = Nothing
      , imageUrl = imageUrlByName "Mawile"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1559
      , originalPokemonID = Nothing
      , fullName = nameFromData "Mawile" "Mega" ""
      , typing = Double Steel Fairy
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Mawile" "Mega"
      , evolutionData = EvolvesFrom [ 431 ] "Holding Mawilite"
      , transformationData = DoesNotTransform
      }
    , { id = 432
      , originalPokemonID = Nothing
      , fullName = "Aron"
      , typing = Double Steel Rock
      , ability = Nothing
      , imageUrl = imageUrlByName "Aron"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 433
      , originalPokemonID = Nothing
      , fullName = "Lairon"
      , typing = Double Steel Rock
      , ability = Nothing
      , imageUrl = imageUrlByName "Lairon"
      , evolutionData = EvolvesFrom [ 432 ] "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 434
      , originalPokemonID = Nothing
      , fullName = "Aggron"
      , typing = Double Steel Rock
      , ability = Nothing
      , imageUrl = imageUrlByName "Aggron"
      , evolutionData = EvolvesFrom [ 433 ] "Level 42"
      , transformationData = DoesNotTransform
      }
    , { id = 1434
      , originalPokemonID = Nothing
      , fullName = nameFromData "Aggron" "Mega" ""
      , typing = Single Steel
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Aggron" "Mega"
      , evolutionData = EvolvesFrom [ 434 ] "Holding Aggronite"
      , transformationData = DoesNotTransform
      }
    , { id = 435
      , originalPokemonID = Nothing
      , fullName = "Meditite"
      , typing = Double Fighting Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Meditite"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 436
      , originalPokemonID = Nothing
      , fullName = "Medicham"
      , typing = Double Fighting Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Medicham"
      , evolutionData = EvolvesFrom [ 435 ] "Level 37"
      , transformationData = DoesNotTransform
      }
    , { id = 1590
      , originalPokemonID = Nothing
      , fullName = nameFromData "Medicham" "Mega" ""
      , typing = Double Fighting Psychic
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Medicham" "Mega"
      , evolutionData = EvolvesFrom [ 436 ] "Holding Medichamite"
      , transformationData = DoesNotTransform
      }
    , { id = 437
      , originalPokemonID = Nothing
      , fullName = "Electrike"
      , typing = Single Electric
      , ability = Just LightningRod
      , imageUrl = imageUrlByName "Electrike"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 438
      , originalPokemonID = Nothing
      , fullName = "Manectric"
      , typing = Single Electric
      , ability = Just LightningRod
      , imageUrl = imageUrlByName "Manectric"
      , evolutionData = EvolvesFrom [ 437 ] "Level 26"
      , transformationData = DoesNotTransform
      }
    , { id = 1437
      , originalPokemonID = Nothing
      , fullName = nameFromData "Manectric" "Mega" ""
      , typing = Single Electric
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Manectric" "Mega"
      , evolutionData = EvolvesFrom [ 438 ] "Holding Manectite"
      , transformationData = DoesNotTransform
      }
    , { id = 439
      , originalPokemonID = Nothing
      , fullName = "Plusle"
      , typing = Single Electric
      , ability = Nothing
      , imageUrl = imageUrlByName "Plusle"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 440
      , originalPokemonID = Nothing
      , fullName = "Minun"
      , typing = Single Electric
      , ability = Just VoltAbsorb
      , imageUrl = imageUrlByName "Minun"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 441
      , originalPokemonID = Nothing
      , fullName = "Volbeat"
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imageUrlByName "Volbeat"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 442
      , originalPokemonID = Nothing
      , fullName = "Illumise"
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imageUrlByName "Illumise"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 604
      , originalPokemonID = Nothing
      , fullName = "Budew"
      , typing = Double Grass Poison
      , ability = Nothing
      , imageUrl = imageUrlByName "Budew"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 443
      , originalPokemonID = Nothing
      , fullName = "Roselia"
      , typing = Double Grass Poison
      , ability = Nothing
      , imageUrl = imageUrlByName "Roselia"
      , evolutionData = EvolvesFrom [ 604 ] "Level during the day with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 605
      , originalPokemonID = Nothing
      , fullName = "Roserade"
      , typing = Double Grass Poison
      , ability = Nothing
      , imageUrl = imageUrlByName "Roserade"
      , evolutionData = EvolvesFrom [ 443 ] "Use Shiny Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 444
      , originalPokemonID = Nothing
      , fullName = "Gulpin"
      , typing = Single Poison
      , ability = Nothing
      , imageUrl = imageUrlByName "Gulpin"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 445
      , originalPokemonID = Nothing
      , fullName = "Swalot"
      , typing = Single Poison
      , ability = Nothing
      , imageUrl = imageUrlByName "Swalot"
      , evolutionData = EvolvesFrom [ 444 ] "Level 26"
      , transformationData = DoesNotTransform
      }
    , { id = 446
      , originalPokemonID = Nothing
      , fullName = "Carvanha"
      , typing = Double Water Dark
      , ability = Nothing
      , imageUrl = imageUrlByName "Carvanha"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 447
      , originalPokemonID = Nothing
      , fullName = "Sharpedo"
      , typing = Double Water Dark
      , ability = Nothing
      , imageUrl = imageUrlByName "Sharpedo"
      , evolutionData = EvolvesFrom [ 446 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 1587
      , originalPokemonID = Nothing
      , fullName = nameFromData "Sharpedo" "Mega" ""
      , typing = Double Water Dark
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Sharpedo" "Mega"
      , evolutionData = EvolvesFrom [ 447 ] "Holding Sharpedonite"
      , transformationData = DoesNotTransform
      }
    , { id = 448
      , originalPokemonID = Nothing
      , fullName = "Wailmer"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Wailmer"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 449
      , originalPokemonID = Nothing
      , fullName = "Wailord"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Wailord"
      , evolutionData = EvolvesFrom [ 448 ] "Level 40"
      , transformationData = DoesNotTransform
      }
    , { id = 450
      , originalPokemonID = Nothing
      , fullName = "Numel"
      , typing = Double Fire Ground
      , ability = Nothing
      , imageUrl = imageUrlByName "Numel"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 451
      , originalPokemonID = Nothing
      , fullName = "Camerupt"
      , typing = Double Fire Ground
      , ability = Nothing
      , imageUrl = imageUrlByName "Camerupt"
      , evolutionData = EvolvesFrom [ 450 ] "Level 33"
      , transformationData = DoesNotTransform
      }
    , { id = 1588
      , originalPokemonID = Nothing
      , fullName = nameFromData "Camerupt" "Mega" ""
      , typing = Double Fire Ground
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Camerupt" "Mega"
      , evolutionData = EvolvesFrom [ 451 ] "Holding Cameruptite"
      , transformationData = DoesNotTransform
      }
    , { id = 452
      , originalPokemonID = Nothing
      , fullName = "Torkoal"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrlByName "Torkoal"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 453
      , originalPokemonID = Nothing
      , fullName = "Spoink"
      , typing = Single Psychic
      , ability = Just ThickFat
      , imageUrl = imageUrlByName "Spoink"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 454
      , originalPokemonID = Nothing
      , fullName = "Grumpig"
      , typing = Single Psychic
      , ability = Just ThickFat
      , imageUrl = imageUrlByName "Grumpig"
      , evolutionData = EvolvesFrom [ 453 ] "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 455
      , originalPokemonID = Nothing
      , fullName = "Spinda"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Spinda"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 456
      , originalPokemonID = Nothing
      , fullName = "Trapinch"
      , typing = Single Ground
      , ability = Nothing
      , imageUrl = imageUrlByName "Trapinch"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 457
      , originalPokemonID = Nothing
      , fullName = "Vibrava"
      , typing = Double Ground Dragon
      , ability = Just Levitate
      , imageUrl = imageUrlByName "Vibrava"
      , evolutionData = EvolvesFrom [ 456 ] "Level 35"
      , transformationData = DoesNotTransform
      }
    , { id = 458
      , originalPokemonID = Nothing
      , fullName = "Flygon"
      , typing = Double Ground Dragon
      , ability = Just Levitate
      , imageUrl = imageUrlByName "Flygon"
      , evolutionData = EvolvesFrom [ 457 ] "Level 45"
      , transformationData = DoesNotTransform
      }
    , { id = 459
      , originalPokemonID = Nothing
      , fullName = "Cacnea"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Cacnea"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 460
      , originalPokemonID = Nothing
      , fullName = "Cacturne"
      , typing = Double Grass Dark
      , ability = Nothing
      , imageUrl = imageUrlByName "Cacturne"
      , evolutionData = EvolvesFrom [ 459 ] "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 461
      , originalPokemonID = Nothing
      , fullName = "Swablu"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Swablu"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 462
      , originalPokemonID = Nothing
      , fullName = "Altaria"
      , typing = Double Dragon Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Altaria"
      , evolutionData = EvolvesFrom [ 461 ] "Level 35"
      , transformationData = DoesNotTransform
      }
    , { id = 1589
      , originalPokemonID = Nothing
      , fullName = nameFromData "Altaria" "Mega" ""
      , typing = Double Dragon Fairy
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Altaria" "Mega"
      , evolutionData = EvolvesFrom [ 462 ] "Holding Altarianite"
      , transformationData = DoesNotTransform
      }
    , { id = 463
      , originalPokemonID = Nothing
      , fullName = "Zangoose"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Zangoose"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 464
      , originalPokemonID = Nothing
      , fullName = "Seviper"
      , typing = Single Poison
      , ability = Nothing
      , imageUrl = imageUrlByName "Seviper"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 465
      , originalPokemonID = Nothing
      , fullName = "Lunatone"
      , typing = Double Rock Psychic
      , ability = Just Levitate
      , imageUrl = imageUrlByName "Lunatone"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 466
      , originalPokemonID = Nothing
      , fullName = "Solrock"
      , typing = Double Rock Psychic
      , ability = Just Levitate
      , imageUrl = imageUrlByName "Solrock"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 467
      , originalPokemonID = Nothing
      , fullName = "Barboach"
      , typing = Double Water Ground
      , ability = Nothing
      , imageUrl = imageUrlByName "Barboach"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 468
      , originalPokemonID = Nothing
      , fullName = "Whiscash"
      , typing = Double Water Ground
      , ability = Nothing
      , imageUrl = imageUrlByName "Whiscash"
      , evolutionData = EvolvesFrom [ 467 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 469
      , originalPokemonID = Nothing
      , fullName = "Corphish"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Corphish"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 470
      , originalPokemonID = Nothing
      , fullName = "Crawdaunt"
      , typing = Double Water Dark
      , ability = Nothing
      , imageUrl = imageUrlByName "Crawdaunt"
      , evolutionData = EvolvesFrom [ 469 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 471
      , originalPokemonID = Nothing
      , fullName = "Baltoy"
      , typing = Double Ground Psychic
      , ability = Just Levitate
      , imageUrl = imageUrlByName "Baltoy"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 472
      , originalPokemonID = Nothing
      , fullName = "Claydol"
      , typing = Double Ground Psychic
      , ability = Just Levitate
      , imageUrl = imageUrlByName "Claydol"
      , evolutionData = EvolvesFrom [ 471 ] "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 473
      , originalPokemonID = Nothing
      , fullName = "Lileep"
      , typing = Double Rock Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Lileep"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 474
      , originalPokemonID = Nothing
      , fullName = "Cradily"
      , typing = Double Rock Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Cradily"
      , evolutionData = EvolvesFrom [ 473 ] "Level 40"
      , transformationData = DoesNotTransform
      }
    , { id = 475
      , originalPokemonID = Nothing
      , fullName = "Anorith"
      , typing = Double Rock Bug
      , ability = Nothing
      , imageUrl = imageUrlByName "Anorith"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 476
      , originalPokemonID = Nothing
      , fullName = "Armaldo"
      , typing = Double Rock Bug
      , ability = Nothing
      , imageUrl = imageUrlByName "Armaldo"
      , evolutionData = EvolvesFrom [ 475 ] "Level 40"
      , transformationData = DoesNotTransform
      }
    , { id = 477
      , originalPokemonID = Nothing
      , fullName = "Feebas"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Feebas"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 478
      , originalPokemonID = Nothing
      , fullName = "Milotic"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Milotic"
      , evolutionData = EvolvesFrom [ 477 ] "Trade holding Prism Scale"
      , transformationData = DoesNotTransform
      }
    , { id = 479
      , originalPokemonID = Nothing
      , fullName = "Castform"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Castform"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 1 "During normal weather, fog, sandstorm, or shadowy aura"
      }
    , { id = 1565
      , originalPokemonID = Just 479
      , fullName = nameFromData "Castform" "Sunny" ""
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Castform" "Sunny"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 1 "During harsh sunlight"
      }
    , { id = 1566
      , originalPokemonID = Just 479
      , fullName = nameFromData "Castform" "Rainy" ""
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Castform" "Rainy"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 1 "During rain"
      }
    , { id = 1567
      , originalPokemonID = Just 479
      , fullName = nameFromData "Castform" "Snowy" ""
      , typing = Single Ice
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Castform" "Snowy"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 1 "During hail"
      }
    , { id = 480
      , originalPokemonID = Nothing
      , fullName = "Kecleon"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Kecleon"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 481
      , originalPokemonID = Nothing
      , fullName = "Shuppet"
      , typing = Single Ghost
      , ability = Nothing
      , imageUrl = imageUrlByName "Shuppet"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 482
      , originalPokemonID = Nothing
      , fullName = "Banette"
      , typing = Single Ghost
      , ability = Nothing
      , imageUrl = imageUrlByName "Banette"
      , evolutionData = EvolvesFrom [ 481 ] "Level 37"
      , transformationData = DoesNotTransform
      }
    , { id = 1592
      , originalPokemonID = Nothing
      , fullName = nameFromData "Banette" "Mega" ""
      , typing = Single Ghost
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Banette" "Mega"
      , evolutionData = EvolvesFrom [ 482 ] "Holding Banettite"
      , transformationData = DoesNotTransform
      }
    , { id = 483
      , originalPokemonID = Nothing
      , fullName = "Duskull"
      , typing = Single Ghost
      , ability = Just Levitate
      , imageUrl = imageUrlByName "Duskull"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 484
      , originalPokemonID = Nothing
      , fullName = "Dusclops"
      , typing = Single Ghost
      , ability = Nothing
      , imageUrl = imageUrlByName "Dusclops"
      , evolutionData = EvolvesFrom [ 483 ] "Level 37"
      , transformationData = DoesNotTransform
      }
    , { id = 727
      , originalPokemonID = Nothing
      , fullName = "Dusknoir"
      , typing = Single Ghost
      , ability = Nothing
      , imageUrl = imageUrlByName "Dusknoir"
      , evolutionData = EvolvesFrom [ 484 ] "Trade holding Reaper Cloth"
      , transformationData = DoesNotTransform
      }
    , { id = 485
      , originalPokemonID = Nothing
      , fullName = "Tropius"
      , typing = Double Grass Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Tropius"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 631
      , originalPokemonID = Nothing
      , fullName = "Chingling"
      , typing = Single Psychic
      , ability = Just Levitate
      , imageUrl = imageUrlByName "Chingling"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 486
      , originalPokemonID = Nothing
      , fullName = "Chimecho"
      , typing = Single Psychic
      , ability = Just Levitate
      , imageUrl = imageUrlByName "Chimecho"
      , evolutionData = EvolvesFrom [ 631 ] "Level during the night with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 487
      , originalPokemonID = Nothing
      , fullName = "Absol"
      , typing = Single Dark
      , ability = Nothing
      , imageUrl = imageUrlByName "Absol"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1591
      , originalPokemonID = Nothing
      , fullName = nameFromData "Absol" "Mega" ""
      , typing = Single Dark
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Absol" "Mega"
      , evolutionData = EvolvesFrom [ 487 ] "Holding Absolite"
      , transformationData = DoesNotTransform
      }
    , { id = 489
      , originalPokemonID = Nothing
      , fullName = "Snorunt"
      , typing = Single Ice
      , ability = Nothing
      , imageUrl = imageUrlByName "Snorunt"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 490
      , originalPokemonID = Nothing
      , fullName = "Glalie"
      , typing = Single Ice
      , ability = Nothing
      , imageUrl = imageUrlByName "Glalie"
      , evolutionData = EvolvesFrom [ 489 ] "Level 42"
      , transformationData = DoesNotTransform
      }
    , { id = 1560
      , originalPokemonID = Nothing
      , fullName = nameFromData "Glalie" "Mega" ""
      , typing = Single Ice
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Glalie" "Mega"
      , evolutionData = EvolvesFrom [ 490 ] "Holding Glalitite"
      , transformationData = DoesNotTransform
      }
    , { id = 728
      , originalPokemonID = Nothing
      , fullName = "Froslass"
      , typing = Double Ice Ghost
      , ability = Nothing
      , imageUrl = imageUrlByName "Froslass"
      , evolutionData = EvolvesFrom [ 489 ] "Use Dawn Stone when female"
      , transformationData = DoesNotTransform
      }
    , { id = 491
      , originalPokemonID = Nothing
      , fullName = "Spheal"
      , typing = Double Ice Water
      , ability = Just ThickFat
      , imageUrl = imageUrlByName "Spheal"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 492
      , originalPokemonID = Nothing
      , fullName = "Sealeo"
      , typing = Double Ice Water
      , ability = Just ThickFat
      , imageUrl = imageUrlByName "Sealeo"
      , evolutionData = EvolvesFrom [ 491 ] "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 493
      , originalPokemonID = Nothing
      , fullName = "Walrein"
      , typing = Double Ice Water
      , ability = Just ThickFat
      , imageUrl = imageUrlByName "Walrein"
      , evolutionData = EvolvesFrom [ 492 ] "Level 44"
      , transformationData = DoesNotTransform
      }
    , { id = 494
      , originalPokemonID = Nothing
      , fullName = "Clamperl"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Clamperl"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 495
      , originalPokemonID = Nothing
      , fullName = "Huntail"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Huntail"
      , evolutionData = EvolvesFrom [ 494 ] "Trade holding Deep Sea Tooth"
      , transformationData = DoesNotTransform
      }
    , { id = 496
      , originalPokemonID = Nothing
      , fullName = "Gorebyss"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Gorebyss"
      , evolutionData = EvolvesFrom [ 494 ] "Trade holding Deep Sea Scale"
      , transformationData = DoesNotTransform
      }
    , { id = 497
      , originalPokemonID = Nothing
      , fullName = "Relicanth"
      , typing = Double Water Rock
      , ability = Nothing
      , imageUrl = imageUrlByName "Relicanth"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 498
      , originalPokemonID = Nothing
      , fullName = "Luvdisc"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Luvdisc"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 499
      , originalPokemonID = Nothing
      , fullName = "Bagon"
      , typing = Single Dragon
      , ability = Nothing
      , imageUrl = imageUrlByName "Bagon"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 500
      , originalPokemonID = Nothing
      , fullName = "Shelgon"
      , typing = Single Dragon
      , ability = Nothing
      , imageUrl = imageUrlByName "Shelgon"
      , evolutionData = EvolvesFrom [ 499 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 501
      , originalPokemonID = Nothing
      , fullName = "Salamence"
      , typing = Double Dragon Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Salamence"
      , evolutionData = EvolvesFrom [ 500 ] "Level 50"
      , transformationData = DoesNotTransform
      }
    , { id = 1586
      , originalPokemonID = Nothing
      , fullName = nameFromData "Salamence" "Mega" ""
      , typing = Double Dragon Flying
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Salamence" "Mega"
      , evolutionData = EvolvesFrom [ 501 ] "Holding Salamencite"
      , transformationData = DoesNotTransform
      }
    , { id = 502
      , originalPokemonID = Nothing
      , fullName = "Beldum"
      , typing = Double Steel Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Beldum"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 503
      , originalPokemonID = Nothing
      , fullName = "Metang"
      , typing = Double Steel Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Metang"
      , evolutionData = EvolvesFrom [ 502 ] "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 504
      , originalPokemonID = Nothing
      , fullName = "Metagross"
      , typing = Double Steel Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Metagross"
      , evolutionData = EvolvesFrom [ 503 ] "Level 45"
      , transformationData = DoesNotTransform
      }
    , { id = 1585
      , originalPokemonID = Nothing
      , fullName = nameFromData "Metagross" "Mega" ""
      , typing = Double Steel Psychic
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Metagross" "Mega"
      , evolutionData = EvolvesFrom [ 504 ] "Holding Metagrossite"
      , transformationData = DoesNotTransform
      }
    , { id = 505
      , originalPokemonID = Nothing
      , fullName = "Regirock"
      , typing = Single Rock
      , ability = Nothing
      , imageUrl = imageUrlByName "Regirock"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 506
      , originalPokemonID = Nothing
      , fullName = "Regice"
      , typing = Single Ice
      , ability = Nothing
      , imageUrl = imageUrlByName "Regice"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 507
      , originalPokemonID = Nothing
      , fullName = "Registeel"
      , typing = Single Steel
      , ability = Nothing
      , imageUrl = imageUrlByName "Registeel"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 508
      , originalPokemonID = Nothing
      , fullName = "Latias"
      , typing = Double Dragon Psychic
      , ability = Just Levitate
      , imageUrl = imageUrlByName "Latias"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1400
      , originalPokemonID = Nothing
      , fullName = nameFromData "Latias" "Mega" ""
      , typing = Double Dragon Psychic
      , ability = Just Levitate
      , imageUrl = imgUrlForAlternateForm "Latias" "Mega"
      , evolutionData = EvolvesFrom [ 508 ] "Holding Latiasite"
      , transformationData = DoesNotTransform
      }
    , { id = 509
      , originalPokemonID = Nothing
      , fullName = "Latios"
      , typing = Double Dragon Psychic
      , ability = Just Levitate
      , imageUrl = imageUrlByName "Latios"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1399
      , originalPokemonID = Nothing
      , fullName = nameFromData "Latios" "Mega" ""
      , typing = Double Dragon Psychic
      , ability = Just Levitate
      , imageUrl = imgUrlForAlternateForm "Latios" "Mega"
      , evolutionData = EvolvesFrom [ 509 ] "Holding Latiosite"
      , transformationData = DoesNotTransform
      }
    , { id = 510
      , originalPokemonID = Nothing
      , fullName = "Kyogre"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Kyogre"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 2 "If not holding Blue Orb"
      }
    , { id = 1432
      , originalPokemonID = Just 510
      , fullName = nameFromData "Kyogre" "Primal" ""
      , typing = Single Water
      , ability = Just PrimordialSea
      , imageUrl = imgUrlForAlternateForm "Kyogre" "Primal"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 2 "While holding Blue Orb"
      }
    , { id = 511
      , originalPokemonID = Nothing
      , fullName = "Groudon"
      , typing = Single Ground
      , ability = Nothing
      , imageUrl = imageUrlByName "Groudon"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 3 "If not holding Red Orb"
      }
    , { id = 1433
      , originalPokemonID = Just 511
      , fullName = nameFromData "Groudon" "Primal" ""
      , typing = Double Ground Fire
      , ability = Just DesolateLand
      , imageUrl = imgUrlForAlternateForm "Groudon" "Primal"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 3 "While holding Red Orb"
      }
    , { id = 512
      , originalPokemonID = Nothing
      , fullName = "Rayquaza"
      , typing = Double Dragon Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Rayquaza"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1431
      , originalPokemonID = Nothing
      , fullName = nameFromData "Rayquaza" "Mega" ""
      , typing = Double Dragon Flying
      , ability = Just DeltaStream
      , imageUrl = imgUrlForAlternateForm "Rayquaza" "Mega"
      , evolutionData = EvolvesFrom [ 512 ] "Knowing Dragon Ascent and not holding a Z-Crystal"
      , transformationData = DoesNotTransform
      }
    , { id = 513
      , originalPokemonID = Nothing
      , fullName = "Jirachi"
      , typing = Double Steel Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Jirachi"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 514
      , originalPokemonID = Nothing
      , fullName = "Deoxys"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Deoxys"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 4 "In Pokémon Ruby and Sapphire"
      }
    , { id = 1391
      , originalPokemonID = Just 514
      , fullName = nameFromData "Deoxys" "Attack" "Deoxys (Attack)"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Deoxys" "Attack"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 4 "In Pokémon FireRed"
      }
    , { id = 1392
      , originalPokemonID = Just 514
      , fullName = nameFromData "Deoxys" "Defense" "Deoxys (Defense)"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Deoxys" "Defense"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 4 "In Pokémon LeafGreen"
      }
    , { id = 1393
      , originalPokemonID = Just 514
      , fullName = nameFromData "Deoxys" "Speed" "Deoxys (Speed)"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Deoxys" "Speed"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 4 "In Pokémon Emerald"
      }
    , { id = 591
      , originalPokemonID = Nothing
      , fullName = "Turtwig"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Turtwig"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 592
      , originalPokemonID = Nothing
      , fullName = "Grotle"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Grotle"
      , evolutionData = EvolvesFrom [ 591 ] "Level 18"
      , transformationData = DoesNotTransform
      }
    , { id = 593
      , originalPokemonID = Nothing
      , fullName = "Torterra"
      , typing = Double Grass Ground
      , ability = Nothing
      , imageUrl = imageUrlByName "Torterra"
      , evolutionData = EvolvesFrom [ 592 ] "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 34
      , originalPokemonID = Nothing
      , fullName = "Chimchar"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrlByName "Chimchar"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 35
      , originalPokemonID = Nothing
      , fullName = "Monferno"
      , typing = Double Fire Fighting
      , ability = Nothing
      , imageUrl = imageUrlByName "Monferno"
      , evolutionData = EvolvesFrom [ 34 ] "Level 14"
      , transformationData = DoesNotTransform
      }
    , { id = 19
      , originalPokemonID = Nothing
      , fullName = "Infernape"
      , typing = Double Fire Fighting
      , ability = Nothing
      , imageUrl = imageUrlByName "Infernape"
      , evolutionData = EvolvesFrom [ 35 ] "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 594
      , originalPokemonID = Nothing
      , fullName = "Piplup"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Piplup"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 595
      , originalPokemonID = Nothing
      , fullName = "Prinplup"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Prinplup"
      , evolutionData = EvolvesFrom [ 594 ] "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 596
      , originalPokemonID = Nothing
      , fullName = "Empoleon"
      , typing = Double Water Steel
      , ability = Nothing
      , imageUrl = imageUrlByName "Empoleon"
      , evolutionData = EvolvesFrom [ 595 ] "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 597
      , originalPokemonID = Nothing
      , fullName = "Starly"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Starly"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 598
      , originalPokemonID = Nothing
      , fullName = "Staravia"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Staravia"
      , evolutionData = EvolvesFrom [ 597 ] "Level 14"
      , transformationData = DoesNotTransform
      }
    , { id = 599
      , originalPokemonID = Nothing
      , fullName = "Staraptor"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Staraptor"
      , evolutionData = EvolvesFrom [ 598 ] "Level 34"
      , transformationData = DoesNotTransform
      }
    , { id = 600
      , originalPokemonID = Nothing
      , fullName = "Bidoof"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Bidoof"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 601
      , originalPokemonID = Nothing
      , fullName = "Bibarel"
      , typing = Double Normal Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Bibarel"
      , evolutionData = EvolvesFrom [ 600 ] "Level 15"
      , transformationData = DoesNotTransform
      }
    , { id = 602
      , originalPokemonID = Nothing
      , fullName = "Kricketot"
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imageUrlByName "Kricketot"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 603
      , originalPokemonID = Nothing
      , fullName = "Kricketune"
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imageUrlByName "Kricketune"
      , evolutionData = EvolvesFrom [ 602 ] "Level 10"
      , transformationData = DoesNotTransform
      }
    , { id = 20
      , originalPokemonID = Nothing
      , fullName = "Shinx"
      , typing = Single Electric
      , ability = Nothing
      , imageUrl = imageUrlByName "Shinx"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 21
      , originalPokemonID = Nothing
      , fullName = "Luxio"
      , typing = Single Electric
      , ability = Nothing
      , imageUrl = imageUrlByName "Luxio"
      , evolutionData = EvolvesFrom [ 20 ] "Level 15"
      , transformationData = DoesNotTransform
      }
    , { id = 22
      , originalPokemonID = Nothing
      , fullName = "Luxray"
      , typing = Single Electric
      , ability = Nothing
      , imageUrl = imageUrlByName "Luxray"
      , evolutionData = EvolvesFrom [ 21 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 606
      , originalPokemonID = Nothing
      , fullName = "Cranidos"
      , typing = Single Rock
      , ability = Nothing
      , imageUrl = imageUrlByName "Cranidos"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 607
      , originalPokemonID = Nothing
      , fullName = "Rampardos"
      , typing = Single Rock
      , ability = Nothing
      , imageUrl = imageUrlByName "Rampardos"
      , evolutionData = EvolvesFrom [ 606 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 608
      , originalPokemonID = Nothing
      , fullName = "Shieldon"
      , typing = Double Rock Steel
      , ability = Nothing
      , imageUrl = imageUrlByName "Shieldon"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 609
      , originalPokemonID = Nothing
      , fullName = "Bastiodon"
      , typing = Double Rock Steel
      , ability = Nothing
      , imageUrl = imageUrlByName "Bastiodon"
      , evolutionData = EvolvesFrom [ 608 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 610
      , originalPokemonID = Nothing
      , fullName = nameFromData "Burmy" "Plant" ""
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Burmy" "Plant"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 5 "If it last battled in grass"
      }
    , { id = 611
      , originalPokemonID = Nothing
      , fullName = nameFromData "Wormadam" "Plant" ""
      , typing = Double Bug Grass
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Wormadam" "Plant"
      , evolutionData = EvolvesFrom [ 610 ] "Level 20 when female"
      , transformationData = DoesNotTransform
      }
    , { id = 1606
      , originalPokemonID = Just 610
      , fullName = nameFromData "Burmy" "Sandy" ""
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Burmy" "Sandy"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 5 "If it last battled in a cave"
      }
    , { id = 1568
      , originalPokemonID = Just 611
      , fullName = nameFromData "Wormadam" "Sandy" ""
      , typing = Double Bug Ground
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Wormadam" "Sandy"
      , evolutionData = EvolvesFrom [ 1606 ] "Level 20 when female"
      , transformationData = DoesNotTransform
      }
    , { id = 1607
      , originalPokemonID = Just 610
      , fullName = nameFromData "Burmy" "Trash" ""
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Burmy" "Trash"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 5 "If it last battled in a building"
      }
    , { id = 1569
      , originalPokemonID = Just 611
      , fullName = nameFromData "Wormadam" "Trash" ""
      , typing = Double Bug Steel
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Wormadam" "Trash"
      , evolutionData = EvolvesFrom [ 1607 ] "Level 20 when female"
      , transformationData = DoesNotTransform
      }
    , { id = 612
      , originalPokemonID = Nothing
      , fullName = "Mothim"
      , typing = Double Bug Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Mothim"
      , evolutionData = EvolvesFrom [ 610, 1606, 1607 ] "Level 20 when male"
      , transformationData = DoesNotTransform
      }
    , { id = 613
      , originalPokemonID = Nothing
      , fullName = "Combee"
      , typing = Double Bug Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Combee"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 614
      , originalPokemonID = Nothing
      , fullName = "Vespiquen"
      , typing = Double Bug Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Vespiquen"
      , evolutionData = EvolvesFrom [ 613 ] "Level 21 When Female"
      , transformationData = DoesNotTransform
      }
    , { id = 615
      , originalPokemonID = Nothing
      , fullName = "Pachirisu"
      , typing = Single Electric
      , ability = Nothing
      , imageUrl = imageUrlByName "Pachirisu"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 616
      , originalPokemonID = Nothing
      , fullName = "Buizel"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Buizel"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 617
      , originalPokemonID = Nothing
      , fullName = "Floatzel"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Floatzel"
      , evolutionData = EvolvesFrom [ 616 ] "Level 26"
      , transformationData = DoesNotTransform
      }
    , { id = 618
      , originalPokemonID = Nothing
      , fullName = "Cherubi"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Cherubi"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 619
      , originalPokemonID = Nothing
      , fullName = "Cherrim"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Cherrim"
      , evolutionData = EvolvesFrom [ 618 ] "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 620
      , originalPokemonID = Nothing
      , fullName = "Shellos"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Shellos"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 621
      , originalPokemonID = Nothing
      , fullName = "Gastrodon"
      , typing = Double Water Ground
      , ability = Nothing
      , imageUrl = imageUrlByName "Gastrodon"
      , evolutionData = EvolvesFrom [ 620 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 623
      , originalPokemonID = Nothing
      , fullName = "Drifloon"
      , typing = Double Ghost Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Drifloon"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 624
      , originalPokemonID = Nothing
      , fullName = "Drifblim"
      , typing = Double Ghost Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Drifblim"
      , evolutionData = EvolvesFrom [ 623 ] "Level 28"
      , transformationData = DoesNotTransform
      }
    , { id = 625
      , originalPokemonID = Nothing
      , fullName = "Buneary"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Buneary"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 626
      , originalPokemonID = Nothing
      , fullName = "Lopunny"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Lopunny"
      , evolutionData = EvolvesFrom [ 625 ] "Level with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 1593
      , originalPokemonID = Nothing
      , fullName = nameFromData "Lopunny" "Mega" ""
      , typing = Double Normal Fighting
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Lopunny" "Mega"
      , evolutionData = EvolvesFrom [ 626 ] "Holding Lopunnite"
      , transformationData = DoesNotTransform
      }
    , { id = 629
      , originalPokemonID = Nothing
      , fullName = "Glameow"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Glameow"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 630
      , originalPokemonID = Nothing
      , fullName = "Purugly"
      , typing = Single Normal
      , ability = Just ThickFat
      , imageUrl = imageUrlByName "Purugly"
      , evolutionData = EvolvesFrom [ 629 ] "Level 38"
      , transformationData = DoesNotTransform
      }
    , { id = 632
      , originalPokemonID = Nothing
      , fullName = "Stunky"
      , typing = Double Poison Dark
      , ability = Nothing
      , imageUrl = imageUrlByName "Stunky"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 633
      , originalPokemonID = Nothing
      , fullName = "Skuntank"
      , typing = Double Poison Dark
      , ability = Nothing
      , imageUrl = imageUrlByName "Skuntank"
      , evolutionData = EvolvesFrom [ 632 ] "Level 34"
      , transformationData = DoesNotTransform
      }
    , { id = 634
      , originalPokemonID = Nothing
      , fullName = "Bronzor"
      , typing = Double Steel Psychic
      , ability = Just Levitate
      , imageUrl = imageUrlByName "Bronzor"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 635
      , originalPokemonID = Nothing
      , fullName = "Bronzong"
      , typing = Double Steel Psychic
      , ability = Just Levitate
      , imageUrl = imageUrlByName "Bronzong"
      , evolutionData = EvolvesFrom [ 634 ] "Level 33"
      , transformationData = DoesNotTransform
      }
    , { id = 639
      , originalPokemonID = Nothing
      , fullName = "Chatot"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Chatot"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 640
      , originalPokemonID = Nothing
      , fullName = "Spiritomb"
      , typing = Double Ghost Dark
      , ability = Nothing
      , imageUrl = imageUrlByName "Spiritomb"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 641
      , originalPokemonID = Nothing
      , fullName = "Gible"
      , typing = Double Dragon Ground
      , ability = Nothing
      , imageUrl = imageUrlByName "Gible"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 642
      , originalPokemonID = Nothing
      , fullName = "Gabite"
      , typing = Double Dragon Ground
      , ability = Nothing
      , imageUrl = imageUrlByName "Gabite"
      , evolutionData = EvolvesFrom [ 641 ] "Level 24"
      , transformationData = DoesNotTransform
      }
    , { id = 643
      , originalPokemonID = Nothing
      , fullName = "Garchomp"
      , typing = Double Dragon Ground
      , ability = Nothing
      , imageUrl = imageUrlByName "Garchomp"
      , evolutionData = EvolvesFrom [ 642 ] "Level 48"
      , transformationData = DoesNotTransform
      }
    , { id = 1438
      , originalPokemonID = Nothing
      , fullName = nameFromData "Garchomp" "Mega" ""
      , typing = Double Dragon Ground
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Garchomp" "Mega"
      , evolutionData = EvolvesFrom [ 643 ] "Holding Garchompite"
      , transformationData = DoesNotTransform
      }
    , { id = 645
      , originalPokemonID = Nothing
      , fullName = "Riolu"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrlByName "Riolu"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 646
      , originalPokemonID = Nothing
      , fullName = "Lucario"
      , typing = Double Fighting Steel
      , ability = Nothing
      , imageUrl = imageUrlByName "Lucario"
      , evolutionData = EvolvesFrom [ 645 ] "Level during the day with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 1561
      , originalPokemonID = Nothing
      , fullName = nameFromData "Lucario" "Mega" ""
      , typing = Double Fighting Steel
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Lucario" "Mega"
      , evolutionData = EvolvesFrom [ 646 ] "Holding Lucarionite"
      , transformationData = DoesNotTransform
      }
    , { id = 647
      , originalPokemonID = Nothing
      , fullName = "Hippopotas"
      , typing = Single Ground
      , ability = Nothing
      , imageUrl = imageUrlByName "Hippopotas"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 648
      , originalPokemonID = Nothing
      , fullName = "Hippowdon"
      , typing = Single Ground
      , ability = Nothing
      , imageUrl = imageUrlByName "Hippowdon"
      , evolutionData = EvolvesFrom [ 647 ] "Level 34"
      , transformationData = DoesNotTransform
      }
    , { id = 649
      , originalPokemonID = Nothing
      , fullName = "Skorupi"
      , typing = Double Poison Bug
      , ability = Nothing
      , imageUrl = imageUrlByName "Skorupi"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 650
      , originalPokemonID = Nothing
      , fullName = "Drapion"
      , typing = Double Poison Dark
      , ability = Nothing
      , imageUrl = imageUrlByName "Drapion"
      , evolutionData = EvolvesFrom [ 649 ] "Level 40"
      , transformationData = DoesNotTransform
      }
    , { id = 651
      , originalPokemonID = Nothing
      , fullName = "Croagunk"
      , typing = Double Poison Fighting
      , ability = Just DrySkin
      , imageUrl = imageUrlByName "Croagunk"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 652
      , originalPokemonID = Nothing
      , fullName = "Toxicroak"
      , typing = Double Poison Fighting
      , ability = Just DrySkin
      , imageUrl = imageUrlByName "Toxicroak"
      , evolutionData = EvolvesFrom [ 651 ] "Level 37"
      , transformationData = DoesNotTransform
      }
    , { id = 653
      , originalPokemonID = Nothing
      , fullName = "Carnivine"
      , typing = Single Grass
      , ability = Just Levitate
      , imageUrl = imageUrlByName "Carnivine"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 654
      , originalPokemonID = Nothing
      , fullName = "Finneon"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Finneon"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 655
      , originalPokemonID = Nothing
      , fullName = "Lumineon"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Lumineon"
      , evolutionData = EvolvesFrom [ 654 ] "Level 31"
      , transformationData = DoesNotTransform
      }
    , { id = 657
      , originalPokemonID = Nothing
      , fullName = "Snover"
      , typing = Double Grass Ice
      , ability = Nothing
      , imageUrl = imageUrlByName "Snover"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 658
      , originalPokemonID = Nothing
      , fullName = "Abomasnow"
      , typing = Double Grass Ice
      , ability = Nothing
      , imageUrl = imageUrlByName "Abomasnow"
      , evolutionData = EvolvesFrom [ 657 ] "Level 40"
      , transformationData = DoesNotTransform
      }
    , { id = 1562
      , originalPokemonID = Nothing
      , fullName = nameFromData "Abomasnow" "Mega" ""
      , typing = Double Grass Ice
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Abomasnow" "Mega"
      , evolutionData = EvolvesFrom [ 658 ] "Holding Abomasite"
      , transformationData = DoesNotTransform
      }
    , { id = 729
      , originalPokemonID = Nothing
      , fullName = "Rotom"
      , typing = Double Electric Ghost
      , ability = Just Levitate
      , imageUrl = imageUrlByName "Rotom"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 6 "When not possessing an appliance"
      }
    , { id = 1408
      , originalPokemonID = Just 729
      , fullName = nameFromData "Rotom" "Heat" ""
      , typing = Double Electric Fire
      , ability = Just Levitate
      , imageUrl = imgUrlForAlternateForm "Rotom" "Heat"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 6 "When it possesses a microwave oven"
      }
    , { id = 1409
      , originalPokemonID = Just 729
      , fullName = nameFromData "Rotom" "Wash" ""
      , typing = Double Electric Water
      , ability = Just Levitate
      , imageUrl = imgUrlForAlternateForm "Rotom" "Wash"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 6 "When it possesses a washing machine"
      }
    , { id = 1410
      , originalPokemonID = Just 729
      , fullName = nameFromData "Rotom" "Frost" ""
      , typing = Double Electric Ice
      , ability = Just Levitate
      , imageUrl = imgUrlForAlternateForm "Rotom" "Frost"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 6 "When it possesses a refrigerator"
      }
    , { id = 1411
      , originalPokemonID = Just 729
      , fullName = nameFromData "Rotom" "Fan" ""
      , typing = Double Electric Flying
      , ability = Just Levitate
      , imageUrl = imgUrlForAlternateForm "Rotom" "Fan"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 6 "When it possesses an electric fan"
      }
    , { id = 1413
      , originalPokemonID = Just 729
      , fullName = nameFromData "Rotom" "Mow" ""
      , typing = Double Electric Grass
      , ability = Just Levitate
      , imageUrl = imgUrlForAlternateForm "Rotom" "Mow"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 6 "When it possesses a lawn mower"
      }
    , { id = 730
      , originalPokemonID = Nothing
      , fullName = "Uxie"
      , typing = Single Psychic
      , ability = Just Levitate
      , imageUrl = imageUrlByName "Uxie"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 731
      , originalPokemonID = Nothing
      , fullName = "Mesprit"
      , typing = Single Psychic
      , ability = Just Levitate
      , imageUrl = imageUrlByName "Mesprit"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 732
      , originalPokemonID = Nothing
      , fullName = "Azelf"
      , typing = Single Psychic
      , ability = Just Levitate
      , imageUrl = imageUrlByName "Azelf"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 733
      , originalPokemonID = Nothing
      , fullName = "Dialga"
      , typing = Double Steel Dragon
      , ability = Nothing
      , imageUrl = imageUrlByName "Dialga"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 734
      , originalPokemonID = Nothing
      , fullName = "Palkia"
      , typing = Double Water Dragon
      , ability = Nothing
      , imageUrl = imageUrlByName "Palkia"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 735
      , originalPokemonID = Nothing
      , fullName = "Heatran"
      , typing = Double Fire Steel
      , ability = Just FlashFire
      , imageUrl = imageUrlByName "Heatran"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 736
      , originalPokemonID = Nothing
      , fullName = "Regigigas"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Regigigas"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 737
      , originalPokemonID = Nothing
      , fullName = "Giratina"
      , typing = Double Ghost Dragon
      , ability = Nothing
      , imageUrl = imageUrlByName "Giratina"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 7 "If not holding a Griseous Orb"
      }
    , { id = 1401
      , originalPokemonID = Just 737
      , fullName = nameFromData "Giratina" "Origin" ""
      , typing = Double Ghost Dragon
      , ability = Just Levitate
      , imageUrl = imgUrlForAlternateForm "Giratina" "Origin"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 7 "While holding a Griseous Orb"
      }
    , { id = 738
      , originalPokemonID = Nothing
      , fullName = "Cresselia"
      , typing = Single Psychic
      , ability = Just Levitate
      , imageUrl = imageUrlByName "Cresselia"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 36
      , originalPokemonID = Nothing
      , fullName = "Phione"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Phione"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 37
      , originalPokemonID = Nothing
      , fullName = "Manaphy"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Manaphy"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 739
      , originalPokemonID = Nothing
      , fullName = "Darkrai"
      , typing = Single Dark
      , ability = Nothing
      , imageUrl = imageUrlByName "Darkrai"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 740
      , originalPokemonID = Nothing
      , fullName = "Shaymin"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Shaymin"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 8 "During the night or when frozen"
      }
    , { id = 1407
      , originalPokemonID = Just 740
      , fullName = nameFromData "Shaymin" "Sky" ""
      , typing = Double Grass Flying
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Shaymin" "Sky"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 8 "Use a Gracidea Flower in the daytime"
      }
    , { id = 741
      , originalPokemonID = Nothing
      , fullName = "Arceus"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Arceus"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 742
      , originalPokemonID = Nothing
      , fullName = "Victini"
      , typing = Double Psychic Fire
      , ability = Nothing
      , imageUrl = imageUrlByName "Victini"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 829
      , originalPokemonID = Nothing
      , fullName = "Snivy"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Snivy"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 830
      , originalPokemonID = Nothing
      , fullName = "Servine"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Servine"
      , evolutionData = EvolvesFrom [ 829 ] "Level 17"
      , transformationData = DoesNotTransform
      }
    , { id = 831
      , originalPokemonID = Nothing
      , fullName = "Serperior"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Serperior"
      , evolutionData = EvolvesFrom [ 830 ] "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 832
      , originalPokemonID = Nothing
      , fullName = "Tepig"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrlByName "Tepig"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 833
      , originalPokemonID = Nothing
      , fullName = "Pignite"
      , typing = Double Fire Fighting
      , ability = Nothing
      , imageUrl = imageUrlByName "Pignite"
      , evolutionData = EvolvesFrom [ 832 ] "Level 17"
      , transformationData = DoesNotTransform
      }
    , { id = 834
      , originalPokemonID = Nothing
      , fullName = "Emboar"
      , typing = Double Fire Fighting
      , ability = Nothing
      , imageUrl = imageUrlByName "Emboar"
      , evolutionData = EvolvesFrom [ 833 ] "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 835
      , originalPokemonID = Nothing
      , fullName = "Oshawott"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Oshawott"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 836
      , originalPokemonID = Nothing
      , fullName = "Dewott"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Dewott"
      , evolutionData = EvolvesFrom [ 835 ] "Level 17"
      , transformationData = DoesNotTransform
      }
    , { id = 837
      , originalPokemonID = Nothing
      , fullName = "Samurott"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Samurott"
      , evolutionData = EvolvesFrom [ 836 ] "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 1624
      , originalPokemonID = Just 837
      , fullName = nameFromData "Samurott" "Hisuian" ""
      , typing = Double Water Dark
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Samurott" "Hisuian"
      , evolutionData = EvolvesFrom [ 836 ] "Level 36 in Legends: Arceus"
      , transformationData = DoesNotTransform
      }
    , { id = 838
      , originalPokemonID = Nothing
      , fullName = "Patrat"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Patrat"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 839
      , originalPokemonID = Nothing
      , fullName = "Watchog"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Watchog"
      , evolutionData = EvolvesFrom [ 838 ] "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 840
      , originalPokemonID = Nothing
      , fullName = "Lillipup"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Lillipup"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 841
      , originalPokemonID = Nothing
      , fullName = "Herdier"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Herdier"
      , evolutionData = EvolvesFrom [ 840 ] "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 842
      , originalPokemonID = Nothing
      , fullName = "Stoutland"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Stoutland"
      , evolutionData = EvolvesFrom [ 841 ] "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 843
      , originalPokemonID = Nothing
      , fullName = "Purrloin"
      , typing = Single Dark
      , ability = Nothing
      , imageUrl = imageUrlByName "Purrloin"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 844
      , originalPokemonID = Nothing
      , fullName = "Liepard"
      , typing = Single Dark
      , ability = Nothing
      , imageUrl = imageUrlByName "Liepard"
      , evolutionData = EvolvesFrom [ 843 ] "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 845
      , originalPokemonID = Nothing
      , fullName = "Pansage"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Pansage"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 846
      , originalPokemonID = Nothing
      , fullName = "Simisage"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Simisage"
      , evolutionData = EvolvesFrom [ 845 ] "Use Leaf Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 847
      , originalPokemonID = Nothing
      , fullName = "Pansear"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrlByName "Pansear"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 848
      , originalPokemonID = Nothing
      , fullName = "Simisear"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrlByName "Simisear"
      , evolutionData = EvolvesFrom [ 847 ] "Use Fire Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 849
      , originalPokemonID = Nothing
      , fullName = "Panpour"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Panpour"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 850
      , originalPokemonID = Nothing
      , fullName = "Simipour"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Simipour"
      , evolutionData = EvolvesFrom [ 849 ] "Use Water Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 851
      , originalPokemonID = Nothing
      , fullName = "Munna"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Munna"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 852
      , originalPokemonID = Nothing
      , fullName = "Musharna"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Musharna"
      , evolutionData = EvolvesFrom [ 851 ] "Use Moon Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 853
      , originalPokemonID = Nothing
      , fullName = "Pidove"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Pidove"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 854
      , originalPokemonID = Nothing
      , fullName = "Tranquill"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Tranquill"
      , evolutionData = EvolvesFrom [ 853 ] "Level 21"
      , transformationData = DoesNotTransform
      }
    , { id = 855
      , originalPokemonID = Nothing
      , fullName = "Unfezant"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Unfezant"
      , evolutionData = EvolvesFrom [ 854 ] "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 856
      , originalPokemonID = Nothing
      , fullName = "Blitzle"
      , typing = Single Electric
      , ability = Just LightningRod
      , imageUrl = imageUrlByName "Blitzle"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 857
      , originalPokemonID = Nothing
      , fullName = "Zebstrika"
      , typing = Single Electric
      , ability = Just LightningRod
      , imageUrl = imageUrlByName "Zebstrika"
      , evolutionData = EvolvesFrom [ 856 ] "Level 27"
      , transformationData = DoesNotTransform
      }
    , { id = 858
      , originalPokemonID = Nothing
      , fullName = "Roggenrola"
      , typing = Single Rock
      , ability = Nothing
      , imageUrl = imageUrlByName "Roggenrola"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 859
      , originalPokemonID = Nothing
      , fullName = "Boldore"
      , typing = Single Rock
      , ability = Nothing
      , imageUrl = imageUrlByName "Boldore"
      , evolutionData = EvolvesFrom [ 858 ] "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 860
      , originalPokemonID = Nothing
      , fullName = "Gigalith"
      , typing = Single Rock
      , ability = Nothing
      , imageUrl = imageUrlByName "Gigalith"
      , evolutionData = EvolvesFrom [ 859 ] "Trade"
      , transformationData = DoesNotTransform
      }
    , { id = 861
      , originalPokemonID = Nothing
      , fullName = "Woobat"
      , typing = Double Psychic Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Woobat"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 862
      , originalPokemonID = Nothing
      , fullName = "Swoobat"
      , typing = Double Psychic Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Swoobat"
      , evolutionData = EvolvesFrom [ 861 ] "Level with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 863
      , originalPokemonID = Nothing
      , fullName = "Drilbur"
      , typing = Single Ground
      , ability = Nothing
      , imageUrl = imageUrlByName "Drilbur"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 864
      , originalPokemonID = Nothing
      , fullName = "Excadrill"
      , typing = Double Ground Steel
      , ability = Nothing
      , imageUrl = imageUrlByName "Excadrill"
      , evolutionData = EvolvesFrom [ 863 ] "Level 31"
      , transformationData = DoesNotTransform
      }
    , { id = 865
      , originalPokemonID = Nothing
      , fullName = "Audino"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Audino"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1563
      , originalPokemonID = Nothing
      , fullName = nameFromData "Audino" "Mega" ""
      , typing = Double Normal Fairy
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Audino" "Mega"
      , evolutionData = EvolvesFrom [ 865 ] "Holding Audinite"
      , transformationData = DoesNotTransform
      }
    , { id = 866
      , originalPokemonID = Nothing
      , fullName = "Timburr"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrlByName "Timburr"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 867
      , originalPokemonID = Nothing
      , fullName = "Gurdurr"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrlByName "Gurdurr"
      , evolutionData = EvolvesFrom [ 866 ] "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 868
      , originalPokemonID = Nothing
      , fullName = "Conkeldurr"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrlByName "Conkeldurr"
      , evolutionData = EvolvesFrom [ 867 ] "Trade"
      , transformationData = DoesNotTransform
      }
    , { id = 869
      , originalPokemonID = Nothing
      , fullName = "Tympole"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Tympole"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 870
      , originalPokemonID = Nothing
      , fullName = "Palpitoad"
      , typing = Double Water Ground
      , ability = Nothing
      , imageUrl = imageUrlByName "Palpitoad"
      , evolutionData = EvolvesFrom [ 869 ] "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 871
      , originalPokemonID = Nothing
      , fullName = "Seismitoad"
      , typing = Double Water Ground
      , ability = Nothing
      , imageUrl = imageUrlByName "Seismitoad"
      , evolutionData = EvolvesFrom [ 870 ] "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 872
      , originalPokemonID = Nothing
      , fullName = "Throh"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrlByName "Throh"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 873
      , originalPokemonID = Nothing
      , fullName = "Sawk"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrlByName "Sawk"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 874
      , originalPokemonID = Nothing
      , fullName = "Sewaddle"
      , typing = Double Bug Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Sewaddle"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 875
      , originalPokemonID = Nothing
      , fullName = "Swadloon"
      , typing = Double Bug Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Swadloon"
      , evolutionData = EvolvesFrom [ 874 ] "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 876
      , originalPokemonID = Nothing
      , fullName = "Leavanny"
      , typing = Double Bug Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Leavanny"
      , evolutionData = EvolvesFrom [ 875 ] "Level with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 877
      , originalPokemonID = Nothing
      , fullName = "Venipede"
      , typing = Double Bug Poison
      , ability = Nothing
      , imageUrl = imageUrlByName "Venipede"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 878
      , originalPokemonID = Nothing
      , fullName = "Whirlipede"
      , typing = Double Bug Poison
      , ability = Nothing
      , imageUrl = imageUrlByName "Whirlipede"
      , evolutionData = EvolvesFrom [ 877 ] "Level 22"
      , transformationData = DoesNotTransform
      }
    , { id = 879
      , originalPokemonID = Nothing
      , fullName = "Scolipede"
      , typing = Double Bug Poison
      , ability = Nothing
      , imageUrl = imageUrlByName "Scolipede"
      , evolutionData = EvolvesFrom [ 878 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 880
      , originalPokemonID = Nothing
      , fullName = "Cottonee"
      , typing = Double Grass Fairy
      , ability = Nothing
      , imageUrl = imageUrlByName "Cottonee"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 881
      , originalPokemonID = Nothing
      , fullName = "Whimsicott"
      , typing = Double Grass Fairy
      , ability = Nothing
      , imageUrl = imageUrlByName "Whimsicott"
      , evolutionData = EvolvesFrom [ 880 ] "Use Sun Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 882
      , originalPokemonID = Nothing
      , fullName = "Petilil"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Petilil"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 883
      , originalPokemonID = Nothing
      , fullName = "Lilligant"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Lilligant"
      , evolutionData = EvolvesFrom [ 882 ] "Use Sun Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 1625
      , originalPokemonID = Just 883
      , fullName = nameFromData "Lilligant" "Hisuian" ""
      , typing = Double Grass Fighting
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Lilligant" "Hisuian"
      , evolutionData = EvolvesFrom [ 882 ] "Use Sun Stone in Legends: Arceus"
      , transformationData = DoesNotTransform
      }
    , { id = 885
      , originalPokemonID = Nothing
      , fullName = "Basculin"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Basculin"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1414
      , originalPokemonID = Just 885
      , fullName = nameFromData "Basculin" "Blue-Striped" ""
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Basculin" "Blue-Striped"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1608
      , originalPokemonID = Just 885
      , fullName = nameFromData "Basculin" "White-Striped" ""
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "legends-arceus" "basculin-white-striped"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1612
      , originalPokemonID = Nothing
      , fullName = "Basculegion"
      , typing = Double Water Ghost
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "legends-arceus" "basculegion"
      , evolutionData = EvolvesFrom [ 1608 ] "After losing at least 294 HP from recoil damage"
      , transformationData = DoesNotTransform
      }
    , { id = 886
      , originalPokemonID = Nothing
      , fullName = "Sandile"
      , typing = Double Ground Dark
      , ability = Nothing
      , imageUrl = imageUrlByName "Sandile"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 887
      , originalPokemonID = Nothing
      , fullName = "Krokorok"
      , typing = Double Ground Dark
      , ability = Nothing
      , imageUrl = imageUrlByName "Krokorok"
      , evolutionData = EvolvesFrom [ 886 ] "Level 29"
      , transformationData = DoesNotTransform
      }
    , { id = 888
      , originalPokemonID = Nothing
      , fullName = "Krookodile"
      , typing = Double Ground Dark
      , ability = Nothing
      , imageUrl = imageUrlByName "Krookodile"
      , evolutionData = EvolvesFrom [ 887 ] "Level 40"
      , transformationData = DoesNotTransform
      }
    , { id = 889
      , originalPokemonID = Nothing
      , fullName = "Darumaka"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrlByName "Darumaka"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 890
      , originalPokemonID = Nothing
      , fullName = "Darmanitan"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrlByName "Darmanitan"
      , evolutionData = EvolvesFrom [ 889 ] "Level 35"
      , transformationData = Transforms 9 "With HP above half"
      }
    , { id = 1554
      , originalPokemonID = Just 890
      , fullName = nameFromData "Darmanitan" "Zen" ""
      , typing = Double Fire Psychic
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Darmanitan" "Zen"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 9 "With HP below half"
      }
    , { id = 1557
      , originalPokemonID = Just 889
      , fullName = nameFromData "Darumaka" "Galarian" ""
      , typing = Single Ice
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Darumaka" "Galarian"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1555
      , originalPokemonID = Just 890
      , fullName = nameFromData "Darmanitan" "Galarian" ""
      , typing = Single Ice
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "home" "darmanitan-galarian-standard"
      , evolutionData = EvolvesFrom [ 1557 ] "Use Ice Stone"
      , transformationData = Transforms 10 "With HP above half"
      }
    , { id = 1556
      , originalPokemonID = Just 890
      , fullName = nameFromData "Darmanitan" "Galarian Zen" ""
      , typing = Double Ice Fire
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "home" "darmanitan-galarian-zen"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 10 "With HP below half"
      }
    , { id = 891
      , originalPokemonID = Nothing
      , fullName = "Maractus"
      , typing = Single Grass
      , ability = Just WaterAbsorb
      , imageUrl = imageUrlByName "Maractus"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 892
      , originalPokemonID = Nothing
      , fullName = "Dwebble"
      , typing = Double Bug Rock
      , ability = Nothing
      , imageUrl = imageUrlByName "Dwebble"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 893
      , originalPokemonID = Nothing
      , fullName = "Crustle"
      , typing = Double Bug Rock
      , ability = Nothing
      , imageUrl = imageUrlByName "Crustle"
      , evolutionData = EvolvesFrom [ 892 ] "Level 34"
      , transformationData = DoesNotTransform
      }
    , { id = 894
      , originalPokemonID = Nothing
      , fullName = "Scraggy"
      , typing = Double Dark Fighting
      , ability = Nothing
      , imageUrl = imageUrlByName "Scraggy"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 914
      , originalPokemonID = Nothing
      , fullName = "Scrafty"
      , typing = Double Dark Fighting
      , ability = Nothing
      , imageUrl = imageUrlByName "Scrafty"
      , evolutionData = EvolvesFrom [ 894 ] "Level 39"
      , transformationData = DoesNotTransform
      }
    , { id = 915
      , originalPokemonID = Nothing
      , fullName = "Sigilyph"
      , typing = Double Psychic Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Sigilyph"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 916
      , originalPokemonID = Nothing
      , fullName = "Yamask"
      , typing = Single Ghost
      , ability = Nothing
      , imageUrl = imageUrlByName "Yamask"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 917
      , originalPokemonID = Nothing
      , fullName = "Cofagrigus"
      , typing = Single Ghost
      , ability = Nothing
      , imageUrl = imageUrlByName "Cofagrigus"
      , evolutionData = EvolvesFrom [ 916 ] "Level 34"
      , transformationData = DoesNotTransform
      }
    , { id = 1464
      , originalPokemonID = Just 916
      , fullName = nameFromData "Yamask" "Galarian" ""
      , typing = Double Ground Ghost
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Yamask" "Galarian"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1465
      , originalPokemonID = Nothing
      , fullName = "Runerigus"
      , typing = Double Ground Ghost
      , ability = Nothing
      , imageUrl = imageUrlByName "Runerigus"
      , evolutionData = EvolvesFrom [ 1464 ] "Take 49+ damage and travel under the Stone Bridge in Dusty Bowl in Galar"
      , transformationData = DoesNotTransform
      }
    , { id = 918
      , originalPokemonID = Nothing
      , fullName = "Tirtouga"
      , typing = Double Water Rock
      , ability = Nothing
      , imageUrl = imageUrlByName "Tirtouga"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 919
      , originalPokemonID = Nothing
      , fullName = "Carracosta"
      , typing = Double Water Rock
      , ability = Nothing
      , imageUrl = imageUrlByName "Carracosta"
      , evolutionData = EvolvesFrom [ 918 ] "Level 37"
      , transformationData = DoesNotTransform
      }
    , { id = 920
      , originalPokemonID = Nothing
      , fullName = "Archen"
      , typing = Double Rock Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Archen"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 921
      , originalPokemonID = Nothing
      , fullName = "Archeops"
      , typing = Double Rock Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Archeops"
      , evolutionData = EvolvesFrom [ 920 ] "Level 37"
      , transformationData = DoesNotTransform
      }
    , { id = 922
      , originalPokemonID = Nothing
      , fullName = "Trubbish"
      , typing = Single Poison
      , ability = Nothing
      , imageUrl = imageUrlByName "Trubbish"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 923
      , originalPokemonID = Nothing
      , fullName = "Garbodor"
      , typing = Single Poison
      , ability = Nothing
      , imageUrl = imageUrlByName "Garbodor"
      , evolutionData = EvolvesFrom [ 922 ] "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 939
      , originalPokemonID = Nothing
      , fullName = "Zorua"
      , typing = Single Dark
      , ability = Nothing
      , imageUrl = imageUrlByName "Zorua"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 940
      , originalPokemonID = Nothing
      , fullName = "Zoroark"
      , typing = Single Dark
      , ability = Nothing
      , imageUrl = imageUrlByName "Zoroark"
      , evolutionData = EvolvesFrom [ 939 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 1626
      , originalPokemonID = Just 939
      , fullName = nameFromData "Zorua" "Hisuian" ""
      , typing = Double Normal Ghost
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Zorua" "Hisuian"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1627
      , originalPokemonID = Just 940
      , fullName = nameFromData "Zoroark" "Hisuian" ""
      , typing = Double Normal Ghost
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Zoroark" "Hisuian"
      , evolutionData = EvolvesFrom [ 1626 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 941
      , originalPokemonID = Nothing
      , fullName = "Minccino"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Minccino"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 942
      , originalPokemonID = Nothing
      , fullName = "Cinccino"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Cinccino"
      , evolutionData = EvolvesFrom [ 941 ] "Use Shiny Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 943
      , originalPokemonID = Nothing
      , fullName = "Gothita"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Gothita"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 944
      , originalPokemonID = Nothing
      , fullName = "Gothorita"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Gothorita"
      , evolutionData = EvolvesFrom [ 943 ] "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 945
      , originalPokemonID = Nothing
      , fullName = "Gothitelle"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Gothitelle"
      , evolutionData = EvolvesFrom [ 944 ] "Level 41"
      , transformationData = DoesNotTransform
      }
    , { id = 946
      , originalPokemonID = Nothing
      , fullName = "Solosis"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Solosis"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 947
      , originalPokemonID = Nothing
      , fullName = "Duosion"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Duosion"
      , evolutionData = EvolvesFrom [ 946 ] "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 948
      , originalPokemonID = Nothing
      , fullName = "Reuniclus"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Reuniclus"
      , evolutionData = EvolvesFrom [ 947 ] "Level 41"
      , transformationData = DoesNotTransform
      }
    , { id = 950
      , originalPokemonID = Nothing
      , fullName = "Ducklett"
      , typing = Double Water Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Ducklett"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 951
      , originalPokemonID = Nothing
      , fullName = "Swanna"
      , typing = Double Water Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Swanna"
      , evolutionData = EvolvesFrom [ 950 ] "Level 35"
      , transformationData = DoesNotTransform
      }
    , { id = 952
      , originalPokemonID = Nothing
      , fullName = "Vanillite"
      , typing = Single Ice
      , ability = Nothing
      , imageUrl = imageUrlByName "Vanillite"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 953
      , originalPokemonID = Nothing
      , fullName = "Vanillish"
      , typing = Single Ice
      , ability = Nothing
      , imageUrl = imageUrlByName "Vanillish"
      , evolutionData = EvolvesFrom [ 952 ] "Level 35"
      , transformationData = DoesNotTransform
      }
    , { id = 954
      , originalPokemonID = Nothing
      , fullName = "Vanilluxe"
      , typing = Single Ice
      , ability = Nothing
      , imageUrl = imageUrlByName "Vanilluxe"
      , evolutionData = EvolvesFrom [ 953 ] "Level 47"
      , transformationData = DoesNotTransform
      }
    , { id = 955
      , originalPokemonID = Nothing
      , fullName = "Deerling"
      , typing = Double Normal Grass
      , ability = Just SapSipper
      , imageUrl = imageUrlByName "Deerling"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 956
      , originalPokemonID = Nothing
      , fullName = "Sawsbuck"
      , typing = Double Normal Grass
      , ability = Just SapSipper
      , imageUrl = imageUrlByName "Sawsbuck"
      , evolutionData = EvolvesFrom [ 955 ] "Level 34"
      , transformationData = DoesNotTransform
      }
    , { id = 957
      , originalPokemonID = Nothing
      , fullName = "Emolga"
      , typing = Double Electric Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Emolga"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 958
      , originalPokemonID = Nothing
      , fullName = "Karrablast"
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imageUrlByName "Karrablast"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 959
      , originalPokemonID = Nothing
      , fullName = "Escavalier"
      , typing = Double Bug Steel
      , ability = Nothing
      , imageUrl = imageUrlByName "Escavalier"
      , evolutionData = EvolvesFrom [ 958 ] "Trade for a Shelmet"
      , transformationData = DoesNotTransform
      }
    , { id = 961
      , originalPokemonID = Nothing
      , fullName = "Foongus"
      , typing = Double Grass Poison
      , ability = Nothing
      , imageUrl = imageUrlByName "Foongus"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 962
      , originalPokemonID = Nothing
      , fullName = "Amoonguss"
      , typing = Double Grass Poison
      , ability = Nothing
      , imageUrl = imageUrlByName "Amoonguss"
      , evolutionData = EvolvesFrom [ 961 ] "Level 39"
      , transformationData = DoesNotTransform
      }
    , { id = 963
      , originalPokemonID = Nothing
      , fullName = "Frillish"
      , typing = Double Water Ghost
      , ability = Just WaterAbsorb
      , imageUrl = imageUrlByName "Frillish"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 964
      , originalPokemonID = Nothing
      , fullName = "Jellicent"
      , typing = Double Water Ghost
      , ability = Just WaterAbsorb
      , imageUrl = imageUrlByName "Jellicent"
      , evolutionData = EvolvesFrom [ 963 ] "Level 40"
      , transformationData = DoesNotTransform
      }
    , { id = 965
      , originalPokemonID = Nothing
      , fullName = "Alomomola"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Alomomola"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 966
      , originalPokemonID = Nothing
      , fullName = "Joltik"
      , typing = Double Bug Electric
      , ability = Nothing
      , imageUrl = imageUrlByName "Joltik"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 967
      , originalPokemonID = Nothing
      , fullName = "Galvantula"
      , typing = Double Bug Electric
      , ability = Nothing
      , imageUrl = imageUrlByName "Galvantula"
      , evolutionData = EvolvesFrom [ 966 ] "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 968
      , originalPokemonID = Nothing
      , fullName = "Ferroseed"
      , typing = Double Grass Steel
      , ability = Nothing
      , imageUrl = imageUrlByName "Ferroseed"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 969
      , originalPokemonID = Nothing
      , fullName = "Ferrothorn"
      , typing = Double Grass Steel
      , ability = Nothing
      , imageUrl = imageUrlByName "Ferrothorn"
      , evolutionData = EvolvesFrom [ 968 ] "Level 40"
      , transformationData = DoesNotTransform
      }
    , { id = 970
      , originalPokemonID = Nothing
      , fullName = "Klink"
      , typing = Single Steel
      , ability = Nothing
      , imageUrl = imageUrlByName "Klink"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 972
      , originalPokemonID = Nothing
      , fullName = "Klang"
      , typing = Single Steel
      , ability = Nothing
      , imageUrl = imageUrlByName "Klang"
      , evolutionData = EvolvesFrom [ 970 ] "Level 38"
      , transformationData = DoesNotTransform
      }
    , { id = 973
      , originalPokemonID = Nothing
      , fullName = "Klinklang"
      , typing = Single Steel
      , ability = Nothing
      , imageUrl = imageUrlByName "Klinklang"
      , evolutionData = EvolvesFrom [ 972 ] "Level 49"
      , transformationData = DoesNotTransform
      }
    , { id = 974
      , originalPokemonID = Nothing
      , fullName = "Tynamo"
      , typing = Single Electric
      , ability = Just Levitate
      , imageUrl = imageUrlByName "Tynamo"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 975
      , originalPokemonID = Nothing
      , fullName = "Eelektrik"
      , typing = Single Electric
      , ability = Just Levitate
      , imageUrl = imageUrlByName "Eelektrik"
      , evolutionData = EvolvesFrom [ 974 ] "Level 39"
      , transformationData = DoesNotTransform
      }
    , { id = 976
      , originalPokemonID = Nothing
      , fullName = "Eelektross"
      , typing = Single Electric
      , ability = Just Levitate
      , imageUrl = imageUrlByName "Eelektross"
      , evolutionData = EvolvesFrom [ 975 ] "Use Thunder Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 977
      , originalPokemonID = Nothing
      , fullName = "Elgyem"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Elgyem"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 978
      , originalPokemonID = Nothing
      , fullName = "Beheeyem"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Beheeyem"
      , evolutionData = EvolvesFrom [ 977 ] "Level 42"
      , transformationData = DoesNotTransform
      }
    , { id = 979
      , originalPokemonID = Nothing
      , fullName = "Litwick"
      , typing = Double Ghost Fire
      , ability = Just FlashFire
      , imageUrl = imageUrlByName "Litwick"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 980
      , originalPokemonID = Nothing
      , fullName = "Lampent"
      , typing = Double Ghost Fire
      , ability = Just FlashFire
      , imageUrl = imageUrlByName "Lampent"
      , evolutionData = EvolvesFrom [ 979 ] "Level 41"
      , transformationData = DoesNotTransform
      }
    , { id = 981
      , originalPokemonID = Nothing
      , fullName = "Chandelure"
      , typing = Double Ghost Fire
      , ability = Just FlashFire
      , imageUrl = imageUrlByName "Chandelure"
      , evolutionData = EvolvesFrom [ 980 ] "Use Dusk Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 983
      , originalPokemonID = Nothing
      , fullName = "Axew"
      , typing = Single Dragon
      , ability = Nothing
      , imageUrl = imageUrlByName "Axew"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 984
      , originalPokemonID = Nothing
      , fullName = "Fraxure"
      , typing = Single Dragon
      , ability = Nothing
      , imageUrl = imageUrlByName "Fraxure"
      , evolutionData = EvolvesFrom [ 983 ] "Level 38"
      , transformationData = DoesNotTransform
      }
    , { id = 985
      , originalPokemonID = Nothing
      , fullName = "Haxorus"
      , typing = Single Dragon
      , ability = Nothing
      , imageUrl = imageUrlByName "Haxorus"
      , evolutionData = EvolvesFrom [ 984 ] "Level 48"
      , transformationData = DoesNotTransform
      }
    , { id = 986
      , originalPokemonID = Nothing
      , fullName = "Cubchoo"
      , typing = Single Ice
      , ability = Nothing
      , imageUrl = imageUrlByName "Cubchoo"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 987
      , originalPokemonID = Nothing
      , fullName = "Beartic"
      , typing = Single Ice
      , ability = Nothing
      , imageUrl = imageUrlByName "Beartic"
      , evolutionData = EvolvesFrom [ 986 ] "Level 37"
      , transformationData = DoesNotTransform
      }
    , { id = 988
      , originalPokemonID = Nothing
      , fullName = "Cryogonal"
      , typing = Single Ice
      , ability = Just Levitate
      , imageUrl = imageUrlByName "Cryogonal"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 989
      , originalPokemonID = Nothing
      , fullName = "Shelmet"
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imageUrlByName "Shelmet"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 990
      , originalPokemonID = Nothing
      , fullName = "Accelgor"
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imageUrlByName "Accelgor"
      , evolutionData = EvolvesFrom [ 989 ] "Trade for Karrablast"
      , transformationData = DoesNotTransform
      }
    , { id = 991
      , originalPokemonID = Nothing
      , fullName = "Stunfisk"
      , typing = Double Ground Electric
      , ability = Nothing
      , imageUrl = imageUrlByName "Stunfisk"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1553
      , originalPokemonID = Just 991
      , fullName = nameFromData "Stunfisk" "Galarian" ""
      , typing = Double Ground Steel
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Stunfisk" "Galarian"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 992
      , originalPokemonID = Nothing
      , fullName = "Mienfoo"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrlByName "Mienfoo"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 994
      , originalPokemonID = Nothing
      , fullName = "Mienshao"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrlByName "Mienshao"
      , evolutionData = EvolvesFrom [ 992 ] "Level 50"
      , transformationData = DoesNotTransform
      }
    , { id = 995
      , originalPokemonID = Nothing
      , fullName = "Druddigon"
      , typing = Single Dragon
      , ability = Nothing
      , imageUrl = imageUrlByName "Druddigon"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 996
      , originalPokemonID = Nothing
      , fullName = "Golett"
      , typing = Double Ground Ghost
      , ability = Nothing
      , imageUrl = imageUrlByName "Golett"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 997
      , originalPokemonID = Nothing
      , fullName = "Golurk"
      , typing = Double Ground Ghost
      , ability = Nothing
      , imageUrl = imageUrlByName "Golurk"
      , evolutionData = EvolvesFrom [ 996 ] "Level 43"
      , transformationData = DoesNotTransform
      }
    , { id = 998
      , originalPokemonID = Nothing
      , fullName = "Pawniard"
      , typing = Double Dark Steel
      , ability = Nothing
      , imageUrl = imageUrlByName "Pawniard"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 999
      , originalPokemonID = Nothing
      , fullName = "Bisharp"
      , typing = Double Dark Steel
      , ability = Nothing
      , imageUrl = imageUrlByName "Bisharp"
      , evolutionData = EvolvesFrom [ 998 ] "Level 52"
      , transformationData = DoesNotTransform
      }
    , { id = 1711
      , originalPokemonID = Nothing
      , fullName = "Kingambit"
      , typing = Double Dark Steel
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "kingambit"
      , evolutionData = EvolvesFrom [ 999 ] "Level after defeating three Bisharp that lead a pack of Pawniard"
      , transformationData = DoesNotTransform
      }
    , { id = 1000
      , originalPokemonID = Nothing
      , fullName = "Bouffalant"
      , typing = Single Normal
      , ability = Just SapSipper
      , imageUrl = imageUrlByName "Bouffalant"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1001
      , originalPokemonID = Nothing
      , fullName = "Rufflet"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Rufflet"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1002
      , originalPokemonID = Nothing
      , fullName = "Braviary"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Braviary"
      , evolutionData = EvolvesFrom [ 1001 ] "Level 54"
      , transformationData = DoesNotTransform
      }
    , { id = 1628
      , originalPokemonID = Just 1002
      , fullName = nameFromData "Braviary" "Hisuian" ""
      , typing = Double Psychic Flying
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Braviary" "Hisuian"
      , evolutionData = EvolvesFrom [ 1001 ] "Level 54 in Legends: Arceus"
      , transformationData = DoesNotTransform
      }
    , { id = 1003
      , originalPokemonID = Nothing
      , fullName = "Vullaby"
      , typing = Double Dark Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Vullaby"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1005
      , originalPokemonID = Nothing
      , fullName = "Mandibuzz"
      , typing = Double Dark Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Mandibuzz"
      , evolutionData = EvolvesFrom [ 1003 ] "Level 54"
      , transformationData = DoesNotTransform
      }
    , { id = 1006
      , originalPokemonID = Nothing
      , fullName = "Heatmor"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrlByName "Heatmor"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1007
      , originalPokemonID = Nothing
      , fullName = "Durant"
      , typing = Double Bug Steel
      , ability = Nothing
      , imageUrl = imageUrlByName "Durant"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1008
      , originalPokemonID = Nothing
      , fullName = "Deino"
      , typing = Double Dark Dragon
      , ability = Nothing
      , imageUrl = imageUrlByName "Deino"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1009
      , originalPokemonID = Nothing
      , fullName = "Zweilous"
      , typing = Double Dark Dragon
      , ability = Nothing
      , imageUrl = imageUrlByName "Zweilous"
      , evolutionData = EvolvesFrom [ 1008 ] "Level 50"
      , transformationData = DoesNotTransform
      }
    , { id = 1010
      , originalPokemonID = Nothing
      , fullName = "Hydreigon"
      , typing = Double Dark Dragon
      , ability = Just Levitate
      , imageUrl = imageUrlByName "Hydreigon"
      , evolutionData = EvolvesFrom [ 1009 ] "Level 64"
      , transformationData = DoesNotTransform
      }
    , { id = 1011
      , originalPokemonID = Nothing
      , fullName = "Larvesta"
      , typing = Double Bug Fire
      , ability = Nothing
      , imageUrl = imageUrlByName "Larvesta"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1012
      , originalPokemonID = Nothing
      , fullName = "Volcarona"
      , typing = Double Bug Fire
      , ability = Nothing
      , imageUrl = imageUrlByName "Volcarona"
      , evolutionData = EvolvesFrom [ 1011 ] "Level 59"
      , transformationData = DoesNotTransform
      }
    , { id = 1013
      , originalPokemonID = Nothing
      , fullName = "Cobalion"
      , typing = Double Steel Fighting
      , ability = Nothing
      , imageUrl = imageUrlByName "Cobalion"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1014
      , originalPokemonID = Nothing
      , fullName = "Terrakion"
      , typing = Double Rock Fighting
      , ability = Nothing
      , imageUrl = imageUrlByName "Terrakion"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1016
      , originalPokemonID = Nothing
      , fullName = "Virizion"
      , typing = Double Grass Fighting
      , ability = Nothing
      , imageUrl = imageUrlByName "Virizion"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1017
      , originalPokemonID = Nothing
      , fullName = "Tornadus"
      , typing = Single Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Tornadus"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 11 "Use the Reveal Glass"
      }
    , { id = 1415
      , originalPokemonID = Just 1017
      , fullName = nameFromData "Tornadus" "Therian" ""
      , typing = Single Flying
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Tornadus" "Therian"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 11 "Use the Reveal Glass"
      }
    , { id = 1018
      , originalPokemonID = Nothing
      , fullName = "Thundurus"
      , typing = Double Electric Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Thundurus"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 12 "Use the Reveal Glass"
      }
    , { id = 1416
      , originalPokemonID = Just 1018
      , fullName = nameFromData "Thundurus" "Therian" ""
      , typing = Double Electric Flying
      , ability = Just VoltAbsorb
      , imageUrl = imgUrlForAlternateForm "Thundurus" "Therian"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 12 "Use the Reveal Glass"
      }
    , { id = 1019
      , originalPokemonID = Nothing
      , fullName = "Reshiram"
      , typing = Double Dragon Fire
      , ability = Nothing
      , imageUrl = imageUrlByName "Reshiram"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1020
      , originalPokemonID = Nothing
      , fullName = "Zekrom"
      , typing = Double Dragon Electric
      , ability = Nothing
      , imageUrl = imageUrlByName "Zekrom"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1021
      , originalPokemonID = Nothing
      , fullName = "Landorus"
      , typing = Double Ground Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Landorus"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 13 "Use the Reveal Glass"
      }
    , { id = 1417
      , originalPokemonID = Just 1021
      , fullName = nameFromData "Landorus" "Therian" ""
      , typing = Double Ground Flying
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Landorus" "Therian"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 13 "Use the Reveal Glass"
      }
    , { id = 1022
      , originalPokemonID = Nothing
      , fullName = "Kyurem"
      , typing = Double Dragon Ice
      , ability = Nothing
      , imageUrl = imageUrlByName "Kyurem"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 14 "Undo DNA Splicing"
      }
    , { id = 1421
      , originalPokemonID = Just 1022
      , fullName = nameFromData "Kyurem" "White" ""
      , typing = Double Dragon Ice
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Kyurem" "White"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 14 "DNA Splice with Reshiram"
      }
    , { id = 1423
      , originalPokemonID = Just 1022
      , fullName = nameFromData "Kyurem" "Black" ""
      , typing = Double Dragon Ice
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Kyurem" "Black"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 14 "DNA Splice with Zekrom"
      }
    , { id = 1023
      , originalPokemonID = Nothing
      , fullName = "Keldeo"
      , typing = Double Water Fighting
      , ability = Nothing
      , imageUrl = imageUrlByName "Keldeo"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1024
      , originalPokemonID = Nothing
      , fullName = "Meloetta"
      , typing = Double Normal Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Meloetta"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 15 "Use the move Relic Song"
      }
    , { id = 1570
      , originalPokemonID = Just 1024
      , fullName = nameFromData "Meloetta" "Pirouette" ""
      , typing = Double Normal Fighting
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Meloetta" "Pirouette"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 15 "Use the move Relic Song"
      }
    , { id = 1025
      , originalPokemonID = Nothing
      , fullName = "Genesect"
      , typing = Double Bug Steel
      , ability = Nothing
      , imageUrl = imageUrlByName "Genesect"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1066
      , originalPokemonID = Nothing
      , fullName = "Chespin"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Chespin"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1067
      , originalPokemonID = Nothing
      , fullName = "Quilladin"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Quilladin"
      , evolutionData = EvolvesFrom [ 1066 ] "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 1068
      , originalPokemonID = Nothing
      , fullName = "Chesnaught"
      , typing = Double Grass Fighting
      , ability = Nothing
      , imageUrl = imageUrlByName "Chesnaught"
      , evolutionData = EvolvesFrom [ 1067 ] "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 1069
      , originalPokemonID = Nothing
      , fullName = "Fennekin"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrlByName "Fennekin"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1070
      , originalPokemonID = Nothing
      , fullName = "Braixen"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrlByName "Braixen"
      , evolutionData = EvolvesFrom [ 1069 ] "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 1071
      , originalPokemonID = Nothing
      , fullName = "Delphox"
      , typing = Double Fire Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Delphox"
      , evolutionData = EvolvesFrom [ 1070 ] "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 1072
      , originalPokemonID = Nothing
      , fullName = "Froakie"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Froakie"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1073
      , originalPokemonID = Nothing
      , fullName = "Frogadier"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Frogadier"
      , evolutionData = EvolvesFrom [ 1072 ] "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 1074
      , originalPokemonID = Nothing
      , fullName = "Greninja"
      , typing = Double Water Dark
      , ability = Nothing
      , imageUrl = imageUrlByName "Greninja"
      , evolutionData = EvolvesFrom [ 1073 ] "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 1075
      , originalPokemonID = Nothing
      , fullName = "Bunnelby"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Bunnelby"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1077
      , originalPokemonID = Nothing
      , fullName = "Diggersby"
      , typing = Double Normal Ground
      , ability = Nothing
      , imageUrl = imageUrlByName "Diggersby"
      , evolutionData = EvolvesFrom [ 1075 ] "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 1078
      , originalPokemonID = Nothing
      , fullName = "Fletchling"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Fletchling"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1079
      , originalPokemonID = Nothing
      , fullName = "Fletchinder"
      , typing = Double Fire Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Fletchinder"
      , evolutionData = EvolvesFrom [ 1078 ] "Level 17"
      , transformationData = DoesNotTransform
      }
    , { id = 1080
      , originalPokemonID = Nothing
      , fullName = "Talonflame"
      , typing = Double Fire Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Talonflame"
      , evolutionData = EvolvesFrom [ 1079 ] "Level 35"
      , transformationData = DoesNotTransform
      }
    , { id = 1081
      , originalPokemonID = Nothing
      , fullName = "Scatterbug"
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imageUrlByName "Scatterbug"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1082
      , originalPokemonID = Nothing
      , fullName = "Spewpa"
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imageUrlByName "Spewpa"
      , evolutionData = EvolvesFrom [ 1081 ] "Level 9"
      , transformationData = DoesNotTransform
      }
    , { id = 1083
      , originalPokemonID = Nothing
      , fullName = "Vivillon"
      , typing = Double Bug Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Vivillon"
      , evolutionData = EvolvesFrom [ 1082 ] "Level 12"
      , transformationData = DoesNotTransform
      }
    , { id = 1084
      , originalPokemonID = Nothing
      , fullName = "Litleo"
      , typing = Double Fire Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Litleo"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1085
      , originalPokemonID = Nothing
      , fullName = "Pyroar"
      , typing = Double Fire Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Pyroar"
      , evolutionData = EvolvesFrom [ 1084 ] "Level 35"
      , transformationData = DoesNotTransform
      }
    , { id = 1086
      , originalPokemonID = Nothing
      , fullName = "Flabébé"
      , typing = Single Fairy
      , ability = Nothing
      , imageUrl = imageUrlByName "Flabébé"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1088
      , originalPokemonID = Nothing
      , fullName = "Floette"
      , typing = Single Fairy
      , ability = Nothing
      , imageUrl = imageUrlByName "Floette"
      , evolutionData = EvolvesFrom [ 1086 ] "Level 19"
      , transformationData = DoesNotTransform
      }
    , { id = 1089
      , originalPokemonID = Nothing
      , fullName = "Florges"
      , typing = Single Fairy
      , ability = Nothing
      , imageUrl = imageUrlByName "Florges"
      , evolutionData = EvolvesFrom [ 1088 ] "Use Shiny Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 1090
      , originalPokemonID = Nothing
      , fullName = "Skiddo"
      , typing = Single Grass
      , ability = Just SapSipper
      , imageUrl = imageUrlByName "Skiddo"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1091
      , originalPokemonID = Nothing
      , fullName = "Gogoat"
      , typing = Single Grass
      , ability = Just SapSipper
      , imageUrl = imageUrlByName "Gogoat"
      , evolutionData = EvolvesFrom [ 1090 ] "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 1092
      , originalPokemonID = Nothing
      , fullName = "Pancham"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrlByName "Pancham"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1093
      , originalPokemonID = Nothing
      , fullName = "Pangoro"
      , typing = Double Fighting Dark
      , ability = Nothing
      , imageUrl = imageUrlByName "Pangoro"
      , evolutionData = EvolvesFrom [ 1092 ] "Level 32 With Dark-Type Pokemon In Party"
      , transformationData = DoesNotTransform
      }
    , { id = 1094
      , originalPokemonID = Nothing
      , fullName = "Furfrou"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Furfrou"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1095
      , originalPokemonID = Nothing
      , fullName = "Espurr"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Espurr"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1096
      , originalPokemonID = Nothing
      , fullName = "Meowstic"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Meowstic"
      , evolutionData = EvolvesFrom [ 1095 ] "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 1580
      , originalPokemonID = Just 1096
      , fullName = nameFromData "Meowstic" "Female" ""
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Meowstic" "Female"
      , evolutionData = EvolvesFrom [ 1095 ] "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 25
      , originalPokemonID = Nothing
      , fullName = "Honedge"
      , typing = Double Steel Ghost
      , ability = Nothing
      , imageUrl = imageUrlByName "Honedge"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 26
      , originalPokemonID = Nothing
      , fullName = "Doublade"
      , typing = Double Steel Ghost
      , ability = Nothing
      , imageUrl = imageUrlByName "Doublade"
      , evolutionData = EvolvesFrom [ 25 ] "Level 35"
      , transformationData = DoesNotTransform
      }
    , { id = 1098
      , originalPokemonID = Nothing
      , fullName = "Aegislash"
      , typing = Double Steel Ghost
      , ability = Nothing
      , imageUrl = imageUrlByName "Aegislash"
      , evolutionData = EvolvesFrom [ 26 ] "Use Dusk Stone"
      , transformationData = Transforms 16 "Use the move King's Shield"
      }
    , { id = 1370
      , originalPokemonID = Just 1098
      , fullName = nameFromData "Aegislash" "Blade" ""
      , typing = Double Steel Ghost
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Aegislash" "Blade"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 16 "Use a damaging move"
      }
    , { id = 1101
      , originalPokemonID = Nothing
      , fullName = "Spritzee"
      , typing = Single Fairy
      , ability = Nothing
      , imageUrl = imageUrlByName "Spritzee"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1102
      , originalPokemonID = Nothing
      , fullName = "Aromatisse"
      , typing = Single Fairy
      , ability = Nothing
      , imageUrl = imageUrlByName "Aromatisse"
      , evolutionData = EvolvesFrom [ 1101 ] "Trade holding Sachet"
      , transformationData = DoesNotTransform
      }
    , { id = 1103
      , originalPokemonID = Nothing
      , fullName = "Swirlix"
      , typing = Single Fairy
      , ability = Nothing
      , imageUrl = imageUrlByName "Swirlix"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1104
      , originalPokemonID = Nothing
      , fullName = "Slurpuff"
      , typing = Single Fairy
      , ability = Nothing
      , imageUrl = imageUrlByName "Slurpuff"
      , evolutionData = EvolvesFrom [ 1103 ] "Trade holding Whipped Dream"
      , transformationData = DoesNotTransform
      }
    , { id = 1105
      , originalPokemonID = Nothing
      , fullName = "Inkay"
      , typing = Double Dark Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Inkay"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1106
      , originalPokemonID = Nothing
      , fullName = "Malamar"
      , typing = Double Dark Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Malamar"
      , evolutionData = EvolvesFrom [ 1105 ] "Level 30 With System Or Controller Upside-Down"
      , transformationData = DoesNotTransform
      }
    , { id = 1107
      , originalPokemonID = Nothing
      , fullName = "Binacle"
      , typing = Double Rock Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Binacle"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1108
      , originalPokemonID = Nothing
      , fullName = "Barbaracle"
      , typing = Double Rock Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Barbaracle"
      , evolutionData = EvolvesFrom [ 1107 ] "Level 39"
      , transformationData = DoesNotTransform
      }
    , { id = 1110
      , originalPokemonID = Nothing
      , fullName = "Skrelp"
      , typing = Double Poison Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Skrelp"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1111
      , originalPokemonID = Nothing
      , fullName = "Dragalge"
      , typing = Double Poison Dragon
      , ability = Nothing
      , imageUrl = imageUrlByName "Dragalge"
      , evolutionData = EvolvesFrom [ 1110 ] "Level 48"
      , transformationData = DoesNotTransform
      }
    , { id = 1112
      , originalPokemonID = Nothing
      , fullName = "Clauncher"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Clauncher"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1113
      , originalPokemonID = Nothing
      , fullName = "Clawitzer"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Clawitzer"
      , evolutionData = EvolvesFrom [ 1112 ] "Level 37"
      , transformationData = DoesNotTransform
      }
    , { id = 1122
      , originalPokemonID = Nothing
      , fullName = "Helioptile"
      , typing = Double Electric Normal
      , ability = Just DrySkin
      , imageUrl = imageUrlByName "Helioptile"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1123
      , originalPokemonID = Nothing
      , fullName = "Heliolisk"
      , typing = Double Electric Normal
      , ability = Just DrySkin
      , imageUrl = imageUrlByName "Heliolisk"
      , evolutionData = EvolvesFrom [ 1122 ] "Use Sun Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 1124
      , originalPokemonID = Nothing
      , fullName = "Tyrunt"
      , typing = Double Rock Dragon
      , ability = Nothing
      , imageUrl = imageUrlByName "Tyrunt"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1125
      , originalPokemonID = Nothing
      , fullName = "Tyrantrum"
      , typing = Double Rock Dragon
      , ability = Nothing
      , imageUrl = imageUrlByName "Tyrantrum"
      , evolutionData = EvolvesFrom [ 1124 ] "Level 39 during the day"
      , transformationData = DoesNotTransform
      }
    , { id = 1126
      , originalPokemonID = Nothing
      , fullName = "Amaura"
      , typing = Double Rock Ice
      , ability = Nothing
      , imageUrl = imageUrlByName "Amaura"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1127
      , originalPokemonID = Nothing
      , fullName = "Aurorus"
      , typing = Double Rock Ice
      , ability = Nothing
      , imageUrl = imageUrlByName "Aurorus"
      , evolutionData = EvolvesFrom [ 1126 ] "Level 39 at night"
      , transformationData = DoesNotTransform
      }
    , { id = 1130
      , originalPokemonID = Nothing
      , fullName = "Hawlucha"
      , typing = Double Fighting Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Hawlucha"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1131
      , originalPokemonID = Nothing
      , fullName = "Dedenne"
      , typing = Double Electric Fairy
      , ability = Nothing
      , imageUrl = imageUrlByName "Dedenne"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1132
      , originalPokemonID = Nothing
      , fullName = "Carbink"
      , typing = Double Rock Fairy
      , ability = Nothing
      , imageUrl = imageUrlByName "Carbink"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1133
      , originalPokemonID = Nothing
      , fullName = "Goomy"
      , typing = Single Dragon
      , ability = Just SapSipper
      , imageUrl = imageUrlByName "Goomy"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1134
      , originalPokemonID = Nothing
      , fullName = "Sliggoo"
      , typing = Single Dragon
      , ability = Just SapSipper
      , imageUrl = imageUrlByName "Sliggoo"
      , evolutionData = EvolvesFrom [ 1133 ] "Level 40"
      , transformationData = DoesNotTransform
      }
    , { id = 1135
      , originalPokemonID = Nothing
      , fullName = "Goodra"
      , typing = Single Dragon
      , ability = Just SapSipper
      , imageUrl = imageUrlByName "Goodra"
      , evolutionData = EvolvesFrom [ 1134 ] "Level 50 When Raining or Foggy outside battle"
      , transformationData = DoesNotTransform
      }
    , { id = 1629
      , originalPokemonID = Just 1134
      , fullName = nameFromData "Sliggoo" "Hisuian" ""
      , typing = Double Dragon Steel
      , ability = Just SapSipper
      , imageUrl = imgUrlForAlternateForm "Sliggoo" "Hisuian"
      , evolutionData = EvolvesFrom [ 1133 ] "Level 40 in Legends: Arceus"
      , transformationData = DoesNotTransform
      }
    , { id = 1630
      , originalPokemonID = Just 1135
      , fullName = nameFromData "Goodra" "Hisuian" ""
      , typing = Double Dragon Steel
      , ability = Just SapSipper
      , imageUrl = imgUrlForAlternateForm "Goodra" "Hisuian"
      , evolutionData = EvolvesFrom [ 1629 ] "Level 50 When Raining or Foggy outside battle"
      , transformationData = DoesNotTransform
      }
    , { id = 1136
      , originalPokemonID = Nothing
      , fullName = "Klefki"
      , typing = Double Steel Fairy
      , ability = Nothing
      , imageUrl = imageUrlByName "Klefki"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1137
      , originalPokemonID = Nothing
      , fullName = "Phantump"
      , typing = Double Ghost Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Phantump"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1138
      , originalPokemonID = Nothing
      , fullName = "Trevenant"
      , typing = Double Ghost Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Trevenant"
      , evolutionData = EvolvesFrom [ 1137 ] "Trade"
      , transformationData = DoesNotTransform
      }
    , { id = 1140
      , originalPokemonID = Nothing
      , fullName = "Pumpkaboo"
      , typing = Double Ghost Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Pumpkaboo"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1141
      , originalPokemonID = Nothing
      , fullName = "Gourgeist"
      , typing = Double Ghost Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Gourgeist"
      , evolutionData = EvolvesFrom [ 1140 ] "Trade"
      , transformationData = DoesNotTransform
      }
    , { id = 1571
      , originalPokemonID = Just 1140
      , fullName = nameFromData "Pumpkaboo" "Small" ""
      , typing = Double Ghost Grass
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Pumpkaboo" "Small"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1574
      , originalPokemonID = Just 1141
      , fullName = nameFromData "Gourgeist" "Small" ""
      , typing = Double Ghost Grass
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Gourgeist" "Small"
      , evolutionData = EvolvesFrom [ 1571 ] "Trade"
      , transformationData = DoesNotTransform
      }
    , { id = 1572
      , originalPokemonID = Just 1140
      , fullName = nameFromData "Pumpkaboo" "Large" ""
      , typing = Double Ghost Grass
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Pumpkaboo" "Large"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1575
      , originalPokemonID = Just 1141
      , fullName = nameFromData "Gourgeist" "Large" ""
      , typing = Double Ghost Grass
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Gourgeist" "Large"
      , evolutionData = EvolvesFrom [ 1572 ] "Trade"
      , transformationData = DoesNotTransform
      }
    , { id = 1573
      , originalPokemonID = Just 1140
      , fullName = nameFromData "Pumpkaboo" "Super" ""
      , typing = Double Ghost Grass
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Pumpkaboo" "Super"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1576
      , originalPokemonID = Just 1141
      , fullName = nameFromData "Gourgeist" "Super" ""
      , typing = Double Ghost Grass
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Gourgeist" "Super"
      , evolutionData = EvolvesFrom [ 1573 ] "Trade"
      , transformationData = DoesNotTransform
      }
    , { id = 1142
      , originalPokemonID = Nothing
      , fullName = "Bergmite"
      , typing = Single Ice
      , ability = Nothing
      , imageUrl = imageUrlByName "Bergmite"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1143
      , originalPokemonID = Nothing
      , fullName = "Avalugg"
      , typing = Single Ice
      , ability = Nothing
      , imageUrl = imageUrlByName "Avalugg"
      , evolutionData = EvolvesFrom [ 1142 ] "Level 37"
      , transformationData = DoesNotTransform
      }
    , { id = 1631
      , originalPokemonID = Just 1143
      , fullName = nameFromData "Avalugg" "Hisuian" ""
      , typing = Double Ice Rock
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Avalugg" "Hisuian"
      , evolutionData = EvolvesFrom [ 1142 ] "Level 37 in Legends: Arceus"
      , transformationData = DoesNotTransform
      }
    , { id = 1144
      , originalPokemonID = Nothing
      , fullName = "Noibat"
      , typing = Double Flying Dragon
      , ability = Nothing
      , imageUrl = imageUrlByName "Noibat"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1145
      , originalPokemonID = Nothing
      , fullName = "Noivern"
      , typing = Double Flying Dragon
      , ability = Nothing
      , imageUrl = imageUrlByName "Noivern"
      , evolutionData = EvolvesFrom [ 1144 ] "Level 48"
      , transformationData = DoesNotTransform
      }
    , { id = 1146
      , originalPokemonID = Nothing
      , fullName = "Xerneas"
      , typing = Single Fairy
      , ability = Nothing
      , imageUrl = imageUrlByName "Xerneas"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1147
      , originalPokemonID = Nothing
      , fullName = "Yveltal"
      , typing = Double Dark Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Yveltal"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1577
      , originalPokemonID = Just 1148
      , fullName = nameFromData "Zygarde" "10%" ""
      , typing = Double Dragon Ground
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Zygarde" "10%"
      , evolutionData = EvolvesFrom [] "Collect 10% of Zygarde Cells"
      , transformationData = DoesNotTransform
      }
    , { id = 1148
      , originalPokemonID = Nothing
      , fullName = "Zygarde"
      , typing = Double Dragon Ground
      , ability = Nothing
      , imageUrl = imageUrlByName "Zygarde"
      , evolutionData = EvolvesFrom [ 1577 ] "Collect 50% of Zygarde Cells"
      , transformationData = Transforms 17 "At the end of battle"
      }
    , { id = 1578
      , originalPokemonID = Just 1148
      , fullName = nameFromData "Zygarde" "Complete" ""
      , typing = Double Dragon Ground
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Zygarde" "Complete"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 17 "If HP is below half"
      }
    , { id = 1149
      , originalPokemonID = Nothing
      , fullName = "Diancie"
      , typing = Double Rock Fairy
      , ability = Nothing
      , imageUrl = imageUrlByName "Diancie"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1594
      , originalPokemonID = Nothing
      , fullName = nameFromData "Diancie" "Mega" ""
      , typing = Double Rock Fairy
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Diancie" "Mega"
      , evolutionData = EvolvesFrom [ 1149 ] "Holding Diancite"
      , transformationData = DoesNotTransform
      }
    , { id = 1151
      , originalPokemonID = Nothing
      , fullName = "Hoopa"
      , typing = Double Psychic Ghost
      , ability = Nothing
      , imageUrl = imageUrlByName "Hoopa"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 18 "After 3 days"
      }
    , { id = 1579
      , originalPokemonID = Just 1151
      , fullName = nameFromData "Hoopa" "Unbound" ""
      , typing = Double Psychic Dark
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Hoopa" "Unbound"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 18 "Use Prison Bottle"
      }
    , { id = 1152
      , originalPokemonID = Nothing
      , fullName = "Volcanion"
      , typing = Double Fire Water
      , ability = Just WaterAbsorb
      , imageUrl = imageUrlByName "Volcanion"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1246
      , originalPokemonID = Nothing
      , fullName = "Rowlet"
      , typing = Double Grass Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Rowlet"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1247
      , originalPokemonID = Nothing
      , fullName = "Dartrix"
      , typing = Double Grass Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Dartrix"
      , evolutionData = EvolvesFrom [ 1246 ] "Level 17"
      , transformationData = DoesNotTransform
      }
    , { id = 1248
      , originalPokemonID = Nothing
      , fullName = "Decidueye"
      , typing = Double Grass Ghost
      , ability = Nothing
      , imageUrl = imageUrlByName "Decidueye"
      , evolutionData = EvolvesFrom [ 1247 ] "Level 34"
      , transformationData = DoesNotTransform
      }
    , { id = 1632
      , originalPokemonID = Just 1248
      , fullName = nameFromData "Decidueye" "Hisuian" ""
      , typing = Double Grass Fighting
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Decidueye" "Hisuian"
      , evolutionData = EvolvesFrom [ 1247 ] "Level 36 in Legends: Arceus"
      , transformationData = DoesNotTransform
      }
    , { id = 1249
      , originalPokemonID = Nothing
      , fullName = "Litten"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrlByName "Litten"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1250
      , originalPokemonID = Nothing
      , fullName = "Torracat"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrlByName "Torracat"
      , evolutionData = EvolvesFrom [ 1249 ] "Level 17"
      , transformationData = DoesNotTransform
      }
    , { id = 1251
      , originalPokemonID = Nothing
      , fullName = "Incineroar"
      , typing = Double Fire Dark
      , ability = Nothing
      , imageUrl = imageUrlByName "Incineroar"
      , evolutionData = EvolvesFrom [ 1250 ] "Level 34"
      , transformationData = DoesNotTransform
      }
    , { id = 1252
      , originalPokemonID = Nothing
      , fullName = "Popplio"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Popplio"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1253
      , originalPokemonID = Nothing
      , fullName = "Brionne"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Brionne"
      , evolutionData = EvolvesFrom [ 1252 ] "Level 17"
      , transformationData = DoesNotTransform
      }
    , { id = 1255
      , originalPokemonID = Nothing
      , fullName = "Primarina"
      , typing = Double Water Fairy
      , ability = Nothing
      , imageUrl = imageUrlByName "Primarina"
      , evolutionData = EvolvesFrom [ 1253 ] "Level 34"
      , transformationData = DoesNotTransform
      }
    , { id = 1256
      , originalPokemonID = Nothing
      , fullName = "Pikipek"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Pikipek"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1257
      , originalPokemonID = Nothing
      , fullName = "Trumbeak"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Trumbeak"
      , evolutionData = EvolvesFrom [ 1256 ] "Level 14"
      , transformationData = DoesNotTransform
      }
    , { id = 1258
      , originalPokemonID = Nothing
      , fullName = "Toucannon"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Toucannon"
      , evolutionData = EvolvesFrom [ 1257 ] "Level 28"
      , transformationData = DoesNotTransform
      }
    , { id = 1259
      , originalPokemonID = Nothing
      , fullName = "Yungoos"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Yungoos"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1260
      , originalPokemonID = Nothing
      , fullName = "Gumshoos"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Gumshoos"
      , evolutionData = EvolvesFrom [ 1259 ] "Level 20 During The Day"
      , transformationData = DoesNotTransform
      }
    , { id = 1261
      , originalPokemonID = Nothing
      , fullName = "Grubbin"
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imageUrlByName "Grubbin"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1262
      , originalPokemonID = Nothing
      , fullName = "Charjabug"
      , typing = Double Bug Electric
      , ability = Nothing
      , imageUrl = imageUrlByName "Charjabug"
      , evolutionData = EvolvesFrom [ 1261 ] "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 1263
      , originalPokemonID = Nothing
      , fullName = "Vikavolt"
      , typing = Double Bug Electric
      , ability = Just Levitate
      , imageUrl = imageUrlByName "Vikavolt"
      , evolutionData = EvolvesFrom [ 1262 ] "Use Thunder Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 1264
      , originalPokemonID = Nothing
      , fullName = "Crabrawler"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrlByName "Crabrawler"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1266
      , originalPokemonID = Nothing
      , fullName = "Crabominable"
      , typing = Double Fighting Ice
      , ability = Nothing
      , imageUrl = imageUrlByName "Crabominable"
      , evolutionData = EvolvesFrom [ 1264 ] "Level at Mount Lanakila in Alola"
      , transformationData = DoesNotTransform
      }
    , { id = 1267
      , originalPokemonID = Nothing
      , fullName = nameFromData "Oricorio" "Baile" ""
      , typing = Double Fire Flying
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Oricorio" "Baile"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 19 "Use Red Nectar"
      }
    , { id = 1582
      , originalPokemonID = Just 1267
      , fullName = nameFromData "Oricorio" "Pom-Pom" ""
      , typing = Double Electric Flying
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Oricorio" "Pom-Pom"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 19 "Use Yellow Nectar"
      }
    , { id = 1583
      , originalPokemonID = Just 1267
      , fullName = nameFromData "Oricorio" "Pa'u" ""
      , typing = Double Psychic Flying
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Oricorio" "Pa'u"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 19 "Use Pink Nectar"
      }
    , { id = 1584
      , originalPokemonID = Just 1267
      , fullName = nameFromData "Oricorio" "Sensu" ""
      , typing = Double Ghost Flying
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Oricorio" "Sensu"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 19 "Use Purple Nectar"
      }
    , { id = 1268
      , originalPokemonID = Nothing
      , fullName = "Cutiefly"
      , typing = Double Bug Fairy
      , ability = Nothing
      , imageUrl = imageUrlByName "Cutiefly"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1269
      , originalPokemonID = Nothing
      , fullName = "Ribombee"
      , typing = Double Bug Fairy
      , ability = Nothing
      , imageUrl = imageUrlByName "Ribombee"
      , evolutionData = EvolvesFrom [ 1268 ] "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 1270
      , originalPokemonID = Nothing
      , fullName = "Rockruff"
      , typing = Single Rock
      , ability = Nothing
      , imageUrl = imageUrlByName "Rockruff"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1271
      , originalPokemonID = Nothing
      , fullName = "Lycanroc"
      , typing = Single Rock
      , ability = Nothing
      , imageUrl = imageUrlByName "Lycanroc"
      , evolutionData = EvolvesFrom [ 1270 ] "Level 25 during the day"
      , transformationData = DoesNotTransform
      }
    , { id = 1403
      , originalPokemonID = Just 1271
      , fullName = nameFromData "Lycanroc" "Dusk" ""
      , typing = Single Rock
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Lycanroc" "Dusk"
      , evolutionData = EvolvesFrom [ 1270 ] "Level 25 with Own Tempo between 5-6pm"
      , transformationData = DoesNotTransform
      }
    , { id = 1402
      , originalPokemonID = Just 1271
      , fullName = nameFromData "Lycanroc" "Midnight" ""
      , typing = Single Rock
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Lycanroc" "Midnight"
      , evolutionData = EvolvesFrom [ 1270 ] "Level 25 at night"
      , transformationData = DoesNotTransform
      }
    , { id = 1272
      , originalPokemonID = Nothing
      , fullName = nameFromData "Wishiwashi" "Solo" "Solo Wishiwashi"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Wishiwashi" "solo"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 20 "If HP below 25%"
      }
    , { id = 1581
      , originalPokemonID = Just 1272
      , fullName = nameFromData "Wishiwashi" "School" "Wishiwashi School"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Wishiwashi" "School"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 20 "If HP above 25% and level 20+"
      }
    , { id = 1273
      , originalPokemonID = Nothing
      , fullName = "Mareanie"
      , typing = Double Poison Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Mareanie"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1274
      , originalPokemonID = Nothing
      , fullName = "Toxapex"
      , typing = Double Poison Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Toxapex"
      , evolutionData = EvolvesFrom [ 1273 ] "Level 38"
      , transformationData = DoesNotTransform
      }
    , { id = 1275
      , originalPokemonID = Nothing
      , fullName = "Mudbray"
      , typing = Single Ground
      , ability = Nothing
      , imageUrl = imageUrlByName "Mudbray"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1277
      , originalPokemonID = Nothing
      , fullName = "Mudsdale"
      , typing = Single Ground
      , ability = Nothing
      , imageUrl = imageUrlByName "Mudsdale"
      , evolutionData = EvolvesFrom [ 1275 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 1278
      , originalPokemonID = Nothing
      , fullName = "Dewpider"
      , typing = Double Water Bug
      , ability = Just WaterBubble
      , imageUrl = imageUrlByName "Dewpider"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1279
      , originalPokemonID = Nothing
      , fullName = "Araquanid"
      , typing = Double Water Bug
      , ability = Just WaterBubble
      , imageUrl = imageUrlByName "Araquanid"
      , evolutionData = EvolvesFrom [ 1278 ] "Level 22"
      , transformationData = DoesNotTransform
      }
    , { id = 1280
      , originalPokemonID = Nothing
      , fullName = "Fomantis"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Fomantis"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1281
      , originalPokemonID = Nothing
      , fullName = "Lurantis"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Lurantis"
      , evolutionData = EvolvesFrom [ 1280 ] "Level 34 During The Day"
      , transformationData = DoesNotTransform
      }
    , { id = 1282
      , originalPokemonID = Nothing
      , fullName = "Morelull"
      , typing = Double Grass Fairy
      , ability = Nothing
      , imageUrl = imageUrlByName "Morelull"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1283
      , originalPokemonID = Nothing
      , fullName = "Shiinotic"
      , typing = Double Grass Fairy
      , ability = Nothing
      , imageUrl = imageUrlByName "Shiinotic"
      , evolutionData = EvolvesFrom [ 1282 ] "Level 24"
      , transformationData = DoesNotTransform
      }
    , { id = 1284
      , originalPokemonID = Nothing
      , fullName = "Salandit"
      , typing = Double Poison Fire
      , ability = Nothing
      , imageUrl = imageUrlByName "Salandit"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1285
      , originalPokemonID = Nothing
      , fullName = "Salazzle"
      , typing = Double Poison Fire
      , ability = Nothing
      , imageUrl = imageUrlByName "Salazzle"
      , evolutionData = EvolvesFrom [ 1284 ] "Level 33 When Female"
      , transformationData = DoesNotTransform
      }
    , { id = 1286
      , originalPokemonID = Nothing
      , fullName = "Stufful"
      , typing = Double Normal Fighting
      , ability = Nothing
      , imageUrl = imageUrlByName "Stufful"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1288
      , originalPokemonID = Nothing
      , fullName = "Bewear"
      , typing = Double Normal Fighting
      , ability = Nothing
      , imageUrl = imageUrlByName "Bewear"
      , evolutionData = EvolvesFrom [ 1286 ] "Level 27"
      , transformationData = DoesNotTransform
      }
    , { id = 1289
      , originalPokemonID = Nothing
      , fullName = "Bounsweet"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Bounsweet"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1290
      , originalPokemonID = Nothing
      , fullName = "Steenee"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Steenee"
      , evolutionData = EvolvesFrom [ 1289 ] "Level 18"
      , transformationData = DoesNotTransform
      }
    , { id = 1291
      , originalPokemonID = Nothing
      , fullName = "Tsareena"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Tsareena"
      , evolutionData = EvolvesFrom [ 1290 ] "Level while knowing Stomp"
      , transformationData = DoesNotTransform
      }
    , { id = 1292
      , originalPokemonID = Nothing
      , fullName = "Comfey"
      , typing = Single Fairy
      , ability = Nothing
      , imageUrl = imageUrlByName "Comfey"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 743
      , originalPokemonID = Nothing
      , fullName = "Oranguru"
      , typing = Double Normal Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Oranguru"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1293
      , originalPokemonID = Nothing
      , fullName = "Passimian"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrlByName "Passimian"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1294
      , originalPokemonID = Nothing
      , fullName = "Wimpod"
      , typing = Double Bug Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Wimpod"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1295
      , originalPokemonID = Nothing
      , fullName = "Golisopod"
      , typing = Double Bug Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Golisopod"
      , evolutionData = EvolvesFrom [ 1294 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 1296
      , originalPokemonID = Nothing
      , fullName = "Sandygast"
      , typing = Double Ghost Ground
      , ability = Nothing
      , imageUrl = imageUrlByName "Sandygast"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1298
      , originalPokemonID = Nothing
      , fullName = "Palossand"
      , typing = Double Ghost Ground
      , ability = Nothing
      , imageUrl = imageUrlByName "Palossand"
      , evolutionData = EvolvesFrom [ 1296 ] "Level 42"
      , transformationData = DoesNotTransform
      }
    , { id = 1299
      , originalPokemonID = Nothing
      , fullName = "Pyukumuku"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Pyukumuku"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1300
      , originalPokemonID = Nothing
      , fullName = "Type: Null"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Type: Null"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1301
      , originalPokemonID = Nothing
      , fullName = "Silvally"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Silvally"
      , evolutionData = EvolvesFrom [ 1300 ] "Level with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 1302
      , originalPokemonID = Nothing
      , fullName = "Minior"
      , typing = Double Rock Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Minior"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 21 "If HP is above half"
      }
    , { id = 1564
      , originalPokemonID = Just 1302
      , fullName = nameFromData "Minior" "Core" "Minior Core"
      , typing = Double Rock Flying
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Minior" "Core"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 21 "If HP is below half"
      }
    , { id = 1303
      , originalPokemonID = Nothing
      , fullName = "Komala"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Komala"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1304
      , originalPokemonID = Nothing
      , fullName = "Turtonator"
      , typing = Double Fire Dragon
      , ability = Nothing
      , imageUrl = imageUrlByName "Turtonator"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1305
      , originalPokemonID = Nothing
      , fullName = "Togedemaru"
      , typing = Double Electric Steel
      , ability = Just LightningRod
      , imageUrl = imageUrlByName "Togedemaru"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1306
      , originalPokemonID = Nothing
      , fullName = "Mimikyu"
      , typing = Double Ghost Fairy
      , ability = Nothing
      , imageUrl = imageUrlByName "Mimikyu"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1307
      , originalPokemonID = Nothing
      , fullName = "Bruxish"
      , typing = Double Water Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Bruxish"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1309
      , originalPokemonID = Nothing
      , fullName = "Drampa"
      , typing = Double Normal Dragon
      , ability = Just SapSipper
      , imageUrl = imageUrlByName "Drampa"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1310
      , originalPokemonID = Nothing
      , fullName = "Dhelmise"
      , typing = Double Ghost Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Dhelmise"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1311
      , originalPokemonID = Nothing
      , fullName = "Jangmo-o"
      , typing = Single Dragon
      , ability = Nothing
      , imageUrl = imageUrlByName "Jangmo-o"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1312
      , originalPokemonID = Nothing
      , fullName = "Hakamo-o"
      , typing = Double Dragon Fighting
      , ability = Nothing
      , imageUrl = imageUrlByName "Hakamo-o"
      , evolutionData = EvolvesFrom [ 1311 ] "Level 35"
      , transformationData = DoesNotTransform
      }
    , { id = 1313
      , originalPokemonID = Nothing
      , fullName = "Kommo-o"
      , typing = Double Dragon Fighting
      , ability = Nothing
      , imageUrl = imageUrlByName "Kommo-o"
      , evolutionData = EvolvesFrom [ 1312 ] "Level 45"
      , transformationData = DoesNotTransform
      }
    , { id = 1314
      , originalPokemonID = Nothing
      , fullName = "Tapu Koko"
      , typing = Double Electric Fairy
      , ability = Nothing
      , imageUrl = imageUrlByName "Tapu Koko"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1315
      , originalPokemonID = Nothing
      , fullName = "Tapu Lele"
      , typing = Double Psychic Fairy
      , ability = Nothing
      , imageUrl = imageUrlByName "Tapu Lele"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1316
      , originalPokemonID = Nothing
      , fullName = "Tapu Bulu"
      , typing = Double Grass Fairy
      , ability = Nothing
      , imageUrl = imageUrlByName "Tapu Bulu"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1317
      , originalPokemonID = Nothing
      , fullName = "Tapu Fini"
      , typing = Double Water Fairy
      , ability = Nothing
      , imageUrl = imageUrlByName "Tapu Fini"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1318
      , originalPokemonID = Nothing
      , fullName = "Cosmog"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Cosmog"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1320
      , originalPokemonID = Nothing
      , fullName = "Cosmoem"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Cosmoem"
      , evolutionData = EvolvesFrom [ 1318 ] "Level 43"
      , transformationData = DoesNotTransform
      }
    , { id = 1321
      , originalPokemonID = Nothing
      , fullName = "Solgaleo"
      , typing = Double Psychic Steel
      , ability = Nothing
      , imageUrl = imageUrlByName "Solgaleo"
      , evolutionData = EvolvesFrom [ 1320 ] "Level 53 In Sun Or Ultra Sun"
      , transformationData = DoesNotTransform
      }
    , { id = 1322
      , originalPokemonID = Nothing
      , fullName = "Lunala"
      , typing = Double Psychic Ghost
      , ability = Nothing
      , imageUrl = imageUrlByName "Lunala"
      , evolutionData = EvolvesFrom [ 1320 ] "Level 53 In Moon Or Ultra Moon"
      , transformationData = DoesNotTransform
      }
    , { id = 1323
      , originalPokemonID = Nothing
      , fullName = "Nihilego"
      , typing = Double Rock Poison
      , ability = Nothing
      , imageUrl = imageUrlByName "Nihilego"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1324
      , originalPokemonID = Nothing
      , fullName = "Buzzwole"
      , typing = Double Bug Fighting
      , ability = Nothing
      , imageUrl = imageUrlByName "Buzzwole"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1325
      , originalPokemonID = Nothing
      , fullName = "Pheromosa"
      , typing = Double Bug Fighting
      , ability = Nothing
      , imageUrl = imageUrlByName "Pheromosa"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1326
      , originalPokemonID = Nothing
      , fullName = "Xurkitree"
      , typing = Single Electric
      , ability = Nothing
      , imageUrl = imageUrlByName "Xurkitree"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1327
      , originalPokemonID = Nothing
      , fullName = "Celesteela"
      , typing = Double Steel Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Celesteela"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1328
      , originalPokemonID = Nothing
      , fullName = "Kartana"
      , typing = Double Grass Steel
      , ability = Nothing
      , imageUrl = imageUrlByName "Kartana"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1329
      , originalPokemonID = Nothing
      , fullName = "Guzzlord"
      , typing = Double Dark Dragon
      , ability = Nothing
      , imageUrl = imageUrlByName "Guzzlord"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1331
      , originalPokemonID = Nothing
      , fullName = "Necrozma"
      , typing = Single Psychic
      , ability = Just PrismArmor
      , imageUrl = imageUrlByName "Necrozma"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 22 "Undo fusion"
      }
    , { id = 1404
      , originalPokemonID = Just 1331
      , fullName = nameFromData "Necrozma" "Dusk Mane" ""
      , typing = Double Psychic Steel
      , ability = Just PrismArmor
      , imageUrl = imgUrlForAlternateForm "Necrozma" "Dusk Mane"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 22 "Fuze with Solgaleo"
      }
    , { id = 1405
      , originalPokemonID = Just 1331
      , fullName = nameFromData "Necrozma" "Dawn Wings" ""
      , typing = Double Psychic Ghost
      , ability = Just PrismArmor
      , imageUrl = imgUrlForAlternateForm "Necrozma" "Dawn Wings"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 22 "Fuze with Lunala"
      }
    , { id = 1406
      , originalPokemonID = Just 1331
      , fullName = nameFromData "Necrozma" "Ultra" ""
      , typing = Double Psychic Dragon
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Necrozma" "Ultra"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 22 "Use Ultra Burst"
      }
    , { id = 1332
      , originalPokemonID = Nothing
      , fullName = "Magearna"
      , typing = Double Steel Fairy
      , ability = Nothing
      , imageUrl = imageUrlByName "Magearna"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1333
      , originalPokemonID = Nothing
      , fullName = "Marshadow"
      , typing = Double Fighting Ghost
      , ability = Nothing
      , imageUrl = imageUrlByName "Marshadow"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1334
      , originalPokemonID = Nothing
      , fullName = "Poipole"
      , typing = Single Poison
      , ability = Nothing
      , imageUrl = imageUrlByName "Poipole"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1335
      , originalPokemonID = Nothing
      , fullName = "Naganadel"
      , typing = Double Poison Dragon
      , ability = Nothing
      , imageUrl = imageUrlByName "Naganadel"
      , evolutionData = EvolvesFrom [ 1334 ] "Level while knowing Dragon Pulse"
      , transformationData = DoesNotTransform
      }
    , { id = 1336
      , originalPokemonID = Nothing
      , fullName = "Stakataka"
      , typing = Double Rock Steel
      , ability = Nothing
      , imageUrl = imageUrlByName "Stakataka"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1337
      , originalPokemonID = Nothing
      , fullName = "Blacephalon"
      , typing = Double Fire Ghost
      , ability = Nothing
      , imageUrl = imageUrlByName "Blacephalon"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1338
      , originalPokemonID = Nothing
      , fullName = "Zeraora"
      , typing = Single Electric
      , ability = Just VoltAbsorb
      , imageUrl = imageUrlByName "Zeraora"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1339
      , originalPokemonID = Nothing
      , fullName = "Meltan"
      , typing = Single Steel
      , ability = Nothing
      , imageUrl = imageUrlByName "Meltan"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1340
      , originalPokemonID = Nothing
      , fullName = "Melmetal"
      , typing = Single Steel
      , ability = Nothing
      , imageUrl = imageUrlByName "Melmetal"
      , evolutionData = EvolvesFrom [ 1339 ] "400 Meltan Candy (Pokemon GO only)"
      , transformationData = DoesNotTransform
      }
    , { id = 1439
      , originalPokemonID = Nothing
      , fullName = "Grookey"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Grookey"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1440
      , originalPokemonID = Nothing
      , fullName = "Thwackey"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Thwackey"
      , evolutionData = EvolvesFrom [ 1439 ] "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 1441
      , originalPokemonID = Nothing
      , fullName = "Rillaboom"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Rillaboom"
      , evolutionData = EvolvesFrom [ 1440 ] "Level 35"
      , transformationData = DoesNotTransform
      }
    , { id = 1442
      , originalPokemonID = Nothing
      , fullName = "Scorbunny"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrlByName "Scorbunny"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1443
      , originalPokemonID = Nothing
      , fullName = "Raboot"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrlByName "Raboot"
      , evolutionData = EvolvesFrom [ 1442 ] "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 1444
      , originalPokemonID = Nothing
      , fullName = "Cinderace"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrlByName "Cinderace"
      , evolutionData = EvolvesFrom [ 1443 ] "Level 35"
      , transformationData = DoesNotTransform
      }
    , { id = 1445
      , originalPokemonID = Nothing
      , fullName = "Sobble"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Sobble"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1446
      , originalPokemonID = Nothing
      , fullName = "Drizzile"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Drizzile"
      , evolutionData = EvolvesFrom [ 1445 ] "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 1447
      , originalPokemonID = Nothing
      , fullName = "Inteleon"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Inteleon"
      , evolutionData = EvolvesFrom [ 1446 ] "Level 35"
      , transformationData = DoesNotTransform
      }
    , { id = 1462
      , originalPokemonID = Nothing
      , fullName = "Skwovet"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Skwovet"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1463
      , originalPokemonID = Nothing
      , fullName = "Greedent"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Greedent"
      , evolutionData = EvolvesFrom [ 1462 ] "Level 24"
      , transformationData = DoesNotTransform
      }
    , { id = 1469
      , originalPokemonID = Nothing
      , fullName = "Rookidee"
      , typing = Single Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Rookidee"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1470
      , originalPokemonID = Nothing
      , fullName = "Corvisquire"
      , typing = Single Flying
      , ability = Nothing
      , imageUrl = imageUrlByName "Corvisquire"
      , evolutionData = EvolvesFrom [ 1469 ] "Level 18"
      , transformationData = DoesNotTransform
      }
    , { id = 1471
      , originalPokemonID = Nothing
      , fullName = "Corviknight"
      , typing = Double Flying Steel
      , ability = Nothing
      , imageUrl = imageUrlByName "Corviknight"
      , evolutionData = EvolvesFrom [ 1470 ] "Level 38"
      , transformationData = DoesNotTransform
      }
    , { id = 1485
      , originalPokemonID = Nothing
      , fullName = "Blipbug"
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imageUrlByName "Blipbug"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1486
      , originalPokemonID = Nothing
      , fullName = "Dottler"
      , typing = Double Bug Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Dottler"
      , evolutionData = EvolvesFrom [ 1485 ] "Level 10"
      , transformationData = DoesNotTransform
      }
    , { id = 1487
      , originalPokemonID = Nothing
      , fullName = "Orbeetle"
      , typing = Double Bug Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Orbeetle"
      , evolutionData = EvolvesFrom [ 1486 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 1488
      , originalPokemonID = Nothing
      , fullName = "Nickit"
      , typing = Single Dark
      , ability = Nothing
      , imageUrl = imageUrlByName "Nickit"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1489
      , originalPokemonID = Nothing
      , fullName = "Thievul"
      , typing = Single Dark
      , ability = Nothing
      , imageUrl = imageUrlByName "Thievul"
      , evolutionData = EvolvesFrom [ 1488 ] "Level 18"
      , transformationData = DoesNotTransform
      }
    , { id = 1490
      , originalPokemonID = Nothing
      , fullName = "Gossifleur"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Gossifleur"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1491
      , originalPokemonID = Nothing
      , fullName = "Eldegoss"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Eldegoss"
      , evolutionData = EvolvesFrom [ 1490 ] "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 1492
      , originalPokemonID = Nothing
      , fullName = "Wooloo"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Wooloo"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1493
      , originalPokemonID = Nothing
      , fullName = "Dubwool"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Dubwool"
      , evolutionData = EvolvesFrom [ 1492 ] "Level 24"
      , transformationData = DoesNotTransform
      }
    , { id = 1494
      , originalPokemonID = Nothing
      , fullName = "Chewtle"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Chewtle"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1495
      , originalPokemonID = Nothing
      , fullName = "Drednaw"
      , typing = Double Water Rock
      , ability = Nothing
      , imageUrl = imageUrlByName "Drednaw"
      , evolutionData = EvolvesFrom [ 1494 ] "Level 22"
      , transformationData = DoesNotTransform
      }
    , { id = 1496
      , originalPokemonID = Nothing
      , fullName = "Yamper"
      , typing = Single Electric
      , ability = Nothing
      , imageUrl = imageUrlByName "Yamper"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1497
      , originalPokemonID = Nothing
      , fullName = "Boltund"
      , typing = Single Electric
      , ability = Nothing
      , imageUrl = imageUrlByName "Boltund"
      , evolutionData = EvolvesFrom [ 1496 ] "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 1498
      , originalPokemonID = Nothing
      , fullName = "Rolycoly"
      , typing = Single Rock
      , ability = Nothing
      , imageUrl = imageUrlByName "Rolycoly"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1499
      , originalPokemonID = Nothing
      , fullName = "Carkol"
      , typing = Double Rock Fire
      , ability = Nothing
      , imageUrl = imageUrlByName "Carkol"
      , evolutionData = EvolvesFrom [ 1498 ] "Level 18"
      , transformationData = DoesNotTransform
      }
    , { id = 1500
      , originalPokemonID = Nothing
      , fullName = "Coalossal"
      , typing = Double Rock Fire
      , ability = Nothing
      , imageUrl = imageUrlByName "Coalossal"
      , evolutionData = EvolvesFrom [ 1499 ] "Level 34"
      , transformationData = DoesNotTransform
      }
    , { id = 1501
      , originalPokemonID = Nothing
      , fullName = "Applin"
      , typing = Double Grass Dragon
      , ability = Nothing
      , imageUrl = imageUrlByName "Applin"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1502
      , originalPokemonID = Nothing
      , fullName = "Flapple"
      , typing = Double Grass Dragon
      , ability = Nothing
      , imageUrl = imageUrlByName "Flapple"
      , evolutionData = EvolvesFrom [ 1501 ] "Use Tart Apple"
      , transformationData = DoesNotTransform
      }
    , { id = 1503
      , originalPokemonID = Nothing
      , fullName = "Appletun"
      , typing = Double Grass Dragon
      , ability = Nothing
      , imageUrl = imageUrlByName "Appletun"
      , evolutionData = EvolvesFrom [ 1501 ] "Use Sweet Apple"
      , transformationData = DoesNotTransform
      }
    , { id = 1504
      , originalPokemonID = Nothing
      , fullName = "Silicobra"
      , typing = Single Ground
      , ability = Nothing
      , imageUrl = imageUrlByName "Silicobra"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1505
      , originalPokemonID = Nothing
      , fullName = "Sandaconda"
      , typing = Single Ground
      , ability = Nothing
      , imageUrl = imageUrlByName "Sandaconda"
      , evolutionData = EvolvesFrom [ 1504 ] "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 1506
      , originalPokemonID = Nothing
      , fullName = "Cramorant"
      , typing = Double Flying Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Cramorant"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1507
      , originalPokemonID = Nothing
      , fullName = "Arrokuda"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Arrokuda"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1508
      , originalPokemonID = Nothing
      , fullName = "Barraskewda"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByName "Barraskewda"
      , evolutionData = EvolvesFrom [ 1507 ] "Level 26"
      , transformationData = DoesNotTransform
      }
    , { id = 14
      , originalPokemonID = Nothing
      , fullName = "Toxel"
      , typing = Double Electric Poison
      , ability = Nothing
      , imageUrl = imageUrlByName "Toxel"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 15
      , originalPokemonID = Nothing
      , fullName = "Toxtricity"
      , typing = Double Electric Poison
      , ability = Nothing
      , imageUrl = imageUrlByName "Toxtricity"
      , evolutionData = EvolvesFrom [ 14 ] "Level 30 With An Adamant, Brave, Docile, Hardy, Hasty, Impish, Jolly, Lax, Naive, Naughty, Quirky, Rash, or Sassy Nature"
      , transformationData = DoesNotTransform
      }
    , { id = 16
      , originalPokemonID = Nothing
      , fullName = "Toxtricity"
      , typing = Double Electric Poison
      , ability = Nothing
      , imageUrl = imageUrlByName "Toxtricity"
      , evolutionData = EvolvesFrom [ 14 ] "Level 30 With A Bashful, Bold, Calm, Careful, Gentle, Lonely, Mild, Modest, Quiet, Relaxed, Serious, or Timid Nature"
      , transformationData = DoesNotTransform
      }
    , { id = 1509
      , originalPokemonID = Nothing
      , fullName = "Sizzlipede"
      , typing = Double Fire Bug
      , ability = Just FlashFire
      , imageUrl = imageUrlByName "Sizzlipede"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1510
      , originalPokemonID = Nothing
      , fullName = "Centiskorch"
      , typing = Double Fire Bug
      , ability = Just FlashFire
      , imageUrl = imageUrlByName "Centiskorch"
      , evolutionData = EvolvesFrom [ 1509 ] "Level 28"
      , transformationData = DoesNotTransform
      }
    , { id = 1511
      , originalPokemonID = Nothing
      , fullName = "Clobbopus"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrlByName "Clobbopus"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1512
      , originalPokemonID = Nothing
      , fullName = "Grapploct"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrlByName "Grapploct"
      , evolutionData = EvolvesFrom [ 1511 ] "Level while knowing Taunt"
      , transformationData = DoesNotTransform
      }
    , { id = 1513
      , originalPokemonID = Nothing
      , fullName = "Sinistea"
      , typing = Single Ghost
      , ability = Nothing
      , imageUrl = imageUrlByName "Sinistea"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1514
      , originalPokemonID = Nothing
      , fullName = "Polteageist"
      , typing = Single Ghost
      , ability = Nothing
      , imageUrl = imageUrlByName "Polteageist"
      , evolutionData = EvolvesFrom [ 1513 ] "Use Cracked Pot when Phony or Chipped Pot when Authentic"
      , transformationData = DoesNotTransform
      }
    , { id = 1515
      , originalPokemonID = Nothing
      , fullName = "Hatenna"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Hatenna"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1516
      , originalPokemonID = Nothing
      , fullName = "Hattrem"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrlByName "Hattrem"
      , evolutionData = EvolvesFrom [ 1515 ] "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 1517
      , originalPokemonID = Nothing
      , fullName = "Hatterene"
      , typing = Double Psychic Fairy
      , ability = Nothing
      , imageUrl = imageUrlByName "Hatterene"
      , evolutionData = EvolvesFrom [ 1516 ] "Level 42"
      , transformationData = DoesNotTransform
      }
    , { id = 1518
      , originalPokemonID = Nothing
      , fullName = "Impidimp"
      , typing = Double Dark Fairy
      , ability = Nothing
      , imageUrl = imageUrlByName "Impidimp"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1519
      , originalPokemonID = Nothing
      , fullName = "Morgrem"
      , typing = Double Dark Fairy
      , ability = Nothing
      , imageUrl = imageUrlByName "Morgrem"
      , evolutionData = EvolvesFrom [ 1518 ] "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 1520
      , originalPokemonID = Nothing
      , fullName = "Grimmsnarl"
      , typing = Double Dark Fairy
      , ability = Nothing
      , imageUrl = imageUrlByName "Grimmsnarl"
      , evolutionData = EvolvesFrom [ 1519 ] "Level 42"
      , transformationData = DoesNotTransform
      }
    , { id = 1483
      , originalPokemonID = Nothing
      , fullName = "Milcery"
      , typing = Single Fairy
      , ability = Nothing
      , imageUrl = imageUrlByName "Milcery"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1484
      , originalPokemonID = Nothing
      , fullName = "Alcremie"
      , typing = Single Fairy
      , ability = Nothing
      , imageUrl = imageUrlByName "Alcremie"
      , evolutionData = EvolvesFrom [ 1483 ] "While holding a Sweet and its Trainer spins"
      , transformationData = DoesNotTransform
      }
    , { id = 1378
      , originalPokemonID = Nothing
      , fullName = "Falinks"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrlByName "Falinks"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1425
      , originalPokemonID = Nothing
      , fullName = "Pincurchin"
      , typing = Single Electric
      , ability = Just LightningRod
      , imageUrl = imageUrlByName "Pincurchin"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1481
      , originalPokemonID = Nothing
      , fullName = "Snom"
      , typing = Double Ice Bug
      , ability = Nothing
      , imageUrl = imageUrlByName "Snom"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1482
      , originalPokemonID = Nothing
      , fullName = "Frosmoth"
      , typing = Double Ice Bug
      , ability = Nothing
      , imageUrl = imageUrlByName "Frosmoth"
      , evolutionData = EvolvesFrom [ 1481 ] "Level during the night with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 1412
      , originalPokemonID = Nothing
      , fullName = "Stonjourner"
      , typing = Single Rock
      , ability = Nothing
      , imageUrl = imageUrlByName "Stonjourner"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1479
      , originalPokemonID = Nothing
      , fullName = "Eiscue"
      , typing = Single Ice
      , ability = Nothing
      , imageUrl = imageUrlByName "Eiscue"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 23 "When a hailstorm starts"
      }
    , { id = 1480
      , originalPokemonID = Just 1479
      , fullName = nameFromData "Eiscue" "Noice" ""
      , typing = Single Ice
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Eiscue" "Noice"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 23 "When hit by a physical move"
      }
    , { id = 1477
      , originalPokemonID = Nothing
      , fullName = "Indeedee"
      , typing = Double Psychic Normal
      , ability = Nothing
      , imageUrl = imageUrlByName "Indeedee"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1478
      , originalPokemonID = Just 1477
      , fullName = nameFromData "Indeedee" "Female" ""
      , typing = Double Psychic Normal
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Indeedee" "Female"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1426
      , originalPokemonID = Nothing
      , fullName = "Morpeko"
      , typing = Double Electric Dark
      , ability = Nothing
      , imageUrl = imageUrlByName "Morpeko"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1475
      , originalPokemonID = Nothing
      , fullName = "Cufant"
      , typing = Single Steel
      , ability = Nothing
      , imageUrl = imageUrlByName "Cufant"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1476
      , originalPokemonID = Nothing
      , fullName = "Copperajah"
      , typing = Single Steel
      , ability = Nothing
      , imageUrl = imageUrlByName "Copperajah"
      , evolutionData = EvolvesFrom [ 1475 ] "Level 34"
      , transformationData = DoesNotTransform
      }
    , { id = 1418
      , originalPokemonID = Nothing
      , fullName = "Dracozolt"
      , typing = Double Electric Dragon
      , ability = Just VoltAbsorb
      , imageUrl = imageUrlByName "Dracozolt"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1420
      , originalPokemonID = Nothing
      , fullName = "Arctozolt"
      , typing = Double Electric Ice
      , ability = Just VoltAbsorb
      , imageUrl = imageUrlByName "Arctozolt"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1419
      , originalPokemonID = Nothing
      , fullName = "Dracovish"
      , typing = Double Water Dragon
      , ability = Just WaterAbsorb
      , imageUrl = imageUrlByName "Dracovish"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1422
      , originalPokemonID = Nothing
      , fullName = "Arctovish"
      , typing = Double Water Ice
      , ability = Just WaterAbsorb
      , imageUrl = imageUrlByName "Arctovish"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1424
      , originalPokemonID = Nothing
      , fullName = "Duraludon"
      , typing = Double Steel Dragon
      , ability = Nothing
      , imageUrl = imageUrlByName "Duraludon"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1472
      , originalPokemonID = Nothing
      , fullName = "Dreepy"
      , typing = Double Dragon Ghost
      , ability = Nothing
      , imageUrl = imageUrlByName "Dreepy"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1473
      , originalPokemonID = Nothing
      , fullName = "Drakloak"
      , typing = Double Dragon Ghost
      , ability = Nothing
      , imageUrl = imageUrlByName "Drakloak"
      , evolutionData = EvolvesFrom [ 1472 ] "Level 50"
      , transformationData = DoesNotTransform
      }
    , { id = 1474
      , originalPokemonID = Nothing
      , fullName = "Dragapult"
      , typing = Double Dragon Ghost
      , ability = Nothing
      , imageUrl = imageUrlByName "Dragapult"
      , evolutionData = EvolvesFrom [ 1473 ] "Level 60"
      , transformationData = DoesNotTransform
      }
    , { id = 1448
      , originalPokemonID = Nothing
      , fullName = "Zacian"
      , typing = Single Fairy
      , ability = Nothing
      , imageUrl = imageUrlByName "Zacian"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 24 "If not holding Rusted Sword"
      }
    , { id = 1449
      , originalPokemonID = Just 1448
      , fullName = nameFromData "Zacian" "Crowned" ""
      , typing = Double Fairy Steel
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Zacian" "Crowned"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 24 "While holding Rusted Sword"
      }
    , { id = 1450
      , originalPokemonID = Nothing
      , fullName = "Zamazenta"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrlByName "Zamazenta"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 25 "If not holding Rusted Shield"
      }
    , { id = 1451
      , originalPokemonID = Just 1450
      , fullName = nameFromData "Zamazenta" "Crowned" ""
      , typing = Double Fighting Steel
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Zamazenta" "Crowned"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 25 "While holding Rusted Shield"
      }
    , { id = 1452
      , originalPokemonID = Nothing
      , fullName = "Eternatus"
      , typing = Double Poison Dragon
      , ability = Nothing
      , imageUrl = imageUrlByName "Eternatus"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 26 "When owned by the player"
      }
    , { id = 1461
      , originalPokemonID = Just 1452
      , fullName = nameFromData "Eternatus" "Eternamax" ""
      , typing = Double Poison Dragon
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Eternatus" "Eternamax"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 26 "During the final battle in Pokémon Sword and Shield"
      }
    , { id = 38
      , originalPokemonID = Nothing
      , fullName = "Kubfu"
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrlByName "Kubfu"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 39
      , originalPokemonID = Nothing
      , fullName = "Urshifu"
      , typing = Double Fighting Dark
      , ability = Nothing
      , imageUrl = imageUrlByName "Urshifu"
      , evolutionData = EvolvesFrom [ 38 ] "Conquer the Tower of Darkness in Galar's Isle of Armor"
      , transformationData = DoesNotTransform
      }
    , { id = 40
      , originalPokemonID = Just 39
      , fullName = nameFromData "Urshifu" "Rapid Strike" ""
      , typing = Double Fighting Water
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Urshifu" "Rapid Strike"
      , evolutionData = EvolvesFrom [ 38 ] "Conquer the Tower of Waters in Galar's Isle of Armor"
      , transformationData = DoesNotTransform
      }
    , { id = 1597
      , originalPokemonID = Nothing
      , fullName = "Zarude"
      , typing = Double Dark Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Zarude"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1598
      , originalPokemonID = Nothing
      , fullName = "Regieleki"
      , typing = Single Electric
      , ability = Nothing
      , imageUrl = imageUrlByName "Regieleki"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1599
      , originalPokemonID = Nothing
      , fullName = "Regidrago"
      , typing = Single Dragon
      , ability = Nothing
      , imageUrl = imageUrlByName "Regidrago"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1602
      , originalPokemonID = Nothing
      , fullName = "Glastrier"
      , typing = Single Ice
      , ability = Nothing
      , imageUrl = imageUrlByName "Glastrier"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1603
      , originalPokemonID = Nothing
      , fullName = "Spectrier"
      , typing = Single Ghost
      , ability = Nothing
      , imageUrl = imageUrlByName "Spectrier"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 41
      , originalPokemonID = Nothing
      , fullName = "Calyrex"
      , typing = Double Psychic Grass
      , ability = Nothing
      , imageUrl = imageUrlByName "Calyrex"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 27 "Undo fusion"
      }
    , { id = 1604
      , originalPokemonID = Just 41
      , fullName = nameFromData "Calyrex" "Ice Rider" ""
      , typing = Double Psychic Ice
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Calyrex" "Ice Rider"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 27 "Fuse with Glastrier"
      }
    , { id = 1605
      , originalPokemonID = Just 41
      , fullName = nameFromData "Calyrex" "Shadow Rider" ""
      , typing = Double Psychic Ghost
      , ability = Nothing
      , imageUrl = imgUrlForAlternateForm "Calyrex" "Shadow Rider"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 27 "Fuse with Spectrier"
      }
    , { id = 1617
      , originalPokemonID = Nothing
      , fullName = "Enamorus"
      , typing = Double Fairy Flying
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "legends-arceus" "enamorus-incarnate"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 28 "Use the Reveal Glass"
      }
    , { id = 1618
      , originalPokemonID = Just 1617
      , fullName = nameFromData "Enamorus" "Therian" ""
      , typing = Double Fairy Flying
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "legends-arceus" "enamorus-therian"
      , evolutionData = DoesNotEvolve
      , transformationData = Transforms 28 "Use the Reveal Glass"
      }
    , { id = 1633
      , originalPokemonID = Nothing
      , fullName = "Sprigatito"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "sprigatito"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1634
      , originalPokemonID = Nothing
      , fullName = "Floragato"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "floragato"
      , evolutionData = EvolvesFrom [ 1633 ] "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 1635
      , originalPokemonID = Nothing
      , fullName = "Meowscarada"
      , typing = Double Grass Dark
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "meowscarada"
      , evolutionData = EvolvesFrom [ 1634 ] "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 1636
      , originalPokemonID = Nothing
      , fullName = "Fuecoco"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "fuecoco"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1637
      , originalPokemonID = Nothing
      , fullName = "Crocalor"
      , typing = Single Fire
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "crocalor"
      , evolutionData = EvolvesFrom [ 1636 ] "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 1638
      , originalPokemonID = Nothing
      , fullName = "Skeledirge"
      , typing = Double Fire Ghost
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "skeledirge"
      , evolutionData = EvolvesFrom [ 1637 ] "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 1639
      , originalPokemonID = Nothing
      , fullName = "Quaxly"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "quaxly"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1640
      , originalPokemonID = Nothing
      , fullName = "Quaxwell"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "quaxwell"
      , evolutionData = EvolvesFrom [ 1639 ] "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 1641
      , originalPokemonID = Nothing
      , fullName = "Quaquaval"
      , typing = Double Water Fighting
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "quaquaval"
      , evolutionData = EvolvesFrom [ 1640 ] "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 1642
      , originalPokemonID = Nothing
      , fullName = "Lechonk"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "lechonk"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1643
      , originalPokemonID = Nothing
      , fullName = "Oinkologne"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "oinkologne"
      , evolutionData = EvolvesFrom [ 1642 ] "Level 18"
      , transformationData = DoesNotTransform
      }
    , { id = 1644
      , originalPokemonID = Nothing
      , fullName = "Tarountula"
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "tarountula"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1645
      , originalPokemonID = Nothing
      , fullName = "Spidops"
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "spidops"
      , evolutionData = EvolvesFrom [ 1644 ] "Level 15"
      , transformationData = DoesNotTransform
      }
    , { id = 1646
      , originalPokemonID = Nothing
      , fullName = "Nymble"
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "nymble"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1647
      , originalPokemonID = Nothing
      , fullName = "Lokix"
      , typing = Double Bug Dark
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "lokix"
      , evolutionData = EvolvesFrom [ 1646 ] "Level 24"
      , transformationData = DoesNotTransform
      }
    , { id = 1648
      , originalPokemonID = Nothing
      , fullName = "Pawmi"
      , typing = Single Electric
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "pawmi"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1649
      , originalPokemonID = Nothing
      , fullName = "Pawmo"
      , typing = Double Electric Fighting
      , ability = Just VoltAbsorb
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "pawmo"
      , evolutionData = EvolvesFrom [ 1648 ] "Level 18"
      , transformationData = DoesNotTransform
      }
    , { id = 1650
      , originalPokemonID = Nothing
      , fullName = "Pawmot"
      , typing = Double Electric Fighting
      , ability = Just VoltAbsorb
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "pawmot"
      , evolutionData = EvolvesFrom [ 1649 ] "While outside of its Poké Ball after walking 1000 steps using the Let's Go feature"
      , transformationData = DoesNotTransform
      }
    , { id = 1653
      , originalPokemonID = Nothing
      , fullName = "Tandemaus"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "tandemaus"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1654
      , originalPokemonID = Nothing
      , fullName = "Maushold"
      , typing = Single Normal
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "maushold"
      , evolutionData = EvolvesFrom [ 1653 ] "Level 25 while battling"
      , transformationData = DoesNotTransform
      }
    , { id = 1655
      , originalPokemonID = Nothing
      , fullName = "Fidough"
      , typing = Single Fairy
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "fidough"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1656
      , originalPokemonID = Nothing
      , fullName = "Dachsbun"
      , typing = Single Fairy
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "dachsbun"
      , evolutionData = EvolvesFrom [ 1655 ] "Level 26"
      , transformationData = DoesNotTransform
      }
    , { id = 1657
      , originalPokemonID = Nothing
      , fullName = "Smoliv"
      , typing = Double Grass Normal
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "smoliv"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1658
      , originalPokemonID = Nothing
      , fullName = "Dolliv"
      , typing = Double Grass Normal
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "dolliv"
      , evolutionData = EvolvesFrom [ 1657 ] "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 1659
      , originalPokemonID = Nothing
      , fullName = "Arboliva"
      , typing = Double Grass Normal
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "arboliva"
      , evolutionData = EvolvesFrom [ 1658 ] "Level 35"
      , transformationData = DoesNotTransform
      }
    , { id = 1660
      , originalPokemonID = Nothing
      , fullName = "Squawkabilly"
      , typing = Double Normal Flying
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "squawkabilly"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1661
      , originalPokemonID = Nothing
      , fullName = "Nacli"
      , typing = Single Rock
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "nacli"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1662
      , originalPokemonID = Nothing
      , fullName = "Naclstack"
      , typing = Single Rock
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "naclstack"
      , evolutionData = EvolvesFrom [ 1661 ] "Level 24"
      , transformationData = DoesNotTransform
      }
    , { id = 1663
      , originalPokemonID = Nothing
      , fullName = "Garganacl"
      , typing = Single Rock
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "garganacl"
      , evolutionData = EvolvesFrom [ 1662 ] "Level 38"
      , transformationData = DoesNotTransform
      }
    , { id = 1665
      , originalPokemonID = Nothing
      , fullName = "Charcadet"
      , typing = Single Fire
      , ability = Just FlashFire
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "charcadet"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1666
      , originalPokemonID = Nothing
      , fullName = "Armarouge"
      , typing = Double Fire Psychic
      , ability = Just FlashFire
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "armarouge"
      , evolutionData = EvolvesFrom [ 1665 ] "Use Auspicious Armor"
      , transformationData = DoesNotTransform
      }
    , { id = 1667
      , originalPokemonID = Nothing
      , fullName = "Ceruledge"
      , typing = Double Fire Ghost
      , ability = Just FlashFire
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "ceruledge"
      , evolutionData = EvolvesFrom [ 1665 ] "Use Malicious Armor"
      , transformationData = DoesNotTransform
      }
    , { id = 1668
      , originalPokemonID = Nothing
      , fullName = "Tadbulb"
      , typing = Single Electric
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "tadbulb"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1669
      , originalPokemonID = Nothing
      , fullName = "Bellibolt"
      , typing = Single Electric
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "bellibolt"
      , evolutionData = EvolvesFrom [ 1668 ] "Use Thunder Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 1670
      , originalPokemonID = Nothing
      , fullName = "Wattrel"
      , typing = Double Electric Flying
      , ability = Just VoltAbsorb
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "wattrel"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1671
      , originalPokemonID = Nothing
      , fullName = "Kilowattrel"
      , typing = Double Electric Flying
      , ability = Just VoltAbsorb
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "kilowattrel"
      , evolutionData = EvolvesFrom [ 1670 ] "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 1674
      , originalPokemonID = Nothing
      , fullName = "Maschiff"
      , typing = Single Dark
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "maschiff"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1675
      , originalPokemonID = Nothing
      , fullName = "Mabosstiff"
      , typing = Single Dark
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "mabosstiff"
      , evolutionData = EvolvesFrom [ 1674 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 1676
      , originalPokemonID = Nothing
      , fullName = "Shroodle"
      , typing = Double Poison Normal
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "shroodle"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1677
      , originalPokemonID = Nothing
      , fullName = "Grafaiai"
      , typing = Double Poison Normal
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "grafaiai"
      , evolutionData = EvolvesFrom [ 1676 ] "Level 28"
      , transformationData = DoesNotTransform
      }
    , { id = 1678
      , originalPokemonID = Just 224
      , fullName = nameFromData "Tauros" "Paldean" ""
      , typing = Single Fighting
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "tauros-paldean"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1679
      , originalPokemonID = Just 224
      , fullName = nameFromData "Tauros" "Paldean Aqua Breed" "Paldean Tauros (Aqua Breed)"
      , typing = Double Fighting Fire
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "tauros-paldean-water"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1680
      , originalPokemonID = Just 224
      , fullName = nameFromData "Tauros" "Paldean Blaze Breed" "Paldean Tauros (Blaze Breed)"
      , typing = Double Fighting Water
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "tauros-paldean-fire"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1681
      , originalPokemonID = Nothing
      , fullName = "Bramblin"
      , typing = Double Grass Ghost
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "bramblin"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1682
      , originalPokemonID = Nothing
      , fullName = "Brambleghast"
      , typing = Double Grass Ghost
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "brambleghast"
      , evolutionData = EvolvesFrom [ 1681 ] "While outside of its Poké Ball after walking 1000 steps using the Let's Go feature"
      , transformationData = DoesNotTransform
      }
    , { id = 1683
      , originalPokemonID = Nothing
      , fullName = "Toedscool"
      , typing = Double Ground Grass
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "toedscool"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1684
      , originalPokemonID = Nothing
      , fullName = "Toedscruel"
      , typing = Double Ground Grass
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "toedscruel"
      , evolutionData = EvolvesFrom [ 1683 ] "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 1685
      , originalPokemonID = Nothing
      , fullName = "Klawf"
      , typing = Single Rock
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "klawf"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1686
      , originalPokemonID = Nothing
      , fullName = "Capsakid"
      , typing = Single Grass
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "capsakid"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1687
      , originalPokemonID = Nothing
      , fullName = "Scovillain"
      , typing = Double Grass Fire
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "scovillain"
      , evolutionData = EvolvesFrom [ 1686 ] "Use Fire Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 1688
      , originalPokemonID = Nothing
      , fullName = "Rellor"
      , typing = Single Bug
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "rellor"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1689
      , originalPokemonID = Nothing
      , fullName = "Rabsca"
      , typing = Double Bug Psychic
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "rabsca"
      , evolutionData = EvolvesFrom [ 1688 ] "While outside of its Poké Ball after walking 1000 steps using the Let's Go feature"
      , transformationData = DoesNotTransform
      }
    , { id = 1690
      , originalPokemonID = Nothing
      , fullName = "Flittle"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "flittle"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1691
      , originalPokemonID = Nothing
      , fullName = "Espathra"
      , typing = Single Psychic
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "espathra"
      , evolutionData = EvolvesFrom [ 1690 ] "Level 35"
      , transformationData = DoesNotTransform
      }
    , { id = 1692
      , originalPokemonID = Nothing
      , fullName = "Tinkatink"
      , typing = Double Fairy Steel
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "tinkatink"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1693
      , originalPokemonID = Nothing
      , fullName = "Tinkatuff"
      , typing = Double Fairy Steel
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "tinkatuff"
      , evolutionData = EvolvesFrom [ 1692 ] "Level 24"
      , transformationData = DoesNotTransform
      }
    , { id = 1694
      , originalPokemonID = Nothing
      , fullName = "Tinkaton"
      , typing = Double Fairy Steel
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "tinkaton"
      , evolutionData = EvolvesFrom [ 1693 ] "Level 38"
      , transformationData = DoesNotTransform
      }
    , { id = 1695
      , originalPokemonID = Nothing
      , fullName = "Wiglett"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "wiglett"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1696
      , originalPokemonID = Nothing
      , fullName = "Wugtrio"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "wugtrio"
      , evolutionData = EvolvesFrom [ 1695 ] "Level 26"
      , transformationData = DoesNotTransform
      }
    , { id = 1697
      , originalPokemonID = Nothing
      , fullName = "Bombirdier"
      , typing = Double Flying Dark
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "bombirdier"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1698
      , originalPokemonID = Nothing
      , fullName = "Finizen"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "finizen"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1699
      , originalPokemonID = Nothing
      , fullName = "Palafin"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "palafin"
      , evolutionData = EvolvesFrom [ 1698 ] "Level 38 while in the Union Circle with another player"
      , transformationData = DoesNotTransform
      }
    , { id = 1700
      , originalPokemonID = Nothing
      , fullName = "Varoom"
      , typing = Double Steel Poison
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "varoom"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1701
      , originalPokemonID = Nothing
      , fullName = "Revavroom"
      , typing = Double Steel Poison
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "revavroom"
      , evolutionData = EvolvesFrom [ 1700 ] "Level 40"
      , transformationData = DoesNotTransform
      }
    , { id = 1702
      , originalPokemonID = Nothing
      , fullName = "Cyclizar"
      , typing = Double Dragon Normal
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "cyclizar"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1703
      , originalPokemonID = Nothing
      , fullName = "Orthworm"
      , typing = Single Steel
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "orthworm"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1704
      , originalPokemonID = Nothing
      , fullName = "Glimmet"
      , typing = Double Rock Poison
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "glimmet"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1705
      , originalPokemonID = Nothing
      , fullName = "Glimmora"
      , typing = Double Rock Poison
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "glimmora"
      , evolutionData = EvolvesFrom [ 1704 ] "Level 35"
      , transformationData = DoesNotTransform
      }
    , { id = 1706
      , originalPokemonID = Nothing
      , fullName = "Greavard"
      , typing = Single Ghost
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "greavard"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1707
      , originalPokemonID = Nothing
      , fullName = "Houndstone"
      , typing = Single Ghost
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "houndstone"
      , evolutionData = EvolvesFrom [ 1706 ] "Level 30 at night"
      , transformationData = DoesNotTransform
      }
    , { id = 1708
      , originalPokemonID = Nothing
      , fullName = "Flamigo"
      , typing = Double Flying Fighting
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "flamigo"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1709
      , originalPokemonID = Nothing
      , fullName = "Cetoddle"
      , typing = Single Ice
      , ability = Just ThickFat
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "cetoddle"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1710
      , originalPokemonID = Nothing
      , fullName = "Cetitan"
      , typing = Single Ice
      , ability = Just ThickFat
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "cetitan"
      , evolutionData = EvolvesFrom [ 1709 ] "Use Ice Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 1712
      , originalPokemonID = Nothing
      , fullName = "Veluza"
      , typing = Double Water Psychic
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "veluza"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1713
      , originalPokemonID = Nothing
      , fullName = "Dondozo"
      , typing = Single Water
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "dondozo"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1714
      , originalPokemonID = Nothing
      , fullName = "Tatsugiri"
      , typing = Double Dragon Water
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "tatsugiri"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1715
      , originalPokemonID = Nothing
      , fullName = "Great Tusk"
      , typing = Double Ground Fighting
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "great-tusk"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1716
      , originalPokemonID = Nothing
      , fullName = "Scream Tail"
      , typing = Double Fairy Psychic
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "scream-tail"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1717
      , originalPokemonID = Nothing
      , fullName = "Brute Bonnet"
      , typing = Double Grass Dark
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "brute-bonnet"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1718
      , originalPokemonID = Nothing
      , fullName = "Flutter Mane"
      , typing = Double Ghost Fairy
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "flutter-mane"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1719
      , originalPokemonID = Nothing
      , fullName = "Slither Wing"
      , typing = Double Bug Fighting
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "slither-wing"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1720
      , originalPokemonID = Nothing
      , fullName = "Sandy Shocks"
      , typing = Double Electric Ground
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "sandy-shocks"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1721
      , originalPokemonID = Nothing
      , fullName = "Iron Treads"
      , typing = Double Ground Steel
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "iron-treads"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1722
      , originalPokemonID = Nothing
      , fullName = "Iron Bundle"
      , typing = Double Ice Water
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "iron-bundle"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1723
      , originalPokemonID = Nothing
      , fullName = "Iron Hands"
      , typing = Double Fighting Electric
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "iron-hands"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1724
      , originalPokemonID = Nothing
      , fullName = "Iron Jugulis"
      , typing = Double Dark Flying
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "iron-jugulis"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1725
      , originalPokemonID = Nothing
      , fullName = "Iron Moth"
      , typing = Double Fire Poison
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "iron-moth"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1726
      , originalPokemonID = Nothing
      , fullName = "Iron Thorns"
      , typing = Double Rock Electric
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "iron-thorns"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1727
      , originalPokemonID = Nothing
      , fullName = "Frigibax"
      , typing = Double Dragon Ice
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "frigibax"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1728
      , originalPokemonID = Nothing
      , fullName = "Arctibax"
      , typing = Double Dragon Ice
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "arctibax"
      , evolutionData = EvolvesFrom [ 1727 ] "Level 35"
      , transformationData = DoesNotTransform
      }
    , { id = 1729
      , originalPokemonID = Nothing
      , fullName = "Baxcalibur"
      , typing = Double Dragon Ice
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "baxcalibur"
      , evolutionData = EvolvesFrom [ 1728 ] "Level 54"
      , transformationData = DoesNotTransform
      }
    , { id = 1730
      , originalPokemonID = Nothing
      , fullName = "Gimmighoul"
      , typing = Single Ghost
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "gimmighoul"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1731
      , originalPokemonID = Nothing
      , fullName = "Gholdengo"
      , typing = Double Ghost Steel
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "gholdengo"
      , evolutionData = EvolvesFrom [ 1730 ] "Level while having 999 Ghimmighoul Coins"
      , transformationData = DoesNotTransform
      }
    , { id = 1732
      , originalPokemonID = Nothing
      , fullName = "Wo-Chien"
      , typing = Double Dark Grass
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "wo-chien"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1733
      , originalPokemonID = Nothing
      , fullName = "Chien-Pao"
      , typing = Double Dark Ice
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "chien-pao"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1734
      , originalPokemonID = Nothing
      , fullName = "Ting-Lu"
      , typing = Double Dark Ground
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "ting-lu"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1735
      , originalPokemonID = Nothing
      , fullName = "Chi-Yu"
      , typing = Double Dark Fire
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "chi-yu"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1736
      , originalPokemonID = Nothing
      , fullName = "Roaring Moon"
      , typing = Double Dragon Dark
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "roaring-moon"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1737
      , originalPokemonID = Nothing
      , fullName = "Iron Valiant"
      , typing = Double Fighting Fairy
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "iron-valiant"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1738
      , originalPokemonID = Nothing
      , fullName = "Koraidon"
      , typing = Double Fighting Dragon
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "koraidon"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    , { id = 1739
      , originalPokemonID = Nothing
      , fullName = "Miraidon"
      , typing = Double Electric Dragon
      , ability = Nothing
      , imageUrl = imageUrlByGenerationAndID "scarlet-violet" "miraidon"
      , evolutionData = DoesNotEvolve
      , transformationData = DoesNotTransform
      }
    ]
