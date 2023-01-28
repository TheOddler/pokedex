module Pokemon.Data exposing (all, first)

import Ability exposing (Ability(..))
import Pokemon exposing (..)
import String.Extra as String
import StringHelpers as String
import Type exposing (..)


type alias PokemonData =
    { id : PokemonID
    , nationalDexNumber : NationDexNumber
    , name : String
    , form : Form
    , typing : Typing
    , ability : Maybe Ability
    , evolutionData : EvolutionData
    , transformationData : TransformationData
    }


type Form
    = Original
    | Mega
    | MegaX
    | MegaY
    | Unique String
    | Regional PokemonID Region
    | UniqueRegional PokemonID Region String


type Region
    = Alola
    | Galar
    | Hisui
    | Paldea


dataToPokemon : PokemonData -> Pokemon
dataToPokemon data =
    { id = data.id
    , nationalDexNumber = data.nationalDexNumber
    , fullName = fullNameFromData data
    , typing = data.typing
    , ability = data.ability
    , imageUrl = imageUrlFromData data
    , evolutionData = data.evolutionData
    , transformationData = data.transformationData
    }


fullNameFromData : PokemonData -> String
fullNameFromData pkmData =
    case pkmData.form of
        Original ->
            pkmData.name

        Mega ->
            "Mega " ++ pkmData.name

        MegaX ->
            "Mega " ++ pkmData.name ++ " X"

        MegaY ->
            "Mega " ++ pkmData.name ++ " Y"

        Unique form ->
            form ++ " " ++ pkmData.name

        Regional _ region ->
            regionToName region ++ " " ++ pkmData.name

        UniqueRegional _ region form ->
            regionToName region ++ " " ++ form ++ " " ++ pkmData.name


regionToName : Region -> String
regionToName region =
    case region of
        Alola ->
            "Alolan"

        Galar ->
            "Galarian"

        Hisui ->
            "Hisuian"

        Paldea ->
            "Paldean"


imageStringPartCleanup : String -> String
imageStringPartCleanup =
    String.removeAll [ ":", "%", " ♀", "♀", " ♂", "♂", "'" ]
        >> String.replaceAll [ ( " ", "_" ) ]
        >> String.removeAccents
        >> String.removeTrailingDot


imageUrlFromData : PokemonData -> String
imageUrlFromData pkmData =
    "images/"
        ++ String.right 4 ("000" ++ String.fromInt pkmData.nationalDexNumber)
        ++ imageStringPartCleanup pkmData.name
        ++ (case pkmData.form of
                Original ->
                    ""

                Mega ->
                    "-Mega"

                MegaX ->
                    "-Mega_X"

                MegaY ->
                    "-Mega_Y"

                Unique form ->
                    "-" ++ imageStringPartCleanup form

                Regional _ region ->
                    "-" ++ regionToImageIDPart region

                UniqueRegional _ region form ->
                    "-" ++ regionToImageIDPart region ++ "_" ++ imageStringPartCleanup form
           )
        ++ ".webp"


regionToImageIDPart : Region -> String
regionToImageIDPart region =
    case region of
        Alola ->
            "Alola"

        Galar ->
            "Galar"

        Hisui ->
            "Hisui"

        Paldea ->
            "Paldea"


first : Pokemon
first =
    dataToPokemon firstData


all : List Pokemon
all =
    List.map dataToPokemon allData


firstData : PokemonData
firstData =
    { id = 1
    , nationalDexNumber = 1
    , name = "Bulbasaur"
    , form = Original
    , typing = Double Grass Poison
    , ability = Nothing
    , evolutionData = IsNotEvolved
    , transformationData = DoesNotTransform
    }


allData : List PokemonData
allData =
    [ firstData
    , { id = 2
      , nationalDexNumber = 2
      , name = "Ivysaur"
      , form = Original
      , typing = Double Grass Poison
      , ability = Nothing
      , evolutionData = EvolvesFrom 1 "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 3
      , nationalDexNumber = 3
      , name = "Venusaur"
      , form = Original
      , typing = Double Grass Poison
      , ability = Nothing
      , evolutionData = EvolvesFrom 2 "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 4
      , nationalDexNumber = 3
      , name = "Venusaur"
      , form = Mega
      , typing = Double Grass Poison
      , ability = Just ThickFat
      , evolutionData = EvolvesFrom 3 "Holding Venusaurite"
      , transformationData = DoesNotTransform
      }
    , { id = 5
      , nationalDexNumber = 4
      , name = "Charmander"
      , form = Original
      , typing = Single Fire
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 6
      , nationalDexNumber = 5
      , name = "Charmeleon"
      , form = Original
      , typing = Single Fire
      , ability = Nothing
      , evolutionData = EvolvesFrom 5 "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 7
      , nationalDexNumber = 6
      , name = "Charizard"
      , form = Original
      , typing = Double Fire Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 6 "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 8
      , nationalDexNumber = 6
      , name = "Charizard"
      , form = MegaX
      , typing = Double Fire Dragon
      , ability = Nothing
      , evolutionData = EvolvesFrom 7 "Holding Charizardite X"
      , transformationData = DoesNotTransform
      }
    , { id = 9
      , nationalDexNumber = 6
      , name = "Charizard"
      , form = MegaY
      , typing = Double Fire Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 7 "Holding Charizardite Y"
      , transformationData = DoesNotTransform
      }
    , { id = 10
      , nationalDexNumber = 7
      , name = "Squirtle"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 11
      , nationalDexNumber = 8
      , name = "Wartortle"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = EvolvesFrom 10 "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 12
      , nationalDexNumber = 9
      , name = "Blastoise"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = EvolvesFrom 11 "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 13
      , nationalDexNumber = 9
      , name = "Blastoise"
      , form = Mega
      , typing = Single Water
      , ability = Nothing
      , evolutionData = EvolvesFrom 12 "Holding Blastoisinite"
      , transformationData = DoesNotTransform
      }
    , { id = 32
      , nationalDexNumber = 10
      , name = "Caterpie"
      , form = Original
      , typing = Single Bug
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 23
      , nationalDexNumber = 11
      , name = "Metapod"
      , form = Original
      , typing = Single Bug
      , ability = Nothing
      , evolutionData = EvolvesFrom 32 "Level 7"
      , transformationData = DoesNotTransform
      }
    , { id = 24
      , nationalDexNumber = 12
      , name = "Butterfree"
      , form = Original
      , typing = Double Bug Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 23 "Level 10"
      , transformationData = DoesNotTransform
      }
    , { id = 31
      , nationalDexNumber = 13
      , name = "Weedle"
      , form = Original
      , typing = Double Bug Poison
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 30
      , nationalDexNumber = 14
      , name = "Kakuna"
      , form = Original
      , typing = Double Bug Poison
      , ability = Nothing
      , evolutionData = EvolvesFrom 31 "Level 7"
      , transformationData = DoesNotTransform
      }
    , { id = 27
      , nationalDexNumber = 15
      , name = "Beedrill"
      , form = Original
      , typing = Double Bug Poison
      , ability = Nothing
      , evolutionData = EvolvesFrom 30 "Level 10"
      , transformationData = DoesNotTransform
      }
    , { id = 28
      , nationalDexNumber = 15
      , name = "Beedrill"
      , form = Mega
      , typing = Double Bug Poison
      , ability = Nothing
      , evolutionData = EvolvesFrom 27 "Holding Beedrillite"
      , transformationData = DoesNotTransform
      }
    , { id = 76
      , nationalDexNumber = 16
      , name = "Pidgey"
      , form = Original
      , typing = Double Normal Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 77
      , nationalDexNumber = 17
      , name = "Pidgeotto"
      , form = Original
      , typing = Double Normal Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 76 "Level 18"
      , transformationData = DoesNotTransform
      }
    , { id = 78
      , nationalDexNumber = 18
      , name = "Pidgeot"
      , form = Original
      , typing = Double Normal Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 77 "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 79
      , nationalDexNumber = 18
      , name = "Pidgeot"
      , form = Mega
      , typing = Double Normal Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 78 "Holding Pidgeotite"
      , transformationData = DoesNotTransform
      }
    , { id = 80
      , nationalDexNumber = 19
      , name = "Rattata"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 82
      , nationalDexNumber = 20
      , name = "Raticate"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = EvolvesFrom 80 "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 81
      , nationalDexNumber = 19
      , name = "Rattata"
      , form = Regional 80 Alola
      , typing = Double Dark Normal
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 83
      , nationalDexNumber = 20
      , name = "Raticate"
      , form = Regional 82 Alola
      , typing = Double Dark Normal
      , ability = Nothing
      , evolutionData = EvolvesFrom 81 "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 84
      , nationalDexNumber = 21
      , name = "Spearow"
      , form = Original
      , typing = Double Normal Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 85
      , nationalDexNumber = 22
      , name = "Fearow"
      , form = Original
      , typing = Double Normal Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 84 "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 86
      , nationalDexNumber = 23
      , name = "Ekans"
      , form = Original
      , typing = Single Poison
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 87
      , nationalDexNumber = 24
      , name = "Arbok"
      , form = Original
      , typing = Single Poison
      , ability = Nothing
      , evolutionData = EvolvesFrom 86 "Level 22"
      , transformationData = DoesNotTransform
      }
    , { id = 88
      , nationalDexNumber = 172
      , name = "Pichu"
      , form = Original
      , typing = Single Electric
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 89
      , nationalDexNumber = 25
      , name = "Pikachu"
      , form = Original
      , typing = Single Electric
      , ability = Nothing
      , evolutionData = EvolvesFrom 88 "Level with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 90
      , nationalDexNumber = 26
      , name = "Raichu"
      , form = Original
      , typing = Single Electric
      , ability = Nothing
      , evolutionData = EvolvesFrom 89 "Use Thunder Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 91
      , nationalDexNumber = 26
      , name = "Raichu"
      , form = Regional 90 Alola
      , typing = Double Electric Psychic
      , ability = Nothing
      , evolutionData = EvolvesFrom 89 "Use Thunder Stone in Alola"
      , transformationData = DoesNotTransform
      }
    , { id = 92
      , nationalDexNumber = 27
      , name = "Sandshrew"
      , form = Original
      , typing = Single Ground
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 94
      , nationalDexNumber = 28
      , name = "Sandslash"
      , form = Original
      , typing = Single Ground
      , ability = Nothing
      , evolutionData = EvolvesFrom 92 "Level 22"
      , transformationData = DoesNotTransform
      }
    , { id = 93
      , nationalDexNumber = 27
      , name = "Sandshrew"
      , form = Regional 92 Alola
      , typing = Double Ice Steel
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 95
      , nationalDexNumber = 28
      , name = "Sandslash"
      , form = Regional 94 Alola
      , typing = Double Ice Steel
      , ability = Nothing
      , evolutionData = EvolvesFrom 93 "Use Ice Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 33
      , nationalDexNumber = 29
      , name = "Nidoran ♀"
      , form = Original
      , typing = Single Poison
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 17
      , nationalDexNumber = 30
      , name = "Nidorina"
      , form = Original
      , typing = Single Poison
      , ability = Nothing
      , evolutionData = EvolvesFrom 33 "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 18
      , nationalDexNumber = 31
      , name = "Nidoqueen"
      , form = Original
      , typing = Double Poison Ground
      , ability = Nothing
      , evolutionData = EvolvesFrom 17 "Use Moon Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 48
      , nationalDexNumber = 32
      , name = "Nidoran ♂"
      , form = Original
      , typing = Single Poison
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 49
      , nationalDexNumber = 33
      , name = "Nidorino"
      , form = Original
      , typing = Single Poison
      , ability = Nothing
      , evolutionData = EvolvesFrom 48 "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 50
      , nationalDexNumber = 34
      , name = "Nidoking"
      , form = Original
      , typing = Double Poison Ground
      , ability = Nothing
      , evolutionData = EvolvesFrom 49 "Use Moon Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 51
      , nationalDexNumber = 173
      , name = "Cleffa"
      , form = Original
      , typing = Single Fairy
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 52
      , nationalDexNumber = 35
      , name = "Clefairy"
      , form = Original
      , typing = Single Fairy
      , ability = Nothing
      , evolutionData = EvolvesFrom 51 "Level with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 53
      , nationalDexNumber = 36
      , name = "Clefable"
      , form = Original
      , typing = Single Fairy
      , ability = Nothing
      , evolutionData = EvolvesFrom 52 "Use Moon Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 54
      , nationalDexNumber = 37
      , name = "Vulpix"
      , form = Original
      , typing = Single Fire
      , ability = Just FlashFire
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 56
      , nationalDexNumber = 38
      , name = "Ninetales"
      , form = Original
      , typing = Single Fire
      , ability = Just FlashFire
      , evolutionData = EvolvesFrom 54 "Use Fire Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 55
      , nationalDexNumber = 37
      , name = "Vulpix"
      , form = Regional 54 Alola
      , typing = Single Ice
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 57
      , nationalDexNumber = 38
      , name = "Ninetales"
      , form = Regional 56 Alola
      , typing = Double Ice Fairy
      , ability = Nothing
      , evolutionData = EvolvesFrom 55 "Use Ice Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 58
      , nationalDexNumber = 174
      , name = "Igglybuff"
      , form = Original
      , typing = Double Normal Fairy
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 59
      , nationalDexNumber = 39
      , name = "Jigglypuff"
      , form = Original
      , typing = Double Normal Fairy
      , ability = Nothing
      , evolutionData = EvolvesFrom 58 "Level with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 60
      , nationalDexNumber = 40
      , name = "Wigglytuff"
      , form = Original
      , typing = Double Normal Fairy
      , ability = Nothing
      , evolutionData = EvolvesFrom 59 "Use Moon Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 61
      , nationalDexNumber = 41
      , name = "Zubat"
      , form = Original
      , typing = Double Poison Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 62
      , nationalDexNumber = 42
      , name = "Golbat"
      , form = Original
      , typing = Double Poison Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 61 "Level 22"
      , transformationData = DoesNotTransform
      }
    , { id = 63
      , nationalDexNumber = 169
      , name = "Crobat"
      , form = Original
      , typing = Double Poison Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 62 "Level with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 64
      , nationalDexNumber = 43
      , name = "Oddish"
      , form = Original
      , typing = Double Grass Poison
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 65
      , nationalDexNumber = 44
      , name = "Gloom"
      , form = Original
      , typing = Double Grass Poison
      , ability = Nothing
      , evolutionData = EvolvesFrom 64 "Level 21"
      , transformationData = DoesNotTransform
      }
    , { id = 66
      , nationalDexNumber = 45
      , name = "Vileplume"
      , form = Original
      , typing = Double Grass Poison
      , ability = Nothing
      , evolutionData = EvolvesFrom 65 "Use Leaf Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 67
      , nationalDexNumber = 182
      , name = "Bellossom"
      , form = Original
      , typing = Single Grass
      , ability = Nothing
      , evolutionData = EvolvesFrom 65 "Use Sun Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 68
      , nationalDexNumber = 46
      , name = "Paras"
      , form = Original
      , typing = Double Bug Grass
      , ability = Just DrySkin
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 69
      , nationalDexNumber = 47
      , name = "Parasect"
      , form = Original
      , typing = Double Bug Grass
      , ability = Just DrySkin
      , evolutionData = EvolvesFrom 68 "Level 24"
      , transformationData = DoesNotTransform
      }
    , { id = 70
      , nationalDexNumber = 48
      , name = "Venonat"
      , form = Original
      , typing = Double Bug Poison
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 71
      , nationalDexNumber = 49
      , name = "Venomoth"
      , form = Original
      , typing = Double Bug Poison
      , ability = Nothing
      , evolutionData = EvolvesFrom 70 "Level 31"
      , transformationData = DoesNotTransform
      }
    , { id = 72
      , nationalDexNumber = 50
      , name = "Diglett"
      , form = Original
      , typing = Single Ground
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 73
      , nationalDexNumber = 51
      , name = "Dugtrio"
      , form = Original
      , typing = Single Ground
      , ability = Nothing
      , evolutionData = EvolvesFrom 72 "Level 26"
      , transformationData = DoesNotTransform
      }
    , { id = 74
      , nationalDexNumber = 50
      , name = "Diglett"
      , form = Regional 72 Alola
      , typing = Double Ground Steel
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 75
      , nationalDexNumber = 51
      , name = "Dugtrio"
      , form = Regional 73 Alola
      , typing = Double Ground Steel
      , ability = Nothing
      , evolutionData = EvolvesFrom 74 "Level 26 in Alola"
      , transformationData = DoesNotTransform
      }
    , { id = 96
      , nationalDexNumber = 52
      , name = "Meowth"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 100
      , nationalDexNumber = 53
      , name = "Persian"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = EvolvesFrom 96 "Level 28"
      , transformationData = DoesNotTransform
      }
    , { id = 97
      , nationalDexNumber = 52
      , name = "Meowth"
      , form = Regional 96 Alola
      , typing = Single Dark
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 101
      , nationalDexNumber = 53
      , name = "Persian"
      , form = Regional 100 Alola
      , typing = Single Dark
      , ability = Nothing
      , evolutionData = EvolvesFrom 97 "Level with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 98
      , nationalDexNumber = 52
      , name = "Meowth"
      , form = Regional 96 Galar
      , typing = Single Steel
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 99
      , nationalDexNumber = 863
      , name = "Perrserker"
      , form = Original
      , typing = Single Steel
      , ability = Nothing
      , evolutionData = EvolvesFrom 98 "Level 28"
      , transformationData = DoesNotTransform
      }
    , { id = 102
      , nationalDexNumber = 54
      , name = "Psyduck"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 884
      , nationalDexNumber = 55
      , name = "Golduck"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = EvolvesFrom 102 "Level 33"
      , transformationData = DoesNotTransform
      }
    , { id = 895
      , nationalDexNumber = 56
      , name = "Mankey"
      , form = Original
      , typing = Single Fighting
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 924
      , nationalDexNumber = 57
      , name = "Primeape"
      , form = Original
      , typing = Single Fighting
      , ability = Nothing
      , evolutionData = EvolvesFrom 895 "Level 28"
      , transformationData = DoesNotTransform
      }
    , { id = 1664
      , nationalDexNumber = 979
      , name = "Annihilape"
      , form = Original
      , typing = Double Fighting Ghost
      , ability = Nothing
      , evolutionData = EvolvesFrom 924 "After using Rage Fist 20 times"
      , transformationData = DoesNotTransform
      }
    , { id = 949
      , nationalDexNumber = 58
      , name = "Growlithe"
      , form = Original
      , typing = Single Fire
      , ability = Just FlashFire
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 960
      , nationalDexNumber = 59
      , name = "Arcanine"
      , form = Original
      , typing = Single Fire
      , ability = Just FlashFire
      , evolutionData = EvolvesFrom 949 "Use Fire Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 1619
      , nationalDexNumber = 58
      , name = "Growlithe"
      , form = Regional 949 Hisui
      , typing = Double Fire Rock
      , ability = Just FlashFire
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1620
      , nationalDexNumber = 59
      , name = "Arcanine"
      , form = Regional 960 Hisui
      , typing = Double Fire Rock
      , ability = Just FlashFire
      , evolutionData = EvolvesFrom 1619 "Use Fire Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 971
      , nationalDexNumber = 60
      , name = "Poliwag"
      , form = Original
      , typing = Single Water
      , ability = Just WaterAbsorb
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 982
      , nationalDexNumber = 61
      , name = "Poliwhirl"
      , form = Original
      , typing = Single Water
      , ability = Just WaterAbsorb
      , evolutionData = EvolvesFrom 971 "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 993
      , nationalDexNumber = 62
      , name = "Poliwrath"
      , form = Original
      , typing = Double Water Fighting
      , ability = Just WaterAbsorb
      , evolutionData = EvolvesFrom 982 "Use Water Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 286
      , nationalDexNumber = 186
      , name = "Politoed"
      , form = Original
      , typing = Single Water
      , ability = Just WaterAbsorb
      , evolutionData = EvolvesFrom 982 "Trade holding King's Rock"
      , transformationData = DoesNotTransform
      }
    , { id = 1004
      , nationalDexNumber = 63
      , name = "Abra"
      , form = Original
      , typing = Single Psychic
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1015
      , nationalDexNumber = 64
      , name = "Kadabra"
      , form = Original
      , typing = Single Psychic
      , ability = Nothing
      , evolutionData = EvolvesFrom 1004 "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 1026
      , nationalDexNumber = 65
      , name = "Alakazam"
      , form = Original
      , typing = Single Psychic
      , ability = Nothing
      , evolutionData = EvolvesFrom 1015 "Trade"
      , transformationData = DoesNotTransform
      }
    , { id = 1360
      , nationalDexNumber = 65
      , name = "Alakazam"
      , form = Mega
      , typing = Single Psychic
      , ability = Nothing
      , evolutionData = EvolvesFrom 1026 "Holding Alakazite"
      , transformationData = DoesNotTransform
      }
    , { id = 1076
      , nationalDexNumber = 66
      , name = "Machop"
      , form = Original
      , typing = Single Fighting
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1087
      , nationalDexNumber = 67
      , name = "Machoke"
      , form = Original
      , typing = Single Fighting
      , ability = Nothing
      , evolutionData = EvolvesFrom 1076 "Level 28"
      , transformationData = DoesNotTransform
      }
    , { id = 1097
      , nationalDexNumber = 68
      , name = "Machamp"
      , form = Original
      , typing = Single Fighting
      , ability = Nothing
      , evolutionData = EvolvesFrom 1087 "Trade"
      , transformationData = DoesNotTransform
      }
    , { id = 1109
      , nationalDexNumber = 69
      , name = "Bellsprout"
      , form = Original
      , typing = Double Grass Poison
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1128
      , nationalDexNumber = 70
      , name = "Weepinbell"
      , form = Original
      , typing = Double Grass Poison
      , ability = Nothing
      , evolutionData = EvolvesFrom 1109 "Level 21"
      , transformationData = DoesNotTransform
      }
    , { id = 1139
      , nationalDexNumber = 71
      , name = "Victreebel"
      , form = Original
      , typing = Double Grass Poison
      , ability = Nothing
      , evolutionData = EvolvesFrom 1128 "Use Leaf Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 1150
      , nationalDexNumber = 72
      , name = "Tentacool"
      , form = Original
      , typing = Double Water Poison
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1254
      , nationalDexNumber = 73
      , name = "Tentacruel"
      , form = Original
      , typing = Double Water Poison
      , ability = Nothing
      , evolutionData = EvolvesFrom 1150 "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 1265
      , nationalDexNumber = 74
      , name = "Geodude"
      , form = Original
      , typing = Double Rock Ground
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1276
      , nationalDexNumber = 75
      , name = "Graveler"
      , form = Original
      , typing = Double Rock Ground
      , ability = Nothing
      , evolutionData = EvolvesFrom 1265 "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 1287
      , nationalDexNumber = 76
      , name = "Golem"
      , form = Original
      , typing = Double Rock Ground
      , ability = Nothing
      , evolutionData = EvolvesFrom 1276 "Trade"
      , transformationData = DoesNotTransform
      }
    , { id = 1361
      , nationalDexNumber = 74
      , name = "Geodude"
      , form = Regional 1265 Alola
      , typing = Double Rock Electric
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1362
      , nationalDexNumber = 75
      , name = "Graveler"
      , form = Regional 1276 Alola
      , typing = Double Rock Electric
      , ability = Nothing
      , evolutionData = EvolvesFrom 1361 "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 1363
      , nationalDexNumber = 76
      , name = "Golem"
      , form = Regional 1287 Alola
      , typing = Double Rock Electric
      , ability = Nothing
      , evolutionData = EvolvesFrom 1362 "Trade"
      , transformationData = DoesNotTransform
      }
    , { id = 1297
      , nationalDexNumber = 77
      , name = "Ponyta"
      , form = Original
      , typing = Single Fire
      , ability = Just FlashFire
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1308
      , nationalDexNumber = 78
      , name = "Rapidash"
      , form = Original
      , typing = Single Fire
      , ability = Just FlashFire
      , evolutionData = EvolvesFrom 1297 "Level 40"
      , transformationData = DoesNotTransform
      }
    , { id = 1364
      , nationalDexNumber = 77
      , name = "Ponyta"
      , form = Regional 1297 Galar
      , typing = Single Psychic
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1365
      , nationalDexNumber = 78
      , name = "Rapidash"
      , form = Regional 1308 Galar
      , typing = Double Psychic Fairy
      , ability = Nothing
      , evolutionData = EvolvesFrom 1364 "Level 40"
      , transformationData = DoesNotTransform
      }
    , { id = 1319
      , nationalDexNumber = 79
      , name = "Slowpoke"
      , form = Original
      , typing = Double Water Psychic
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1330
      , nationalDexNumber = 80
      , name = "Slowbro"
      , form = Original
      , typing = Double Water Psychic
      , ability = Nothing
      , evolutionData = EvolvesFrom 1319 "Level 37"
      , transformationData = DoesNotTransform
      }
    , { id = 1367
      , nationalDexNumber = 80
      , name = "Slowbro"
      , form = Mega
      , typing = Double Water Psychic
      , ability = Nothing
      , evolutionData = EvolvesFrom 1330 "Holding Slowbronite"
      , transformationData = DoesNotTransform
      }
    , { id = 299
      , nationalDexNumber = 199
      , name = "Slowking"
      , form = Original
      , typing = Double Water Psychic
      , ability = Nothing
      , evolutionData = EvolvesFrom 1319 "Trade holding King's Rock"
      , transformationData = DoesNotTransform
      }
    , { id = 1366
      , nationalDexNumber = 79
      , name = "Slowpoke"
      , form = Regional 1319 Galar
      , typing = Single Psychic
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1368
      , nationalDexNumber = 80
      , name = "Slowbro"
      , form = Regional 1330 Galar
      , typing = Double Poison Psychic
      , ability = Nothing
      , evolutionData = EvolvesFrom 1366 "Use Galarica Cuff"
      , transformationData = DoesNotTransform
      }
    , { id = 1388
      , nationalDexNumber = 199
      , name = "Slowking"
      , form = Regional 299 Galar
      , typing = Double Poison Psychic
      , ability = Nothing
      , evolutionData = EvolvesFrom 1366 "Use Galarica Wreath"
      , transformationData = DoesNotTransform
      }
    , { id = 46
      , nationalDexNumber = 81
      , name = "Magnemite"
      , form = Original
      , typing = Double Electric Steel
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1341
      , nationalDexNumber = 82
      , name = "Magneton"
      , form = Original
      , typing = Double Electric Steel
      , ability = Nothing
      , evolutionData = EvolvesFrom 46 "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 684
      , nationalDexNumber = 462
      , name = "Magnezone"
      , form = Original
      , typing = Double Electric Steel
      , ability = Nothing
      , evolutionData = EvolvesFrom 1341 "Use Thunder Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 1342
      , nationalDexNumber = 83
      , name = "Farfetch'd"
      , form = Original
      , typing = Double Normal Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1369
      , nationalDexNumber = 83
      , name = "Farfetch'd"
      , form = Regional 1342 Galar
      , typing = Single Fighting
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1398
      , nationalDexNumber = 865
      , name = "Sirfetch'd"
      , form = Original
      , typing = Single Fighting
      , ability = Nothing
      , evolutionData = EvolvesFrom 1369 "Land three Critical Hits in one battle"
      , transformationData = DoesNotTransform
      }
    , { id = 1343
      , nationalDexNumber = 84
      , name = "Doduo"
      , form = Original
      , typing = Double Normal Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1344
      , nationalDexNumber = 85
      , name = "Dodrio"
      , form = Original
      , typing = Double Normal Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 1343 "Level 31"
      , transformationData = DoesNotTransform
      }
    , { id = 1345
      , nationalDexNumber = 86
      , name = "Seel"
      , form = Original
      , typing = Single Water
      , ability = Just ThickFat
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1346
      , nationalDexNumber = 87
      , name = "Dewgong"
      , form = Original
      , typing = Double Water Ice
      , ability = Just ThickFat
      , evolutionData = EvolvesFrom 1345 "Level 34"
      , transformationData = DoesNotTransform
      }
    , { id = 1347
      , nationalDexNumber = 88
      , name = "Grimer"
      , form = Original
      , typing = Single Poison
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1348
      , nationalDexNumber = 89
      , name = "Muk"
      , form = Original
      , typing = Single Poison
      , ability = Nothing
      , evolutionData = EvolvesFrom 1347 "Level 38"
      , transformationData = DoesNotTransform
      }
    , { id = 1372
      , nationalDexNumber = 88
      , name = "Grimer"
      , form = Regional 1347 Alola
      , typing = Double Poison Dark
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1373
      , nationalDexNumber = 89
      , name = "Muk"
      , form = Regional 1348 Alola
      , typing = Double Poison Dark
      , ability = Nothing
      , evolutionData = EvolvesFrom 1372 "Level 38"
      , transformationData = DoesNotTransform
      }
    , { id = 1349
      , nationalDexNumber = 90
      , name = "Shellder"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1350
      , nationalDexNumber = 91
      , name = "Cloyster"
      , form = Original
      , typing = Double Water Ice
      , ability = Nothing
      , evolutionData = EvolvesFrom 1349 "Use Water Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 1351
      , nationalDexNumber = 92
      , name = "Gastly"
      , form = Original
      , typing = Double Ghost Poison
      , ability = Just Levitate
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1352
      , nationalDexNumber = 93
      , name = "Haunter"
      , form = Original
      , typing = Double Ghost Poison
      , ability = Just Levitate
      , evolutionData = EvolvesFrom 1351 "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 1353
      , nationalDexNumber = 94
      , name = "Gengar"
      , form = Original
      , typing = Double Ghost Poison
      , ability = Nothing
      , evolutionData = EvolvesFrom 1352 "Trade"
      , transformationData = DoesNotTransform
      }
    , { id = 1371
      , nationalDexNumber = 94
      , name = "Gengar"
      , form = Mega
      , typing = Double Ghost Poison
      , ability = Nothing
      , evolutionData = EvolvesFrom 1353 "Holding Gengarite"
      , transformationData = DoesNotTransform
      }
    , { id = 1354
      , nationalDexNumber = 95
      , name = "Onix"
      , form = Original
      , typing = Double Rock Ground
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 332
      , nationalDexNumber = 208
      , name = "Steelix"
      , form = Original
      , typing = Double Steel Ground
      , ability = Nothing
      , evolutionData = EvolvesFrom 1354 "Trade holding Metal Coat"
      , transformationData = DoesNotTransform
      }
    , { id = 1383
      , nationalDexNumber = 208
      , name = "Steelix"
      , form = Mega
      , typing = Double Steel Ground
      , ability = Nothing
      , evolutionData = EvolvesFrom 332 "Holding Steelixite"
      , transformationData = DoesNotTransform
      }
    , { id = 1355
      , nationalDexNumber = 96
      , name = "Drowzee"
      , form = Original
      , typing = Single Psychic
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1356
      , nationalDexNumber = 97
      , name = "Hypno"
      , form = Original
      , typing = Single Psychic
      , ability = Nothing
      , evolutionData = EvolvesFrom 1355 "Level 26"
      , transformationData = DoesNotTransform
      }
    , { id = 1357
      , nationalDexNumber = 98
      , name = "Krabby"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1358
      , nationalDexNumber = 99
      , name = "Kingler"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = EvolvesFrom 1357 "Level 28"
      , transformationData = DoesNotTransform
      }
    , { id = 47
      , nationalDexNumber = 100
      , name = "Voltorb"
      , form = Original
      , typing = Single Electric
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 105
      , nationalDexNumber = 101
      , name = "Electrode"
      , form = Original
      , typing = Single Electric
      , ability = Nothing
      , evolutionData = EvolvesFrom 47 "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 1621
      , nationalDexNumber = 100
      , name = "Voltorb"
      , form = Regional 47 Hisui
      , typing = Double Electric Grass
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1622
      , nationalDexNumber = 101
      , name = "Electrode"
      , form = Regional 105 Hisui
      , typing = Double Electric Grass
      , ability = Nothing
      , evolutionData = EvolvesFrom 1621 "Use Leaf Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 136
      , nationalDexNumber = 102
      , name = "Exeggcute"
      , form = Original
      , typing = Double Grass Psychic
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 137
      , nationalDexNumber = 103
      , name = "Exeggutor"
      , form = Original
      , typing = Double Grass Psychic
      , ability = Nothing
      , evolutionData = EvolvesFrom 136 "Use Leaf Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 1359
      , nationalDexNumber = 103
      , name = "Exeggutor"
      , form = Regional 137 Alola
      , typing = Double Grass Dragon
      , ability = Nothing
      , evolutionData = EvolvesFrom 136 "Use Leaf Stone in Alola"
      , transformationData = DoesNotTransform
      }
    , { id = 138
      , nationalDexNumber = 104
      , name = "Cubone"
      , form = Original
      , typing = Single Ground
      , ability = Just LightningRod
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 139
      , nationalDexNumber = 105
      , name = "Marowak"
      , form = Original
      , typing = Single Ground
      , ability = Just LightningRod
      , evolutionData = EvolvesFrom 138 "Level 28"
      , transformationData = DoesNotTransform
      }
    , { id = 1374
      , nationalDexNumber = 105
      , name = "Marowak"
      , form = Regional 139 Alola
      , typing = Double Ghost Fire
      , ability = Just LightningRod
      , evolutionData = EvolvesFrom 138 "Level 28 at night"
      , transformationData = DoesNotTransform
      }
    , { id = 360
      , nationalDexNumber = 236
      , name = "Tyrogue"
      , form = Original
      , typing = Single Fighting
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 140
      , nationalDexNumber = 106
      , name = "Hitmonlee"
      , form = Original
      , typing = Single Fighting
      , ability = Nothing
      , evolutionData = EvolvesFrom 360 "Level 20 With Attack > Defense"
      , transformationData = DoesNotTransform
      }
    , { id = 141
      , nationalDexNumber = 107
      , name = "Hitmonchan"
      , form = Original
      , typing = Single Fighting
      , ability = Nothing
      , evolutionData = EvolvesFrom 360 "Level 20 With Attack < Defense"
      , transformationData = DoesNotTransform
      }
    , { id = 361
      , nationalDexNumber = 237
      , name = "Hitmontop"
      , form = Original
      , typing = Single Fighting
      , ability = Nothing
      , evolutionData = EvolvesFrom 360 "Level 20 With Attack = Defense"
      , transformationData = DoesNotTransform
      }
    , { id = 142
      , nationalDexNumber = 108
      , name = "Lickitung"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 685
      , nationalDexNumber = 463
      , name = "Lickilicky"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = EvolvesFrom 142 "Level while knowing Rollout"
      , transformationData = DoesNotTransform
      }
    , { id = 143
      , nationalDexNumber = 109
      , name = "Koffing"
      , form = Original
      , typing = Single Poison
      , ability = Just Levitate
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 144
      , nationalDexNumber = 110
      , name = "Weezing"
      , form = Original
      , typing = Single Poison
      , ability = Just Levitate
      , evolutionData = EvolvesFrom 143 "Level 35"
      , transformationData = DoesNotTransform
      }
    , { id = 1375
      , nationalDexNumber = 110
      , name = "Weezing"
      , form = Regional 144 Galar
      , typing = Double Poison Fairy
      , ability = Just Levitate
      , evolutionData = EvolvesFrom 143 "Level 35 In Galar"
      , transformationData = DoesNotTransform
      }
    , { id = 145
      , nationalDexNumber = 111
      , name = "Rhyhorn"
      , form = Original
      , typing = Double Ground Rock
      , ability = Just LightningRod
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 146
      , nationalDexNumber = 112
      , name = "Rhydon"
      , form = Original
      , typing = Double Ground Rock
      , ability = Just LightningRod
      , evolutionData = EvolvesFrom 145 "Level 42"
      , transformationData = DoesNotTransform
      }
    , { id = 686
      , nationalDexNumber = 464
      , name = "Rhyperior"
      , form = Original
      , typing = Double Ground Rock
      , ability = Just LightningRod
      , evolutionData = EvolvesFrom 146 "Trade holding Protector"
      , transformationData = DoesNotTransform
      }
    , { id = 638
      , nationalDexNumber = 440
      , name = "Happiny"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 147
      , nationalDexNumber = 113
      , name = "Chansey"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = EvolvesFrom 638 "Level while holding an Oval Stone during the day"
      , transformationData = DoesNotTransform
      }
    , { id = 366
      , nationalDexNumber = 242
      , name = "Blissey"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = EvolvesFrom 147 "Level with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 148
      , nationalDexNumber = 114
      , name = "Tangela"
      , form = Original
      , typing = Single Grass
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 687
      , nationalDexNumber = 465
      , name = "Tangrowth"
      , form = Original
      , typing = Single Grass
      , ability = Nothing
      , evolutionData = EvolvesFrom 148 "Level while knowing Ancient Power"
      , transformationData = DoesNotTransform
      }
    , { id = 211
      , nationalDexNumber = 115
      , name = "Kangaskhan"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1379
      , nationalDexNumber = 115
      , name = "Kangaskhan"
      , form = Mega
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = EvolvesFrom 211 "Holding Kangaskhanite"
      , transformationData = DoesNotTransform
      }
    , { id = 212
      , nationalDexNumber = 116
      , name = "Horsea"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 213
      , nationalDexNumber = 117
      , name = "Seadra"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = EvolvesFrom 212 "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 354
      , nationalDexNumber = 230
      , name = "Kingdra"
      , form = Original
      , typing = Double Water Dragon
      , ability = Nothing
      , evolutionData = EvolvesFrom 213 "Trade holding Dragon Scale"
      , transformationData = DoesNotTransform
      }
    , { id = 214
      , nationalDexNumber = 118
      , name = "Goldeen"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 215
      , nationalDexNumber = 119
      , name = "Seaking"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = EvolvesFrom 214 "Level 33"
      , transformationData = DoesNotTransform
      }
    , { id = 216
      , nationalDexNumber = 120
      , name = "Staryu"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 217
      , nationalDexNumber = 121
      , name = "Starmie"
      , form = Original
      , typing = Double Water Psychic
      , ability = Nothing
      , evolutionData = EvolvesFrom 216 "Use Water Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 637
      , nationalDexNumber = 439
      , name = "Mime Jr."
      , form = Original
      , typing = Double Psychic Fairy
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 218
      , nationalDexNumber = 122
      , name = "Mr. Mime"
      , form = Original
      , typing = Double Psychic Fairy
      , ability = Nothing
      , evolutionData = EvolvesFrom 637 "Level while knowing Mimic"
      , transformationData = DoesNotTransform
      }
    , { id = 1377
      , nationalDexNumber = 122
      , name = "Mr. Mime"
      , form = Regional 218 Galar
      , typing = Double Ice Psychic
      , ability = Nothing
      , evolutionData = EvolvesFrom 637 "Level while knowing Mimic in Galar"
      , transformationData = DoesNotTransform
      }
    , { id = 1376
      , nationalDexNumber = 866
      , name = "Mr. Rime"
      , form = Original
      , typing = Double Ice Psychic
      , ability = Nothing
      , evolutionData = EvolvesFrom 1377 "Level 42"
      , transformationData = DoesNotTransform
      }
    , { id = 219
      , nationalDexNumber = 123
      , name = "Scyther"
      , form = Original
      , typing = Double Bug Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 336
      , nationalDexNumber = 212
      , name = "Scizor"
      , form = Original
      , typing = Double Bug Steel
      , ability = Nothing
      , evolutionData = EvolvesFrom 219 "Trade holding Metal Coat"
      , transformationData = DoesNotTransform
      }
    , { id = 1384
      , nationalDexNumber = 212
      , name = "Scizor"
      , form = Mega
      , typing = Double Bug Steel
      , ability = Nothing
      , evolutionData = EvolvesFrom 336 "Holding Scizorite"
      , transformationData = DoesNotTransform
      }
    , { id = 1610
      , nationalDexNumber = 900
      , name = "Kleavor"
      , form = Original
      , typing = Double Bug Rock
      , ability = Nothing
      , evolutionData = EvolvesFrom 219 "Use Black Augurite"
      , transformationData = DoesNotTransform
      }
    , { id = 362
      , nationalDexNumber = 238
      , name = "Smoochum"
      , form = Original
      , typing = Double Ice Psychic
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 220
      , nationalDexNumber = 124
      , name = "Jynx"
      , form = Original
      , typing = Double Ice Psychic
      , ability = Nothing
      , evolutionData = EvolvesFrom 362 "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 363
      , nationalDexNumber = 239
      , name = "Elekid"
      , form = Original
      , typing = Single Electric
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 221
      , nationalDexNumber = 125
      , name = "Electabuzz"
      , form = Original
      , typing = Single Electric
      , ability = Nothing
      , evolutionData = EvolvesFrom 363 "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 688
      , nationalDexNumber = 466
      , name = "Electivire"
      , form = Original
      , typing = Single Electric
      , ability = Just MotorDrive
      , evolutionData = EvolvesFrom 221 "Trade holding Electirizer"
      , transformationData = DoesNotTransform
      }
    , { id = 364
      , nationalDexNumber = 240
      , name = "Magby"
      , form = Original
      , typing = Single Fire
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 222
      , nationalDexNumber = 126
      , name = "Magmar"
      , form = Original
      , typing = Single Fire
      , ability = Nothing
      , evolutionData = EvolvesFrom 364 "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 699
      , nationalDexNumber = 467
      , name = "Magmortar"
      , form = Original
      , typing = Single Fire
      , ability = Nothing
      , evolutionData = EvolvesFrom 222 "Trade holding Magmarizer"
      , transformationData = DoesNotTransform
      }
    , { id = 223
      , nationalDexNumber = 127
      , name = "Pinsir"
      , form = Original
      , typing = Single Bug
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1380
      , nationalDexNumber = 127
      , name = "Pinsir"
      , form = Mega
      , typing = Double Bug Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 223 "Holding Pinsirite"
      , transformationData = DoesNotTransform
      }
    , { id = 224
      , nationalDexNumber = 128
      , name = "Tauros"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1678
      , nationalDexNumber = 128
      , name = "Tauros"
      , form = UniqueRegional 224 Paldea "Combat"
      , typing = Single Fighting
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1679
      , nationalDexNumber = 128
      , name = "Tauros"
      , form = UniqueRegional 224 Paldea "Aqua"
      , typing = Double Fighting Fire
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1680
      , nationalDexNumber = 128
      , name = "Tauros"
      , form = UniqueRegional 224 Paldea "Blaze"
      , typing = Double Fighting Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 225
      , nationalDexNumber = 129
      , name = "Magikarp"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 226
      , nationalDexNumber = 130
      , name = "Gyarados"
      , form = Original
      , typing = Double Water Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 225 "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 1381
      , nationalDexNumber = 130
      , name = "Gyarados"
      , form = Mega
      , typing = Double Water Dark
      , ability = Nothing
      , evolutionData = EvolvesFrom 226 "Holding Gyaradosite"
      , transformationData = DoesNotTransform
      }
    , { id = 227
      , nationalDexNumber = 131
      , name = "Lapras"
      , form = Original
      , typing = Double Water Ice
      , ability = Just WaterAbsorb
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 29
      , nationalDexNumber = 132
      , name = "Ditto"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 228
      , nationalDexNumber = 133
      , name = "Eevee"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 229
      , nationalDexNumber = 134
      , name = "Vaporeon"
      , form = Original
      , typing = Single Water
      , ability = Just WaterAbsorb
      , evolutionData = EvolvesFrom 228 "Use Water Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 230
      , nationalDexNumber = 135
      , name = "Jolteon"
      , form = Original
      , typing = Single Electric
      , ability = Just VoltAbsorb
      , evolutionData = EvolvesFrom 228 "Use Thunder Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 231
      , nationalDexNumber = 136
      , name = "Flareon"
      , form = Original
      , typing = Single Fire
      , ability = Just FlashFire
      , evolutionData = EvolvesFrom 228 "Use Fire Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 296
      , nationalDexNumber = 196
      , name = "Espeon"
      , form = Original
      , typing = Single Psychic
      , ability = Nothing
      , evolutionData = EvolvesFrom 228 "Level during the day with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 297
      , nationalDexNumber = 197
      , name = "Umbreon"
      , form = Original
      , typing = Single Dark
      , ability = Nothing
      , evolutionData = EvolvesFrom 228 "Level during the night with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 715
      , nationalDexNumber = 470
      , name = "Leafeon"
      , form = Original
      , typing = Single Grass
      , ability = Nothing
      , evolutionData = EvolvesFrom 228 "Use Leaf Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 716
      , nationalDexNumber = 471
      , name = "Glaceon"
      , form = Original
      , typing = Single Ice
      , ability = Nothing
      , evolutionData = EvolvesFrom 228 "Use Ice Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 1129
      , nationalDexNumber = 700
      , name = "Sylveon"
      , form = Original
      , typing = Single Fairy
      , ability = Nothing
      , evolutionData = EvolvesFrom 228 "Level while knowing a Fairy move with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 232
      , nationalDexNumber = 137
      , name = "Porygon"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 357
      , nationalDexNumber = 233
      , name = "Porygon2"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = EvolvesFrom 232 "Trade holding Upgrade"
      , transformationData = DoesNotTransform
      }
    , { id = 719
      , nationalDexNumber = 474
      , name = "Porygon-Z"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = EvolvesFrom 357 "Trade holding Dubious Disc"
      , transformationData = DoesNotTransform
      }
    , { id = 233
      , nationalDexNumber = 138
      , name = "Omanyte"
      , form = Original
      , typing = Double Rock Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 234
      , nationalDexNumber = 139
      , name = "Omastar"
      , form = Original
      , typing = Double Rock Water
      , ability = Nothing
      , evolutionData = EvolvesFrom 233 "Level 40"
      , transformationData = DoesNotTransform
      }
    , { id = 235
      , nationalDexNumber = 140
      , name = "Kabuto"
      , form = Original
      , typing = Double Rock Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 236
      , nationalDexNumber = 141
      , name = "Kabutops"
      , form = Original
      , typing = Double Rock Water
      , ability = Nothing
      , evolutionData = EvolvesFrom 235 "Level 40"
      , transformationData = DoesNotTransform
      }
    , { id = 237
      , nationalDexNumber = 142
      , name = "Aerodactyl"
      , form = Original
      , typing = Double Rock Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1382
      , nationalDexNumber = 142
      , name = "Aerodactyl"
      , form = Mega
      , typing = Double Rock Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 237 "Holding Aerodactylite"
      , transformationData = DoesNotTransform
      }
    , { id = 644
      , nationalDexNumber = 446
      , name = "Munchlax"
      , form = Original
      , typing = Single Normal
      , ability = Just ThickFat
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 238
      , nationalDexNumber = 143
      , name = "Snorlax"
      , form = Original
      , typing = Single Normal
      , ability = Just ThickFat
      , evolutionData = EvolvesFrom 644 "Level with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 239
      , nationalDexNumber = 144
      , name = "Articuno"
      , form = Original
      , typing = Double Ice Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1385
      , nationalDexNumber = 144
      , name = "Articuno"
      , form = Regional 239 Galar
      , typing = Double Psychic Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 240
      , nationalDexNumber = 145
      , name = "Zapdos"
      , form = Original
      , typing = Double Electric Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1386
      , nationalDexNumber = 145
      , name = "Zapdos"
      , form = Regional 240 Galar
      , typing = Double Fighting Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 241
      , nationalDexNumber = 146
      , name = "Moltres"
      , form = Original
      , typing = Double Fire Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1387
      , nationalDexNumber = 146
      , name = "Moltres"
      , form = Regional 241 Galar
      , typing = Double Dark Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 242
      , nationalDexNumber = 147
      , name = "Dratini"
      , form = Original
      , typing = Single Dragon
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 243
      , nationalDexNumber = 148
      , name = "Dragonair"
      , form = Original
      , typing = Single Dragon
      , ability = Nothing
      , evolutionData = EvolvesFrom 242 "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 244
      , nationalDexNumber = 149
      , name = "Dragonite"
      , form = Original
      , typing = Double Dragon Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 243 "Level 55"
      , transformationData = DoesNotTransform
      }
    , { id = 43
      , nationalDexNumber = 150
      , name = "Mewtwo"
      , form = Original
      , typing = Single Psychic
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 44
      , nationalDexNumber = 150
      , name = "Mewtwo"
      , form = MegaX
      , typing = Double Psychic Fighting
      , ability = Nothing
      , evolutionData = EvolvesFrom 43 "Holding Mewtwonite X"
      , transformationData = DoesNotTransform
      }
    , { id = 45
      , nationalDexNumber = 150
      , name = "Mewtwo"
      , form = MegaY
      , typing = Single Psychic
      , ability = Nothing
      , evolutionData = EvolvesFrom 43 "Holding Mewtwonite Y"
      , transformationData = DoesNotTransform
      }
    , { id = 42
      , nationalDexNumber = 151
      , name = "Mew"
      , form = Original
      , typing = Single Psychic
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 245
      , nationalDexNumber = 152
      , name = "Chikorita"
      , form = Original
      , typing = Single Grass
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 246
      , nationalDexNumber = 153
      , name = "Bayleef"
      , form = Original
      , typing = Single Grass
      , ability = Nothing
      , evolutionData = EvolvesFrom 245 "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 247
      , nationalDexNumber = 154
      , name = "Meganium"
      , form = Original
      , typing = Single Grass
      , ability = Nothing
      , evolutionData = EvolvesFrom 246 "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 248
      , nationalDexNumber = 155
      , name = "Cyndaquil"
      , form = Original
      , typing = Single Fire
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 249
      , nationalDexNumber = 156
      , name = "Quilava"
      , form = Original
      , typing = Single Fire
      , ability = Nothing
      , evolutionData = EvolvesFrom 248 "Level 14 (17 in Legends: Arceus)"
      , transformationData = DoesNotTransform
      }
    , { id = 250
      , nationalDexNumber = 157
      , name = "Typhlosion"
      , form = Original
      , typing = Single Fire
      , ability = Nothing
      , evolutionData = EvolvesFrom 249 "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 1623
      , nationalDexNumber = 157
      , name = "Typhlosion"
      , form = Regional 250 Hisui
      , typing = Double Fire Ghost
      , ability = Nothing
      , evolutionData = EvolvesFrom 249 "Level 36 in Legends: Arceus"
      , transformationData = DoesNotTransform
      }
    , { id = 251
      , nationalDexNumber = 158
      , name = "Totodile"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 252
      , nationalDexNumber = 159
      , name = "Croconaw"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = EvolvesFrom 251 "Level 18"
      , transformationData = DoesNotTransform
      }
    , { id = 253
      , nationalDexNumber = 160
      , name = "Feraligatr"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = EvolvesFrom 252 "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 254
      , nationalDexNumber = 161
      , name = "Sentret"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 255
      , nationalDexNumber = 162
      , name = "Furret"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = EvolvesFrom 254 "Level 15"
      , transformationData = DoesNotTransform
      }
    , { id = 256
      , nationalDexNumber = 163
      , name = "Hoothoot"
      , form = Original
      , typing = Double Normal Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 257
      , nationalDexNumber = 164
      , name = "Noctowl"
      , form = Original
      , typing = Double Normal Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 256 "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 258
      , nationalDexNumber = 165
      , name = "Ledyba"
      , form = Original
      , typing = Double Bug Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 259
      , nationalDexNumber = 166
      , name = "Ledian"
      , form = Original
      , typing = Double Bug Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 258 "Level 18"
      , transformationData = DoesNotTransform
      }
    , { id = 260
      , nationalDexNumber = 167
      , name = "Spinarak"
      , form = Original
      , typing = Double Bug Poison
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 261
      , nationalDexNumber = 168
      , name = "Ariados"
      , form = Original
      , typing = Double Bug Poison
      , ability = Nothing
      , evolutionData = EvolvesFrom 260 "Level 22"
      , transformationData = DoesNotTransform
      }
    , { id = 262
      , nationalDexNumber = 170
      , name = "Chinchou"
      , form = Original
      , typing = Double Water Electric
      , ability = Just VoltAbsorb
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 263
      , nationalDexNumber = 171
      , name = "Lanturn"
      , form = Original
      , typing = Double Water Electric
      , ability = Just VoltAbsorb
      , evolutionData = EvolvesFrom 262 "Level 27"
      , transformationData = DoesNotTransform
      }
    , { id = 264
      , nationalDexNumber = 175
      , name = "Togepi"
      , form = Original
      , typing = Single Fairy
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 265
      , nationalDexNumber = 176
      , name = "Togetic"
      , form = Original
      , typing = Double Fairy Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 264 "Level with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 713
      , nationalDexNumber = 468
      , name = "Togekiss"
      , form = Original
      , typing = Double Fairy Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 265 "Use Shiny Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 266
      , nationalDexNumber = 177
      , name = "Natu"
      , form = Original
      , typing = Double Psychic Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 267
      , nationalDexNumber = 178
      , name = "Xatu"
      , form = Original
      , typing = Double Psychic Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 266 "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 278
      , nationalDexNumber = 179
      , name = "Mareep"
      , form = Original
      , typing = Single Electric
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 279
      , nationalDexNumber = 180
      , name = "Flaaffy"
      , form = Original
      , typing = Single Electric
      , ability = Nothing
      , evolutionData = EvolvesFrom 278 "Level 15"
      , transformationData = DoesNotTransform
      }
    , { id = 280
      , nationalDexNumber = 181
      , name = "Ampharos"
      , form = Original
      , typing = Single Electric
      , ability = Nothing
      , evolutionData = EvolvesFrom 279 "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 1394
      , nationalDexNumber = 181
      , name = "Ampharos"
      , form = Mega
      , typing = Double Electric Dragon
      , ability = Nothing
      , evolutionData = EvolvesFrom 280 "Holding Ampharosite"
      , transformationData = DoesNotTransform
      }
    , { id = 426
      , nationalDexNumber = 298
      , name = "Azurill"
      , form = Original
      , typing = Double Normal Fairy
      , ability = Just ThickFat
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 283
      , nationalDexNumber = 183
      , name = "Marill"
      , form = Original
      , typing = Double Water Fairy
      , ability = Just ThickFat
      , evolutionData = EvolvesFrom 426 "Level with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 284
      , nationalDexNumber = 184
      , name = "Azumarill"
      , form = Original
      , typing = Double Water Fairy
      , ability = Just ThickFat
      , evolutionData = EvolvesFrom 283 "Level 18"
      , transformationData = DoesNotTransform
      }
    , { id = 636
      , nationalDexNumber = 438
      , name = "Bonsly"
      , form = Original
      , typing = Single Rock
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 285
      , nationalDexNumber = 185
      , name = "Sudowoodo"
      , form = Original
      , typing = Single Rock
      , ability = Nothing
      , evolutionData = EvolvesFrom 636 "Level while knowing Mimic"
      , transformationData = DoesNotTransform
      }
    , { id = 287
      , nationalDexNumber = 187
      , name = "Hoppip"
      , form = Original
      , typing = Double Grass Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 288
      , nationalDexNumber = 188
      , name = "Skiploom"
      , form = Original
      , typing = Double Grass Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 287 "Level 18"
      , transformationData = DoesNotTransform
      }
    , { id = 289
      , nationalDexNumber = 189
      , name = "Jumpluff"
      , form = Original
      , typing = Double Grass Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 288 "Level 27"
      , transformationData = DoesNotTransform
      }
    , { id = 290
      , nationalDexNumber = 190
      , name = "Aipom"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 622
      , nationalDexNumber = 424
      , name = "Ambipom"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = EvolvesFrom 290 "Level while knowing Double Hit"
      , transformationData = DoesNotTransform
      }
    , { id = 291
      , nationalDexNumber = 191
      , name = "Sunkern"
      , form = Original
      , typing = Single Grass
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 292
      , nationalDexNumber = 192
      , name = "Sunflora"
      , form = Original
      , typing = Single Grass
      , ability = Nothing
      , evolutionData = EvolvesFrom 291 "Use Sun Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 293
      , nationalDexNumber = 193
      , name = "Yanma"
      , form = Original
      , typing = Double Bug Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 714
      , nationalDexNumber = 469
      , name = "Yanmega"
      , form = Original
      , typing = Double Bug Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 293 "Level while knowing Ancient Power"
      , transformationData = DoesNotTransform
      }
    , { id = 294
      , nationalDexNumber = 194
      , name = "Wooper"
      , form = Original
      , typing = Double Water Ground
      , ability = Just WaterAbsorb
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 295
      , nationalDexNumber = 195
      , name = "Quagsire"
      , form = Original
      , typing = Double Water Ground
      , ability = Just WaterAbsorb
      , evolutionData = EvolvesFrom 294 "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 1651
      , nationalDexNumber = 194
      , name = "Wooper"
      , form = Regional 294 Paldea
      , typing = Double Poison Ground
      , ability = Just WaterAbsorb
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1652
      , nationalDexNumber = 980
      , name = "Clodsire"
      , form = Original
      , typing = Double Poison Ground
      , ability = Just WaterAbsorb
      , evolutionData = EvolvesFrom 1651 "Level 28"
      , transformationData = DoesNotTransform
      }
    , { id = 298
      , nationalDexNumber = 198
      , name = "Murkrow"
      , form = Original
      , typing = Double Dark Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 628
      , nationalDexNumber = 430
      , name = "Honchkrow"
      , form = Original
      , typing = Double Dark Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 298 "Use Dusk Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 324
      , nationalDexNumber = 200
      , name = "Misdreavus"
      , form = Original
      , typing = Single Ghost
      , ability = Just Levitate
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 627
      , nationalDexNumber = 429
      , name = "Mismagius"
      , form = Original
      , typing = Single Ghost
      , ability = Just Levitate
      , evolutionData = EvolvesFrom 324 "Use Dusk Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 325
      , nationalDexNumber = 201
      , name = "Unown"
      , form = Original
      , typing = Single Psychic
      , ability = Just Levitate
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 488
      , nationalDexNumber = 360
      , name = "Wynaut"
      , form = Original
      , typing = Single Psychic
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 326
      , nationalDexNumber = 202
      , name = "Wobbuffet"
      , form = Original
      , typing = Single Psychic
      , ability = Nothing
      , evolutionData = EvolvesFrom 488 "Level 15"
      , transformationData = DoesNotTransform
      }
    , { id = 327
      , nationalDexNumber = 203
      , name = "Girafarig"
      , form = Original
      , typing = Double Normal Psychic
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1673
      , nationalDexNumber = 981
      , name = "Farigiraf"
      , form = Original
      , typing = Double Normal Psychic
      , ability = Nothing
      , evolutionData = EvolvesFrom 327 "Level while knowing Twin Beam"
      , transformationData = DoesNotTransform
      }
    , { id = 328
      , nationalDexNumber = 204
      , name = "Pineco"
      , form = Original
      , typing = Single Bug
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 329
      , nationalDexNumber = 205
      , name = "Forretress"
      , form = Original
      , typing = Double Bug Steel
      , ability = Nothing
      , evolutionData = EvolvesFrom 328 "Level 31"
      , transformationData = DoesNotTransform
      }
    , { id = 330
      , nationalDexNumber = 206
      , name = "Dunsparce"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1672
      , nationalDexNumber = 982
      , name = "Dudunsparce"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = EvolvesFrom 330 "Level while knowing Hyper Drill"
      , transformationData = DoesNotTransform
      }
    , { id = 331
      , nationalDexNumber = 207
      , name = "Gligar"
      , form = Original
      , typing = Double Ground Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 717
      , nationalDexNumber = 472
      , name = "Gliscor"
      , form = Original
      , typing = Double Ground Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 331 "Level while holding Razor Fang at night"
      , transformationData = DoesNotTransform
      }
    , { id = 333
      , nationalDexNumber = 209
      , name = "Snubbull"
      , form = Original
      , typing = Single Fairy
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 334
      , nationalDexNumber = 210
      , name = "Granbull"
      , form = Original
      , typing = Single Fairy
      , ability = Nothing
      , evolutionData = EvolvesFrom 333 "Level 23"
      , transformationData = DoesNotTransform
      }
    , { id = 335
      , nationalDexNumber = 211
      , name = "Qwilfish"
      , form = Original
      , typing = Double Water Poison
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1615
      , nationalDexNumber = 211
      , name = "Qwilfish"
      , form = Regional 335 Hisui
      , typing = Double Dark Poison
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1616
      , nationalDexNumber = 904
      , name = "Overqwil"
      , form = Original
      , typing = Double Dark Poison
      , ability = Nothing
      , evolutionData = EvolvesFrom 1615 "Use Barb Barrage in Strong Style 20 times"
      , transformationData = DoesNotTransform
      }
    , { id = 337
      , nationalDexNumber = 213
      , name = "Shuckle"
      , form = Original
      , typing = Double Bug Rock
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 338
      , nationalDexNumber = 214
      , name = "Heracross"
      , form = Original
      , typing = Double Bug Fighting
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1395
      , nationalDexNumber = 214
      , name = "Heracross"
      , form = Mega
      , typing = Double Bug Fighting
      , ability = Nothing
      , evolutionData = EvolvesFrom 338 "Holding Heracronite"
      , transformationData = DoesNotTransform
      }
    , { id = 339
      , nationalDexNumber = 215
      , name = "Sneasel"
      , form = Original
      , typing = Double Dark Ice
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 683
      , nationalDexNumber = 461
      , name = "Weavile"
      , form = Original
      , typing = Double Dark Ice
      , ability = Nothing
      , evolutionData = EvolvesFrom 339 "Level while holding Razor Claw at night"
      , transformationData = DoesNotTransform
      }
    , { id = 1613
      , nationalDexNumber = 215
      , name = "Sneasel"
      , form = Regional 339 Hisui
      , typing = Double Fighting Poison
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1614
      , nationalDexNumber = 903
      , name = "Sneasler"
      , form = Original
      , typing = Double Fighting Poison
      , ability = Nothing
      , evolutionData = EvolvesFrom 1613 "Use Razor Claw during the day"
      , transformationData = DoesNotTransform
      }
    , { id = 340
      , nationalDexNumber = 216
      , name = "Teddiursa"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 341
      , nationalDexNumber = 217
      , name = "Ursaring"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = EvolvesFrom 340 "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 1611
      , nationalDexNumber = 901
      , name = "Ursaluna"
      , form = Original
      , typing = Double Normal Ground
      , ability = Nothing
      , evolutionData = EvolvesFrom 341 "Use Peat Block under a full moon"
      , transformationData = DoesNotTransform
      }
    , { id = 342
      , nationalDexNumber = 218
      , name = "Slugma"
      , form = Original
      , typing = Single Fire
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 343
      , nationalDexNumber = 219
      , name = "Magcargo"
      , form = Original
      , typing = Double Fire Rock
      , ability = Nothing
      , evolutionData = EvolvesFrom 342 "Level 38"
      , transformationData = DoesNotTransform
      }
    , { id = 344
      , nationalDexNumber = 220
      , name = "Swinub"
      , form = Original
      , typing = Double Ice Ground
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 345
      , nationalDexNumber = 221
      , name = "Piloswine"
      , form = Original
      , typing = Double Ice Ground
      , ability = Nothing
      , evolutionData = EvolvesFrom 344 "Level 33"
      , transformationData = DoesNotTransform
      }
    , { id = 718
      , nationalDexNumber = 473
      , name = "Mamoswine"
      , form = Original
      , typing = Double Ice Ground
      , ability = Nothing
      , evolutionData = EvolvesFrom 345 "Level while knowing Ancient Power"
      , transformationData = DoesNotTransform
      }
    , { id = 346
      , nationalDexNumber = 222
      , name = "Corsola"
      , form = Original
      , typing = Double Water Rock
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1389
      , nationalDexNumber = 222
      , name = "Corsola"
      , form = Regional 346 Galar
      , typing = Single Ghost
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1390
      , nationalDexNumber = 864
      , name = "Cursola"
      , form = Original
      , typing = Single Ghost
      , ability = Nothing
      , evolutionData = EvolvesFrom 1389 "Level 38"
      , transformationData = DoesNotTransform
      }
    , { id = 347
      , nationalDexNumber = 223
      , name = "Remoraid"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 348
      , nationalDexNumber = 224
      , name = "Octillery"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = EvolvesFrom 347 "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 349
      , nationalDexNumber = 225
      , name = "Delibird"
      , form = Original
      , typing = Double Ice Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 656
      , nationalDexNumber = 458
      , name = "Mantyke"
      , form = Original
      , typing = Double Water Flying
      , ability = Just WaterAbsorb
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 350
      , nationalDexNumber = 226
      , name = "Mantine"
      , form = Original
      , typing = Double Water Flying
      , ability = Just WaterAbsorb
      , evolutionData = EvolvesFrom 656 "Level with Remoraid in party"
      , transformationData = DoesNotTransform
      }
    , { id = 351
      , nationalDexNumber = 227
      , name = "Skarmory"
      , form = Original
      , typing = Double Steel Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 352
      , nationalDexNumber = 228
      , name = "Houndour"
      , form = Original
      , typing = Double Dark Fire
      , ability = Just FlashFire
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 353
      , nationalDexNumber = 229
      , name = "Houndoom"
      , form = Original
      , typing = Double Dark Fire
      , ability = Just FlashFire
      , evolutionData = EvolvesFrom 352 "Level 24"
      , transformationData = DoesNotTransform
      }
    , { id = 1396
      , nationalDexNumber = 229
      , name = "Houndoom"
      , form = Mega
      , typing = Double Dark Fire
      , ability = Just FlashFire
      , evolutionData = EvolvesFrom 353 "Holding Houndoominite"
      , transformationData = DoesNotTransform
      }
    , { id = 355
      , nationalDexNumber = 231
      , name = "Phanpy"
      , form = Original
      , typing = Single Ground
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 356
      , nationalDexNumber = 232
      , name = "Donphan"
      , form = Original
      , typing = Single Ground
      , ability = Nothing
      , evolutionData = EvolvesFrom 355 "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 358
      , nationalDexNumber = 234
      , name = "Stantler"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1609
      , nationalDexNumber = 899
      , name = "Wyrdeer"
      , form = Original
      , typing = Double Normal Psychic
      , ability = Nothing
      , evolutionData = EvolvesFrom 358 "Use Psyshield Bash 20 times in Agile Style"
      , transformationData = DoesNotTransform
      }
    , { id = 359
      , nationalDexNumber = 235
      , name = "Smeargle"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 365
      , nationalDexNumber = 241
      , name = "Miltank"
      , form = Original
      , typing = Single Normal
      , ability = Just ThickFat
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 367
      , nationalDexNumber = 243
      , name = "Raikou"
      , form = Original
      , typing = Single Electric
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 368
      , nationalDexNumber = 244
      , name = "Entei"
      , form = Original
      , typing = Single Fire
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 369
      , nationalDexNumber = 245
      , name = "Suicune"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 370
      , nationalDexNumber = 246
      , name = "Larvitar"
      , form = Original
      , typing = Double Rock Ground
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 371
      , nationalDexNumber = 247
      , name = "Pupitar"
      , form = Original
      , typing = Double Rock Ground
      , ability = Nothing
      , evolutionData = EvolvesFrom 370 "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 372
      , nationalDexNumber = 248
      , name = "Tyranitar"
      , form = Original
      , typing = Double Rock Dark
      , ability = Nothing
      , evolutionData = EvolvesFrom 371 "Level 55"
      , transformationData = DoesNotTransform
      }
    , { id = 1397
      , nationalDexNumber = 248
      , name = "Tyranitar"
      , form = Mega
      , typing = Double Rock Dark
      , ability = Nothing
      , evolutionData = EvolvesFrom 372 "Holding Tyranitarite"
      , transformationData = DoesNotTransform
      }
    , { id = 373
      , nationalDexNumber = 249
      , name = "Lugia"
      , form = Original
      , typing = Double Psychic Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 374
      , nationalDexNumber = 250
      , name = "Ho-Oh"
      , form = Original
      , typing = Double Fire Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 375
      , nationalDexNumber = 251
      , name = "Celebi"
      , form = Original
      , typing = Double Psychic Grass
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 380
      , nationalDexNumber = 252
      , name = "Treecko"
      , form = Original
      , typing = Single Grass
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 381
      , nationalDexNumber = 253
      , name = "Grovyle"
      , form = Original
      , typing = Single Grass
      , ability = Nothing
      , evolutionData = EvolvesFrom 380 "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 382
      , nationalDexNumber = 254
      , name = "Sceptile"
      , form = Original
      , typing = Single Grass
      , ability = Nothing
      , evolutionData = EvolvesFrom 381 "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 1428
      , nationalDexNumber = 254
      , name = "Sceptile"
      , form = Mega
      , typing = Double Grass Dragon
      , ability = Just LightningRod
      , evolutionData = EvolvesFrom 382 "Holding Sceptilite"
      , transformationData = DoesNotTransform
      }
    , { id = 383
      , nationalDexNumber = 255
      , name = "Torchic"
      , form = Original
      , typing = Single Fire
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 384
      , nationalDexNumber = 256
      , name = "Combusken"
      , form = Original
      , typing = Double Fire Fighting
      , ability = Nothing
      , evolutionData = EvolvesFrom 383 "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 385
      , nationalDexNumber = 257
      , name = "Blaziken"
      , form = Original
      , typing = Double Fire Fighting
      , ability = Nothing
      , evolutionData = EvolvesFrom 384 "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 1429
      , nationalDexNumber = 257
      , name = "Blaziken"
      , form = Mega
      , typing = Double Fire Fighting
      , ability = Nothing
      , evolutionData = EvolvesFrom 385 "Holding Blazikenite"
      , transformationData = DoesNotTransform
      }
    , { id = 386
      , nationalDexNumber = 258
      , name = "Mudkip"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 387
      , nationalDexNumber = 259
      , name = "Marshtomp"
      , form = Original
      , typing = Double Water Ground
      , ability = Nothing
      , evolutionData = EvolvesFrom 386 "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 388
      , nationalDexNumber = 260
      , name = "Swampert"
      , form = Original
      , typing = Double Water Ground
      , ability = Nothing
      , evolutionData = EvolvesFrom 387 "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 1430
      , nationalDexNumber = 260
      , name = "Swampert"
      , form = Mega
      , typing = Double Water Ground
      , ability = Nothing
      , evolutionData = EvolvesFrom 388 "Holding Swampertite"
      , transformationData = DoesNotTransform
      }
    , { id = 389
      , nationalDexNumber = 261
      , name = "Poochyena"
      , form = Original
      , typing = Single Dark
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 390
      , nationalDexNumber = 262
      , name = "Mightyena"
      , form = Original
      , typing = Single Dark
      , ability = Nothing
      , evolutionData = EvolvesFrom 389 "Level 18"
      , transformationData = DoesNotTransform
      }
    , { id = 391
      , nationalDexNumber = 263
      , name = "Zigzagoon"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 392
      , nationalDexNumber = 264
      , name = "Linoone"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = EvolvesFrom 391 "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 1466
      , nationalDexNumber = 263
      , name = "Zigzagoon"
      , form = Regional 391 Galar
      , typing = Double Dark Normal
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1467
      , nationalDexNumber = 264
      , name = "Linoone"
      , form = Regional 392 Galar
      , typing = Double Dark Normal
      , ability = Nothing
      , evolutionData = EvolvesFrom 1466 "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 1468
      , nationalDexNumber = 862
      , name = "Obstagoon"
      , form = Original
      , typing = Double Dark Normal
      , ability = Nothing
      , evolutionData = EvolvesFrom 1467 "Level 35 at night"
      , transformationData = DoesNotTransform
      }
    , { id = 393
      , nationalDexNumber = 265
      , name = "Wurmple"
      , form = Original
      , typing = Single Bug
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 394
      , nationalDexNumber = 266
      , name = "Silcoon"
      , form = Original
      , typing = Single Bug
      , ability = Nothing
      , evolutionData = EvolvesFrom 393 "Level 7 (random)"
      , transformationData = DoesNotTransform
      }
    , { id = 395
      , nationalDexNumber = 267
      , name = "Beautifly"
      , form = Original
      , typing = Double Bug Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 394 "Level 10"
      , transformationData = DoesNotTransform
      }
    , { id = 396
      , nationalDexNumber = 268
      , name = "Cascoon"
      , form = Original
      , typing = Single Bug
      , ability = Nothing
      , evolutionData = EvolvesFrom 393 "Level 7 (random)"
      , transformationData = DoesNotTransform
      }
    , { id = 397
      , nationalDexNumber = 269
      , name = "Dustox"
      , form = Original
      , typing = Double Bug Poison
      , ability = Nothing
      , evolutionData = EvolvesFrom 396 "Level 10"
      , transformationData = DoesNotTransform
      }
    , { id = 398
      , nationalDexNumber = 270
      , name = "Lotad"
      , form = Original
      , typing = Double Water Grass
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 399
      , nationalDexNumber = 271
      , name = "Lombre"
      , form = Original
      , typing = Double Water Grass
      , ability = Nothing
      , evolutionData = EvolvesFrom 398 "Level 14"
      , transformationData = DoesNotTransform
      }
    , { id = 400
      , nationalDexNumber = 272
      , name = "Ludicolo"
      , form = Original
      , typing = Double Water Grass
      , ability = Nothing
      , evolutionData = EvolvesFrom 399 "Use Water Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 401
      , nationalDexNumber = 273
      , name = "Seedot"
      , form = Original
      , typing = Single Grass
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 402
      , nationalDexNumber = 274
      , name = "Nuzleaf"
      , form = Original
      , typing = Double Grass Dark
      , ability = Nothing
      , evolutionData = EvolvesFrom 401 "Level 14"
      , transformationData = DoesNotTransform
      }
    , { id = 403
      , nationalDexNumber = 275
      , name = "Shiftry"
      , form = Original
      , typing = Double Grass Dark
      , ability = Nothing
      , evolutionData = EvolvesFrom 402 "Use Leaf Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 404
      , nationalDexNumber = 276
      , name = "Taillow"
      , form = Original
      , typing = Double Normal Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 405
      , nationalDexNumber = 277
      , name = "Swellow"
      , form = Original
      , typing = Double Normal Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 404 "Level 22"
      , transformationData = DoesNotTransform
      }
    , { id = 406
      , nationalDexNumber = 278
      , name = "Wingull"
      , form = Original
      , typing = Double Water Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 407
      , nationalDexNumber = 279
      , name = "Pelipper"
      , form = Original
      , typing = Double Water Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 406 "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 408
      , nationalDexNumber = 280
      , name = "Ralts"
      , form = Original
      , typing = Double Psychic Fairy
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 409
      , nationalDexNumber = 281
      , name = "Kirlia"
      , form = Original
      , typing = Double Psychic Fairy
      , ability = Nothing
      , evolutionData = EvolvesFrom 408 "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 410
      , nationalDexNumber = 282
      , name = "Gardevoir"
      , form = Original
      , typing = Double Psychic Fairy
      , ability = Nothing
      , evolutionData = EvolvesFrom 409 "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 1435
      , nationalDexNumber = 282
      , name = "Gardevoir"
      , form = Mega
      , typing = Double Psychic Fairy
      , ability = Nothing
      , evolutionData = EvolvesFrom 410 "Holding Gardevoirite"
      , transformationData = DoesNotTransform
      }
    , { id = 720
      , nationalDexNumber = 475
      , name = "Gallade"
      , form = Original
      , typing = Double Psychic Fighting
      , ability = Nothing
      , evolutionData = EvolvesFrom 409 "Use Dawn Stone when male"
      , transformationData = DoesNotTransform
      }
    , { id = 1436
      , nationalDexNumber = 475
      , name = "Gallade"
      , form = Mega
      , typing = Double Psychic Fighting
      , ability = Nothing
      , evolutionData = EvolvesFrom 720 "Holding Galladite"
      , transformationData = DoesNotTransform
      }
    , { id = 411
      , nationalDexNumber = 283
      , name = "Surskit"
      , form = Original
      , typing = Double Bug Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 412
      , nationalDexNumber = 284
      , name = "Masquerain"
      , form = Original
      , typing = Double Bug Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 411 "Level 22"
      , transformationData = DoesNotTransform
      }
    , { id = 413
      , nationalDexNumber = 285
      , name = "Shroomish"
      , form = Original
      , typing = Single Grass
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 414
      , nationalDexNumber = 286
      , name = "Breloom"
      , form = Original
      , typing = Double Grass Fighting
      , ability = Nothing
      , evolutionData = EvolvesFrom 413 "Level 23"
      , transformationData = DoesNotTransform
      }
    , { id = 415
      , nationalDexNumber = 287
      , name = "Slakoth"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 416
      , nationalDexNumber = 288
      , name = "Vigoroth"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = EvolvesFrom 415 "Level 18"
      , transformationData = DoesNotTransform
      }
    , { id = 417
      , nationalDexNumber = 289
      , name = "Slaking"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = EvolvesFrom 416 "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 418
      , nationalDexNumber = 290
      , name = "Nincada"
      , form = Original
      , typing = Double Bug Ground
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 419
      , nationalDexNumber = 291
      , name = "Ninjask"
      , form = Original
      , typing = Double Bug Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 418 "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 420
      , nationalDexNumber = 292
      , name = "Shedinja"
      , form = Original
      , typing = Double Bug Ghost
      , ability = Just WonderGuard
      , evolutionData = EvolvesFrom 418 "Evolve while having a Pokeball and an empty space in party"
      , transformationData = DoesNotTransform
      }
    , { id = 421
      , nationalDexNumber = 293
      , name = "Whismur"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 422
      , nationalDexNumber = 294
      , name = "Loudred"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = EvolvesFrom 421 "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 423
      , nationalDexNumber = 295
      , name = "Exploud"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = EvolvesFrom 422 "Level 40"
      , transformationData = DoesNotTransform
      }
    , { id = 424
      , nationalDexNumber = 296
      , name = "Makuhita"
      , form = Original
      , typing = Single Fighting
      , ability = Just ThickFat
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 425
      , nationalDexNumber = 297
      , name = "Hariyama"
      , form = Original
      , typing = Single Fighting
      , ability = Just ThickFat
      , evolutionData = EvolvesFrom 424 "Level 24"
      , transformationData = DoesNotTransform
      }
    , { id = 427
      , nationalDexNumber = 299
      , name = "Nosepass"
      , form = Original
      , typing = Single Rock
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 726
      , nationalDexNumber = 476
      , name = "Probopass"
      , form = Original
      , typing = Double Rock Steel
      , ability = Nothing
      , evolutionData = EvolvesFrom 427 "Level near a Special Magnetic Field"
      , transformationData = DoesNotTransform
      }
    , { id = 428
      , nationalDexNumber = 300
      , name = "Skitty"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 429
      , nationalDexNumber = 301
      , name = "Delcatty"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = EvolvesFrom 428 "Use Moon Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 430
      , nationalDexNumber = 302
      , name = "Sableye"
      , form = Original
      , typing = Double Dark Ghost
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1558
      , nationalDexNumber = 302
      , name = "Sableye"
      , form = Mega
      , typing = Double Dark Ghost
      , ability = Nothing
      , evolutionData = EvolvesFrom 430 "Holding Sablenite"
      , transformationData = DoesNotTransform
      }
    , { id = 431
      , nationalDexNumber = 303
      , name = "Mawile"
      , form = Original
      , typing = Double Steel Fairy
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1559
      , nationalDexNumber = 303
      , name = "Mawile"
      , form = Mega
      , typing = Double Steel Fairy
      , ability = Nothing
      , evolutionData = EvolvesFrom 431 "Holding Mawilite"
      , transformationData = DoesNotTransform
      }
    , { id = 432
      , nationalDexNumber = 304
      , name = "Aron"
      , form = Original
      , typing = Double Steel Rock
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 433
      , nationalDexNumber = 305
      , name = "Lairon"
      , form = Original
      , typing = Double Steel Rock
      , ability = Nothing
      , evolutionData = EvolvesFrom 432 "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 434
      , nationalDexNumber = 306
      , name = "Aggron"
      , form = Original
      , typing = Double Steel Rock
      , ability = Nothing
      , evolutionData = EvolvesFrom 433 "Level 42"
      , transformationData = DoesNotTransform
      }
    , { id = 1434
      , nationalDexNumber = 306
      , name = "Aggron"
      , form = Mega
      , typing = Single Steel
      , ability = Nothing
      , evolutionData = EvolvesFrom 434 "Holding Aggronite"
      , transformationData = DoesNotTransform
      }
    , { id = 435
      , nationalDexNumber = 307
      , name = "Meditite"
      , form = Original
      , typing = Double Fighting Psychic
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 436
      , nationalDexNumber = 308
      , name = "Medicham"
      , form = Original
      , typing = Double Fighting Psychic
      , ability = Nothing
      , evolutionData = EvolvesFrom 435 "Level 37"
      , transformationData = DoesNotTransform
      }
    , { id = 1590
      , nationalDexNumber = 308
      , name = "Medicham"
      , form = Mega
      , typing = Double Fighting Psychic
      , ability = Nothing
      , evolutionData = EvolvesFrom 436 "Holding Medichamite"
      , transformationData = DoesNotTransform
      }
    , { id = 437
      , nationalDexNumber = 309
      , name = "Electrike"
      , form = Original
      , typing = Single Electric
      , ability = Just LightningRod
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 438
      , nationalDexNumber = 310
      , name = "Manectric"
      , form = Original
      , typing = Single Electric
      , ability = Just LightningRod
      , evolutionData = EvolvesFrom 437 "Level 26"
      , transformationData = DoesNotTransform
      }
    , { id = 1437
      , nationalDexNumber = 310
      , name = "Manectric"
      , form = Mega
      , typing = Single Electric
      , ability = Nothing
      , evolutionData = EvolvesFrom 438 "Holding Manectite"
      , transformationData = DoesNotTransform
      }
    , { id = 439
      , nationalDexNumber = 311
      , name = "Plusle"
      , form = Original
      , typing = Single Electric
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 440
      , nationalDexNumber = 312
      , name = "Minun"
      , form = Original
      , typing = Single Electric
      , ability = Just VoltAbsorb
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 441
      , nationalDexNumber = 313
      , name = "Volbeat"
      , form = Original
      , typing = Single Bug
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 442
      , nationalDexNumber = 314
      , name = "Illumise"
      , form = Original
      , typing = Single Bug
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 604
      , nationalDexNumber = 406
      , name = "Budew"
      , form = Original
      , typing = Double Grass Poison
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 443
      , nationalDexNumber = 315
      , name = "Roselia"
      , form = Original
      , typing = Double Grass Poison
      , ability = Nothing
      , evolutionData = EvolvesFrom 604 "Level during the day with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 605
      , nationalDexNumber = 407
      , name = "Roserade"
      , form = Original
      , typing = Double Grass Poison
      , ability = Nothing
      , evolutionData = EvolvesFrom 443 "Use Shiny Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 444
      , nationalDexNumber = 316
      , name = "Gulpin"
      , form = Original
      , typing = Single Poison
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 445
      , nationalDexNumber = 317
      , name = "Swalot"
      , form = Original
      , typing = Single Poison
      , ability = Nothing
      , evolutionData = EvolvesFrom 444 "Level 26"
      , transformationData = DoesNotTransform
      }
    , { id = 446
      , nationalDexNumber = 318
      , name = "Carvanha"
      , form = Original
      , typing = Double Water Dark
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 447
      , nationalDexNumber = 319
      , name = "Sharpedo"
      , form = Original
      , typing = Double Water Dark
      , ability = Nothing
      , evolutionData = EvolvesFrom 446 "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 1587
      , nationalDexNumber = 319
      , name = "Sharpedo"
      , form = Mega
      , typing = Double Water Dark
      , ability = Nothing
      , evolutionData = EvolvesFrom 447 "Holding Sharpedonite"
      , transformationData = DoesNotTransform
      }
    , { id = 448
      , nationalDexNumber = 320
      , name = "Wailmer"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 449
      , nationalDexNumber = 321
      , name = "Wailord"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = EvolvesFrom 448 "Level 40"
      , transformationData = DoesNotTransform
      }
    , { id = 450
      , nationalDexNumber = 322
      , name = "Numel"
      , form = Original
      , typing = Double Fire Ground
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 451
      , nationalDexNumber = 323
      , name = "Camerupt"
      , form = Original
      , typing = Double Fire Ground
      , ability = Nothing
      , evolutionData = EvolvesFrom 450 "Level 33"
      , transformationData = DoesNotTransform
      }
    , { id = 1588
      , nationalDexNumber = 323
      , name = "Camerupt"
      , form = Mega
      , typing = Double Fire Ground
      , ability = Nothing
      , evolutionData = EvolvesFrom 451 "Holding Cameruptite"
      , transformationData = DoesNotTransform
      }
    , { id = 452
      , nationalDexNumber = 324
      , name = "Torkoal"
      , form = Original
      , typing = Single Fire
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 453
      , nationalDexNumber = 325
      , name = "Spoink"
      , form = Original
      , typing = Single Psychic
      , ability = Just ThickFat
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 454
      , nationalDexNumber = 326
      , name = "Grumpig"
      , form = Original
      , typing = Single Psychic
      , ability = Just ThickFat
      , evolutionData = EvolvesFrom 453 "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 455
      , nationalDexNumber = 327
      , name = "Spinda"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 456
      , nationalDexNumber = 328
      , name = "Trapinch"
      , form = Original
      , typing = Single Ground
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 457
      , nationalDexNumber = 329
      , name = "Vibrava"
      , form = Original
      , typing = Double Ground Dragon
      , ability = Just Levitate
      , evolutionData = EvolvesFrom 456 "Level 35"
      , transformationData = DoesNotTransform
      }
    , { id = 458
      , nationalDexNumber = 330
      , name = "Flygon"
      , form = Original
      , typing = Double Ground Dragon
      , ability = Just Levitate
      , evolutionData = EvolvesFrom 457 "Level 45"
      , transformationData = DoesNotTransform
      }
    , { id = 459
      , nationalDexNumber = 331
      , name = "Cacnea"
      , form = Original
      , typing = Single Grass
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 460
      , nationalDexNumber = 332
      , name = "Cacturne"
      , form = Original
      , typing = Double Grass Dark
      , ability = Nothing
      , evolutionData = EvolvesFrom 459 "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 461
      , nationalDexNumber = 333
      , name = "Swablu"
      , form = Original
      , typing = Double Normal Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 462
      , nationalDexNumber = 334
      , name = "Altaria"
      , form = Original
      , typing = Double Dragon Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 461 "Level 35"
      , transformationData = DoesNotTransform
      }
    , { id = 1589
      , nationalDexNumber = 334
      , name = "Altaria"
      , form = Mega
      , typing = Double Dragon Fairy
      , ability = Nothing
      , evolutionData = EvolvesFrom 462 "Holding Altarianite"
      , transformationData = DoesNotTransform
      }
    , { id = 463
      , nationalDexNumber = 335
      , name = "Zangoose"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 464
      , nationalDexNumber = 336
      , name = "Seviper"
      , form = Original
      , typing = Single Poison
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 465
      , nationalDexNumber = 337
      , name = "Lunatone"
      , form = Original
      , typing = Double Rock Psychic
      , ability = Just Levitate
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 466
      , nationalDexNumber = 338
      , name = "Solrock"
      , form = Original
      , typing = Double Rock Psychic
      , ability = Just Levitate
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 467
      , nationalDexNumber = 339
      , name = "Barboach"
      , form = Original
      , typing = Double Water Ground
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 468
      , nationalDexNumber = 340
      , name = "Whiscash"
      , form = Original
      , typing = Double Water Ground
      , ability = Nothing
      , evolutionData = EvolvesFrom 467 "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 469
      , nationalDexNumber = 341
      , name = "Corphish"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 470
      , nationalDexNumber = 342
      , name = "Crawdaunt"
      , form = Original
      , typing = Double Water Dark
      , ability = Nothing
      , evolutionData = EvolvesFrom 469 "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 471
      , nationalDexNumber = 343
      , name = "Baltoy"
      , form = Original
      , typing = Double Ground Psychic
      , ability = Just Levitate
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 472
      , nationalDexNumber = 344
      , name = "Claydol"
      , form = Original
      , typing = Double Ground Psychic
      , ability = Just Levitate
      , evolutionData = EvolvesFrom 471 "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 473
      , nationalDexNumber = 345
      , name = "Lileep"
      , form = Original
      , typing = Double Rock Grass
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 474
      , nationalDexNumber = 346
      , name = "Cradily"
      , form = Original
      , typing = Double Rock Grass
      , ability = Nothing
      , evolutionData = EvolvesFrom 473 "Level 40"
      , transformationData = DoesNotTransform
      }
    , { id = 475
      , nationalDexNumber = 347
      , name = "Anorith"
      , form = Original
      , typing = Double Rock Bug
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 476
      , nationalDexNumber = 348
      , name = "Armaldo"
      , form = Original
      , typing = Double Rock Bug
      , ability = Nothing
      , evolutionData = EvolvesFrom 475 "Level 40"
      , transformationData = DoesNotTransform
      }
    , { id = 477
      , nationalDexNumber = 349
      , name = "Feebas"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 478
      , nationalDexNumber = 350
      , name = "Milotic"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = EvolvesFrom 477 "Trade holding Prism Scale"
      , transformationData = DoesNotTransform
      }
    , { id = 479
      , nationalDexNumber = 351
      , name = "Castform"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 1 "During normal weather, fog, sandstorm, or shadowy aura"
      }
    , { id = 1565
      , nationalDexNumber = 351
      , name = "Castform"
      , form = Unique "Sunny"
      , typing = Single Fire
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 1 "During harsh sunlight"
      }
    , { id = 1566
      , nationalDexNumber = 351
      , name = "Castform"
      , form = Unique "Rainy"
      , typing = Single Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 1 "During rain"
      }
    , { id = 1567
      , nationalDexNumber = 351
      , name = "Castform"
      , form = Unique "Snowy"
      , typing = Single Ice
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 1 "During hail"
      }
    , { id = 480
      , nationalDexNumber = 352
      , name = "Kecleon"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 481
      , nationalDexNumber = 353
      , name = "Shuppet"
      , form = Original
      , typing = Single Ghost
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 482
      , nationalDexNumber = 354
      , name = "Banette"
      , form = Original
      , typing = Single Ghost
      , ability = Nothing
      , evolutionData = EvolvesFrom 481 "Level 37"
      , transformationData = DoesNotTransform
      }
    , { id = 1592
      , nationalDexNumber = 354
      , name = "Banette"
      , form = Mega
      , typing = Single Ghost
      , ability = Nothing
      , evolutionData = EvolvesFrom 482 "Holding Banettite"
      , transformationData = DoesNotTransform
      }
    , { id = 483
      , nationalDexNumber = 355
      , name = "Duskull"
      , form = Original
      , typing = Single Ghost
      , ability = Just Levitate
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 484
      , nationalDexNumber = 356
      , name = "Dusclops"
      , form = Original
      , typing = Single Ghost
      , ability = Nothing
      , evolutionData = EvolvesFrom 483 "Level 37"
      , transformationData = DoesNotTransform
      }
    , { id = 727
      , nationalDexNumber = 477
      , name = "Dusknoir"
      , form = Original
      , typing = Single Ghost
      , ability = Nothing
      , evolutionData = EvolvesFrom 484 "Trade holding Reaper Cloth"
      , transformationData = DoesNotTransform
      }
    , { id = 485
      , nationalDexNumber = 357
      , name = "Tropius"
      , form = Original
      , typing = Double Grass Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 631
      , nationalDexNumber = 433
      , name = "Chingling"
      , form = Original
      , typing = Single Psychic
      , ability = Just Levitate
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 486
      , nationalDexNumber = 358
      , name = "Chimecho"
      , form = Original
      , typing = Single Psychic
      , ability = Just Levitate
      , evolutionData = EvolvesFrom 631 "Level during the night with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 487
      , nationalDexNumber = 359
      , name = "Absol"
      , form = Original
      , typing = Single Dark
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1591
      , nationalDexNumber = 359
      , name = "Absol"
      , form = Mega
      , typing = Single Dark
      , ability = Nothing
      , evolutionData = EvolvesFrom 487 "Holding Absolite"
      , transformationData = DoesNotTransform
      }
    , { id = 489
      , nationalDexNumber = 361
      , name = "Snorunt"
      , form = Original
      , typing = Single Ice
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 490
      , nationalDexNumber = 362
      , name = "Glalie"
      , form = Original
      , typing = Single Ice
      , ability = Nothing
      , evolutionData = EvolvesFrom 489 "Level 42"
      , transformationData = DoesNotTransform
      }
    , { id = 1560
      , nationalDexNumber = 362
      , name = "Glalie"
      , form = Mega
      , typing = Single Ice
      , ability = Nothing
      , evolutionData = EvolvesFrom 490 "Holding Glalitite"
      , transformationData = DoesNotTransform
      }
    , { id = 728
      , nationalDexNumber = 478
      , name = "Froslass"
      , form = Original
      , typing = Double Ice Ghost
      , ability = Nothing
      , evolutionData = EvolvesFrom 489 "Use Dawn Stone when female"
      , transformationData = DoesNotTransform
      }
    , { id = 491
      , nationalDexNumber = 363
      , name = "Spheal"
      , form = Original
      , typing = Double Ice Water
      , ability = Just ThickFat
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 492
      , nationalDexNumber = 364
      , name = "Sealeo"
      , form = Original
      , typing = Double Ice Water
      , ability = Just ThickFat
      , evolutionData = EvolvesFrom 491 "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 493
      , nationalDexNumber = 365
      , name = "Walrein"
      , form = Original
      , typing = Double Ice Water
      , ability = Just ThickFat
      , evolutionData = EvolvesFrom 492 "Level 44"
      , transformationData = DoesNotTransform
      }
    , { id = 494
      , nationalDexNumber = 366
      , name = "Clamperl"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 495
      , nationalDexNumber = 367
      , name = "Huntail"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = EvolvesFrom 494 "Trade holding Deep Sea Tooth"
      , transformationData = DoesNotTransform
      }
    , { id = 496
      , nationalDexNumber = 368
      , name = "Gorebyss"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = EvolvesFrom 494 "Trade holding Deep Sea Scale"
      , transformationData = DoesNotTransform
      }
    , { id = 497
      , nationalDexNumber = 369
      , name = "Relicanth"
      , form = Original
      , typing = Double Water Rock
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 498
      , nationalDexNumber = 370
      , name = "Luvdisc"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 499
      , nationalDexNumber = 371
      , name = "Bagon"
      , form = Original
      , typing = Single Dragon
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 500
      , nationalDexNumber = 372
      , name = "Shelgon"
      , form = Original
      , typing = Single Dragon
      , ability = Nothing
      , evolutionData = EvolvesFrom 499 "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 501
      , nationalDexNumber = 373
      , name = "Salamence"
      , form = Original
      , typing = Double Dragon Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 500 "Level 50"
      , transformationData = DoesNotTransform
      }
    , { id = 1586
      , nationalDexNumber = 373
      , name = "Salamence"
      , form = Mega
      , typing = Double Dragon Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 501 "Holding Salamencite"
      , transformationData = DoesNotTransform
      }
    , { id = 502
      , nationalDexNumber = 374
      , name = "Beldum"
      , form = Original
      , typing = Double Steel Psychic
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 503
      , nationalDexNumber = 375
      , name = "Metang"
      , form = Original
      , typing = Double Steel Psychic
      , ability = Nothing
      , evolutionData = EvolvesFrom 502 "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 504
      , nationalDexNumber = 376
      , name = "Metagross"
      , form = Original
      , typing = Double Steel Psychic
      , ability = Nothing
      , evolutionData = EvolvesFrom 503 "Level 45"
      , transformationData = DoesNotTransform
      }
    , { id = 1585
      , nationalDexNumber = 376
      , name = "Metagross"
      , form = Mega
      , typing = Double Steel Psychic
      , ability = Nothing
      , evolutionData = EvolvesFrom 504 "Holding Metagrossite"
      , transformationData = DoesNotTransform
      }
    , { id = 505
      , nationalDexNumber = 377
      , name = "Regirock"
      , form = Original
      , typing = Single Rock
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 506
      , nationalDexNumber = 378
      , name = "Regice"
      , form = Original
      , typing = Single Ice
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 507
      , nationalDexNumber = 379
      , name = "Registeel"
      , form = Original
      , typing = Single Steel
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 508
      , nationalDexNumber = 380
      , name = "Latias"
      , form = Original
      , typing = Double Dragon Psychic
      , ability = Just Levitate
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1400
      , nationalDexNumber = 380
      , name = "Latias"
      , form = Mega
      , typing = Double Dragon Psychic
      , ability = Just Levitate
      , evolutionData = EvolvesFrom 508 "Holding Latiasite"
      , transformationData = DoesNotTransform
      }
    , { id = 509
      , nationalDexNumber = 381
      , name = "Latios"
      , form = Original
      , typing = Double Dragon Psychic
      , ability = Just Levitate
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1399
      , nationalDexNumber = 381
      , name = "Latios"
      , form = Mega
      , typing = Double Dragon Psychic
      , ability = Just Levitate
      , evolutionData = EvolvesFrom 509 "Holding Latiosite"
      , transformationData = DoesNotTransform
      }
    , { id = 510
      , nationalDexNumber = 382
      , name = "Kyogre"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 2 "If not holding Blue Orb"
      }
    , { id = 1432
      , nationalDexNumber = 382
      , name = "Kyogre"
      , form = Unique "Primal"
      , typing = Single Water
      , ability = Just PrimordialSea
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 2 "While holding Blue Orb"
      }
    , { id = 511
      , nationalDexNumber = 383
      , name = "Groudon"
      , form = Original
      , typing = Single Ground
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 3 "If not holding Red Orb"
      }
    , { id = 1433
      , nationalDexNumber = 383
      , name = "Groudon"
      , form = Unique "Primal"
      , typing = Double Ground Fire
      , ability = Just DesolateLand
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 3 "While holding Red Orb"
      }
    , { id = 512
      , nationalDexNumber = 384
      , name = "Rayquaza"
      , form = Original
      , typing = Double Dragon Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1431
      , nationalDexNumber = 384
      , name = "Rayquaza"
      , form = Mega
      , typing = Double Dragon Flying
      , ability = Just DeltaStream
      , evolutionData = EvolvesFrom 512 "Knowing Dragon Ascent and not holding a Z-Crystal"
      , transformationData = DoesNotTransform
      }
    , { id = 513
      , nationalDexNumber = 385
      , name = "Jirachi"
      , form = Original
      , typing = Double Steel Psychic
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 514
      , nationalDexNumber = 386
      , name = "Deoxys"
      , form = Original
      , typing = Single Psychic
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 4 "In Pokémon Ruby and Sapphire"
      }
    , { id = 1391
      , nationalDexNumber = 386
      , name = "Deoxys"
      , form = Unique "Attack"
      , typing = Single Psychic
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 4 "In Pokémon FireRed"
      }
    , { id = 1392
      , nationalDexNumber = 386
      , name = "Deoxys"
      , form = Unique "Defense"
      , typing = Single Psychic
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 4 "In Pokémon LeafGreen"
      }
    , { id = 1393
      , nationalDexNumber = 386
      , name = "Deoxys"
      , form = Unique "Speed"
      , typing = Single Psychic
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 4 "In Pokémon Emerald"
      }
    , { id = 591
      , nationalDexNumber = 387
      , name = "Turtwig"
      , form = Original
      , typing = Single Grass
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 592
      , nationalDexNumber = 388
      , name = "Grotle"
      , form = Original
      , typing = Single Grass
      , ability = Nothing
      , evolutionData = EvolvesFrom 591 "Level 18"
      , transformationData = DoesNotTransform
      }
    , { id = 593
      , nationalDexNumber = 389
      , name = "Torterra"
      , form = Original
      , typing = Double Grass Ground
      , ability = Nothing
      , evolutionData = EvolvesFrom 592 "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 34
      , nationalDexNumber = 390
      , name = "Chimchar"
      , form = Original
      , typing = Single Fire
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 35
      , nationalDexNumber = 391
      , name = "Monferno"
      , form = Original
      , typing = Double Fire Fighting
      , ability = Nothing
      , evolutionData = EvolvesFrom 34 "Level 14"
      , transformationData = DoesNotTransform
      }
    , { id = 19
      , nationalDexNumber = 392
      , name = "Infernape"
      , form = Original
      , typing = Double Fire Fighting
      , ability = Nothing
      , evolutionData = EvolvesFrom 35 "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 594
      , nationalDexNumber = 393
      , name = "Piplup"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 595
      , nationalDexNumber = 394
      , name = "Prinplup"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = EvolvesFrom 594 "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 596
      , nationalDexNumber = 395
      , name = "Empoleon"
      , form = Original
      , typing = Double Water Steel
      , ability = Nothing
      , evolutionData = EvolvesFrom 595 "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 597
      , nationalDexNumber = 396
      , name = "Starly"
      , form = Original
      , typing = Double Normal Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 598
      , nationalDexNumber = 397
      , name = "Staravia"
      , form = Original
      , typing = Double Normal Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 597 "Level 14"
      , transformationData = DoesNotTransform
      }
    , { id = 599
      , nationalDexNumber = 398
      , name = "Staraptor"
      , form = Original
      , typing = Double Normal Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 598 "Level 34"
      , transformationData = DoesNotTransform
      }
    , { id = 600
      , nationalDexNumber = 399
      , name = "Bidoof"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 601
      , nationalDexNumber = 400
      , name = "Bibarel"
      , form = Original
      , typing = Double Normal Water
      , ability = Nothing
      , evolutionData = EvolvesFrom 600 "Level 15"
      , transformationData = DoesNotTransform
      }
    , { id = 602
      , nationalDexNumber = 401
      , name = "Kricketot"
      , form = Original
      , typing = Single Bug
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 603
      , nationalDexNumber = 402
      , name = "Kricketune"
      , form = Original
      , typing = Single Bug
      , ability = Nothing
      , evolutionData = EvolvesFrom 602 "Level 10"
      , transformationData = DoesNotTransform
      }
    , { id = 20
      , nationalDexNumber = 403
      , name = "Shinx"
      , form = Original
      , typing = Single Electric
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 21
      , nationalDexNumber = 404
      , name = "Luxio"
      , form = Original
      , typing = Single Electric
      , ability = Nothing
      , evolutionData = EvolvesFrom 20 "Level 15"
      , transformationData = DoesNotTransform
      }
    , { id = 22
      , nationalDexNumber = 405
      , name = "Luxray"
      , form = Original
      , typing = Single Electric
      , ability = Nothing
      , evolutionData = EvolvesFrom 21 "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 606
      , nationalDexNumber = 408
      , name = "Cranidos"
      , form = Original
      , typing = Single Rock
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 607
      , nationalDexNumber = 409
      , name = "Rampardos"
      , form = Original
      , typing = Single Rock
      , ability = Nothing
      , evolutionData = EvolvesFrom 606 "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 608
      , nationalDexNumber = 410
      , name = "Shieldon"
      , form = Original
      , typing = Double Rock Steel
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 609
      , nationalDexNumber = 411
      , name = "Bastiodon"
      , form = Original
      , typing = Double Rock Steel
      , ability = Nothing
      , evolutionData = EvolvesFrom 608 "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 610
      , nationalDexNumber = 412
      , name = "Burmy"
      , form = Unique "Plant"
      , typing = Single Bug
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 5 "If it last battled in grass"
      }
    , { id = 611
      , nationalDexNumber = 413
      , name = "Wormadam"
      , form = Unique "Plant"
      , typing = Double Bug Grass
      , ability = Nothing
      , evolutionData = EvolvesFrom 610 "Level 20 when female"
      , transformationData = DoesNotTransform
      }
    , { id = 1606
      , nationalDexNumber = 412
      , name = "Burmy"
      , form = Unique "Sandy"
      , typing = Single Bug
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 5 "If it last battled in a cave"
      }
    , { id = 1568
      , nationalDexNumber = 413
      , name = "Wormadam"
      , form = Unique "Sandy"
      , typing = Double Bug Ground
      , ability = Nothing
      , evolutionData = EvolvesFrom 1606 "Level 20 when female"
      , transformationData = DoesNotTransform
      }
    , { id = 1607
      , nationalDexNumber = 412
      , name = "Burmy"
      , form = Unique "Trash"
      , typing = Single Bug
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 5 "If it last battled in a building"
      }
    , { id = 1569
      , nationalDexNumber = 413
      , name = "Wormadam"
      , form = Unique "Trash"
      , typing = Double Bug Steel
      , ability = Nothing
      , evolutionData = EvolvesFrom 1607 "Level 20 when female"
      , transformationData = DoesNotTransform
      }
    , { id = 612
      , nationalDexNumber = 414
      , name = "Mothim"
      , form = Original
      , typing = Double Bug Flying
      , ability = Nothing
      , evolutionData = EvolvesFromMultiple [ 610, 1606, 1607 ] "Level 20 when male"
      , transformationData = DoesNotTransform
      }
    , { id = 613
      , nationalDexNumber = 415
      , name = "Combee"
      , form = Original
      , typing = Double Bug Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 614
      , nationalDexNumber = 416
      , name = "Vespiquen"
      , form = Original
      , typing = Double Bug Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 613 "Level 21 When Female"
      , transformationData = DoesNotTransform
      }
    , { id = 615
      , nationalDexNumber = 417
      , name = "Pachirisu"
      , form = Original
      , typing = Single Electric
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 616
      , nationalDexNumber = 418
      , name = "Buizel"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 617
      , nationalDexNumber = 419
      , name = "Floatzel"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = EvolvesFrom 616 "Level 26"
      , transformationData = DoesNotTransform
      }
    , { id = 618
      , nationalDexNumber = 420
      , name = "Cherubi"
      , form = Original
      , typing = Single Grass
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 619
      , nationalDexNumber = 421
      , name = "Cherrim"
      , form = Original
      , typing = Single Grass
      , ability = Nothing
      , evolutionData = EvolvesFrom 618 "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 620
      , nationalDexNumber = 422
      , name = "Shellos"
      , form = Unique "West"
      , typing = Single Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 621
      , nationalDexNumber = 423
      , name = "Gastrodon"
      , form = Unique "West"
      , typing = Double Water Ground
      , ability = Nothing
      , evolutionData = EvolvesFrom 620 "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 1740
      , nationalDexNumber = 422
      , name = "Shellos"
      , form = Unique "East"
      , typing = Single Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1741
      , nationalDexNumber = 423
      , name = "Gastrodon"
      , form = Unique "East"
      , typing = Double Water Ground
      , ability = Nothing
      , evolutionData = EvolvesFrom 620 "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 623
      , nationalDexNumber = 425
      , name = "Drifloon"
      , form = Original
      , typing = Double Ghost Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 624
      , nationalDexNumber = 426
      , name = "Drifblim"
      , form = Original
      , typing = Double Ghost Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 623 "Level 28"
      , transformationData = DoesNotTransform
      }
    , { id = 625
      , nationalDexNumber = 427
      , name = "Buneary"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 626
      , nationalDexNumber = 428
      , name = "Lopunny"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = EvolvesFrom 625 "Level with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 1593
      , nationalDexNumber = 428
      , name = "Lopunny"
      , form = Mega
      , typing = Double Normal Fighting
      , ability = Nothing
      , evolutionData = EvolvesFrom 626 "Holding Lopunnite"
      , transformationData = DoesNotTransform
      }
    , { id = 629
      , nationalDexNumber = 431
      , name = "Glameow"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 630
      , nationalDexNumber = 432
      , name = "Purugly"
      , form = Original
      , typing = Single Normal
      , ability = Just ThickFat
      , evolutionData = EvolvesFrom 629 "Level 38"
      , transformationData = DoesNotTransform
      }
    , { id = 632
      , nationalDexNumber = 434
      , name = "Stunky"
      , form = Original
      , typing = Double Poison Dark
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 633
      , nationalDexNumber = 435
      , name = "Skuntank"
      , form = Original
      , typing = Double Poison Dark
      , ability = Nothing
      , evolutionData = EvolvesFrom 632 "Level 34"
      , transformationData = DoesNotTransform
      }
    , { id = 634
      , nationalDexNumber = 436
      , name = "Bronzor"
      , form = Original
      , typing = Double Steel Psychic
      , ability = Just Levitate
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 635
      , nationalDexNumber = 437
      , name = "Bronzong"
      , form = Original
      , typing = Double Steel Psychic
      , ability = Just Levitate
      , evolutionData = EvolvesFrom 634 "Level 33"
      , transformationData = DoesNotTransform
      }
    , { id = 639
      , nationalDexNumber = 441
      , name = "Chatot"
      , form = Original
      , typing = Double Normal Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 640
      , nationalDexNumber = 442
      , name = "Spiritomb"
      , form = Original
      , typing = Double Ghost Dark
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 641
      , nationalDexNumber = 443
      , name = "Gible"
      , form = Original
      , typing = Double Dragon Ground
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 642
      , nationalDexNumber = 444
      , name = "Gabite"
      , form = Original
      , typing = Double Dragon Ground
      , ability = Nothing
      , evolutionData = EvolvesFrom 641 "Level 24"
      , transformationData = DoesNotTransform
      }
    , { id = 643
      , nationalDexNumber = 445
      , name = "Garchomp"
      , form = Original
      , typing = Double Dragon Ground
      , ability = Nothing
      , evolutionData = EvolvesFrom 642 "Level 48"
      , transformationData = DoesNotTransform
      }
    , { id = 1438
      , nationalDexNumber = 445
      , name = "Garchomp"
      , form = Mega
      , typing = Double Dragon Ground
      , ability = Nothing
      , evolutionData = EvolvesFrom 643 "Holding Garchompite"
      , transformationData = DoesNotTransform
      }
    , { id = 645
      , nationalDexNumber = 447
      , name = "Riolu"
      , form = Original
      , typing = Single Fighting
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 646
      , nationalDexNumber = 448
      , name = "Lucario"
      , form = Original
      , typing = Double Fighting Steel
      , ability = Nothing
      , evolutionData = EvolvesFrom 645 "Level during the day with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 1561
      , nationalDexNumber = 448
      , name = "Lucario"
      , form = Mega
      , typing = Double Fighting Steel
      , ability = Nothing
      , evolutionData = EvolvesFrom 646 "Holding Lucarionite"
      , transformationData = DoesNotTransform
      }
    , { id = 647
      , nationalDexNumber = 449
      , name = "Hippopotas"
      , form = Original
      , typing = Single Ground
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 648
      , nationalDexNumber = 450
      , name = "Hippowdon"
      , form = Original
      , typing = Single Ground
      , ability = Nothing
      , evolutionData = EvolvesFrom 647 "Level 34"
      , transformationData = DoesNotTransform
      }
    , { id = 649
      , nationalDexNumber = 451
      , name = "Skorupi"
      , form = Original
      , typing = Double Poison Bug
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 650
      , nationalDexNumber = 452
      , name = "Drapion"
      , form = Original
      , typing = Double Poison Dark
      , ability = Nothing
      , evolutionData = EvolvesFrom 649 "Level 40"
      , transformationData = DoesNotTransform
      }
    , { id = 651
      , nationalDexNumber = 453
      , name = "Croagunk"
      , form = Original
      , typing = Double Poison Fighting
      , ability = Just DrySkin
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 652
      , nationalDexNumber = 454
      , name = "Toxicroak"
      , form = Original
      , typing = Double Poison Fighting
      , ability = Just DrySkin
      , evolutionData = EvolvesFrom 651 "Level 37"
      , transformationData = DoesNotTransform
      }
    , { id = 653
      , nationalDexNumber = 455
      , name = "Carnivine"
      , form = Original
      , typing = Single Grass
      , ability = Just Levitate
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 654
      , nationalDexNumber = 456
      , name = "Finneon"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 655
      , nationalDexNumber = 457
      , name = "Lumineon"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = EvolvesFrom 654 "Level 31"
      , transformationData = DoesNotTransform
      }
    , { id = 657
      , nationalDexNumber = 459
      , name = "Snover"
      , form = Original
      , typing = Double Grass Ice
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 658
      , nationalDexNumber = 460
      , name = "Abomasnow"
      , form = Original
      , typing = Double Grass Ice
      , ability = Nothing
      , evolutionData = EvolvesFrom 657 "Level 40"
      , transformationData = DoesNotTransform
      }
    , { id = 1562
      , nationalDexNumber = 460
      , name = "Abomasnow"
      , form = Mega
      , typing = Double Grass Ice
      , ability = Nothing
      , evolutionData = EvolvesFrom 658 "Holding Abomasite"
      , transformationData = DoesNotTransform
      }
    , { id = 729
      , nationalDexNumber = 479
      , name = "Rotom"
      , form = Original
      , typing = Double Electric Ghost
      , ability = Just Levitate
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 6 "When not possessing an appliance"
      }
    , { id = 1408
      , nationalDexNumber = 479
      , name = "Rotom"
      , form = Unique "Heat"
      , typing = Double Electric Fire
      , ability = Just Levitate
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 6 "When it possesses a microwave oven"
      }
    , { id = 1409
      , nationalDexNumber = 479
      , name = "Rotom"
      , form = Unique "Wash"
      , typing = Double Electric Water
      , ability = Just Levitate
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 6 "When it possesses a washing machine"
      }
    , { id = 1410
      , nationalDexNumber = 479
      , name = "Rotom"
      , form = Unique "Frost"
      , typing = Double Electric Ice
      , ability = Just Levitate
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 6 "When it possesses a refrigerator"
      }
    , { id = 1411
      , nationalDexNumber = 479
      , name = "Rotom"
      , form = Unique "Fan"
      , typing = Double Electric Flying
      , ability = Just Levitate
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 6 "When it possesses an electric fan"
      }
    , { id = 1413
      , nationalDexNumber = 479
      , name = "Rotom"
      , form = Unique "Mow"
      , typing = Double Electric Grass
      , ability = Just Levitate
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 6 "When it possesses a lawn mower"
      }
    , { id = 730
      , nationalDexNumber = 480
      , name = "Uxie"
      , form = Original
      , typing = Single Psychic
      , ability = Just Levitate
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 731
      , nationalDexNumber = 481
      , name = "Mesprit"
      , form = Original
      , typing = Single Psychic
      , ability = Just Levitate
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 732
      , nationalDexNumber = 482
      , name = "Azelf"
      , form = Original
      , typing = Single Psychic
      , ability = Just Levitate
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 733
      , nationalDexNumber = 483
      , name = "Dialga"
      , form = Original
      , typing = Double Steel Dragon
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 734
      , nationalDexNumber = 484
      , name = "Palkia"
      , form = Original
      , typing = Double Water Dragon
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 735
      , nationalDexNumber = 485
      , name = "Heatran"
      , form = Original
      , typing = Double Fire Steel
      , ability = Just FlashFire
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 736
      , nationalDexNumber = 486
      , name = "Regigigas"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 737
      , nationalDexNumber = 487
      , name = "Giratina"
      , form = Original
      , typing = Double Ghost Dragon
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 7 "If not holding a Griseous Orb"
      }
    , { id = 1401
      , nationalDexNumber = 487
      , name = "Giratina"
      , form = Unique "Origin"
      , typing = Double Ghost Dragon
      , ability = Just Levitate
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 7 "While holding a Griseous Orb"
      }
    , { id = 738
      , nationalDexNumber = 488
      , name = "Cresselia"
      , form = Original
      , typing = Single Psychic
      , ability = Just Levitate
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 36
      , nationalDexNumber = 489
      , name = "Phione"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 37
      , nationalDexNumber = 490
      , name = "Manaphy"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 739
      , nationalDexNumber = 491
      , name = "Darkrai"
      , form = Original
      , typing = Single Dark
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 740
      , nationalDexNumber = 492
      , name = "Shaymin"
      , form = Original
      , typing = Single Grass
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 8 "During the night or when frozen"
      }
    , { id = 1407
      , nationalDexNumber = 492
      , name = "Shaymin"
      , form = Unique "Sky"
      , typing = Double Grass Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 8 "Use a Gracidea Flower in the daytime"
      }
    , { id = 741
      , nationalDexNumber = 493
      , name = "Arceus"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 742
      , nationalDexNumber = 494
      , name = "Victini"
      , form = Original
      , typing = Double Psychic Fire
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 829
      , nationalDexNumber = 495
      , name = "Snivy"
      , form = Original
      , typing = Single Grass
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 830
      , nationalDexNumber = 496
      , name = "Servine"
      , form = Original
      , typing = Single Grass
      , ability = Nothing
      , evolutionData = EvolvesFrom 829 "Level 17"
      , transformationData = DoesNotTransform
      }
    , { id = 831
      , nationalDexNumber = 497
      , name = "Serperior"
      , form = Original
      , typing = Single Grass
      , ability = Nothing
      , evolutionData = EvolvesFrom 830 "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 832
      , nationalDexNumber = 498
      , name = "Tepig"
      , form = Original
      , typing = Single Fire
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 833
      , nationalDexNumber = 499
      , name = "Pignite"
      , form = Original
      , typing = Double Fire Fighting
      , ability = Nothing
      , evolutionData = EvolvesFrom 832 "Level 17"
      , transformationData = DoesNotTransform
      }
    , { id = 834
      , nationalDexNumber = 500
      , name = "Emboar"
      , form = Original
      , typing = Double Fire Fighting
      , ability = Nothing
      , evolutionData = EvolvesFrom 833 "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 835
      , nationalDexNumber = 501
      , name = "Oshawott"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 836
      , nationalDexNumber = 502
      , name = "Dewott"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = EvolvesFrom 835 "Level 17"
      , transformationData = DoesNotTransform
      }
    , { id = 837
      , nationalDexNumber = 503
      , name = "Samurott"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = EvolvesFrom 836 "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 1624
      , nationalDexNumber = 503
      , name = "Samurott"
      , form = Regional 837 Hisui
      , typing = Double Water Dark
      , ability = Nothing
      , evolutionData = EvolvesFrom 836 "Level 36 in Legends: Arceus"
      , transformationData = DoesNotTransform
      }
    , { id = 838
      , nationalDexNumber = 504
      , name = "Patrat"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 839
      , nationalDexNumber = 505
      , name = "Watchog"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = EvolvesFrom 838 "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 840
      , nationalDexNumber = 506
      , name = "Lillipup"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 841
      , nationalDexNumber = 507
      , name = "Herdier"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = EvolvesFrom 840 "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 842
      , nationalDexNumber = 508
      , name = "Stoutland"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = EvolvesFrom 841 "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 843
      , nationalDexNumber = 509
      , name = "Purrloin"
      , form = Original
      , typing = Single Dark
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 844
      , nationalDexNumber = 510
      , name = "Liepard"
      , form = Original
      , typing = Single Dark
      , ability = Nothing
      , evolutionData = EvolvesFrom 843 "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 845
      , nationalDexNumber = 511
      , name = "Pansage"
      , form = Original
      , typing = Single Grass
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 846
      , nationalDexNumber = 512
      , name = "Simisage"
      , form = Original
      , typing = Single Grass
      , ability = Nothing
      , evolutionData = EvolvesFrom 845 "Use Leaf Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 847
      , nationalDexNumber = 513
      , name = "Pansear"
      , form = Original
      , typing = Single Fire
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 848
      , nationalDexNumber = 514
      , name = "Simisear"
      , form = Original
      , typing = Single Fire
      , ability = Nothing
      , evolutionData = EvolvesFrom 847 "Use Fire Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 849
      , nationalDexNumber = 515
      , name = "Panpour"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 850
      , nationalDexNumber = 516
      , name = "Simipour"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = EvolvesFrom 849 "Use Water Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 851
      , nationalDexNumber = 517
      , name = "Munna"
      , form = Original
      , typing = Single Psychic
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 852
      , nationalDexNumber = 518
      , name = "Musharna"
      , form = Original
      , typing = Single Psychic
      , ability = Nothing
      , evolutionData = EvolvesFrom 851 "Use Moon Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 853
      , nationalDexNumber = 519
      , name = "Pidove"
      , form = Original
      , typing = Double Normal Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 854
      , nationalDexNumber = 520
      , name = "Tranquill"
      , form = Original
      , typing = Double Normal Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 853 "Level 21"
      , transformationData = DoesNotTransform
      }
    , { id = 855
      , nationalDexNumber = 521
      , name = "Unfezant"
      , form = Original
      , typing = Double Normal Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 854 "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 856
      , nationalDexNumber = 522
      , name = "Blitzle"
      , form = Original
      , typing = Single Electric
      , ability = Just LightningRod
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 857
      , nationalDexNumber = 523
      , name = "Zebstrika"
      , form = Original
      , typing = Single Electric
      , ability = Just LightningRod
      , evolutionData = EvolvesFrom 856 "Level 27"
      , transformationData = DoesNotTransform
      }
    , { id = 858
      , nationalDexNumber = 524
      , name = "Roggenrola"
      , form = Original
      , typing = Single Rock
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 859
      , nationalDexNumber = 525
      , name = "Boldore"
      , form = Original
      , typing = Single Rock
      , ability = Nothing
      , evolutionData = EvolvesFrom 858 "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 860
      , nationalDexNumber = 526
      , name = "Gigalith"
      , form = Original
      , typing = Single Rock
      , ability = Nothing
      , evolutionData = EvolvesFrom 859 "Trade"
      , transformationData = DoesNotTransform
      }
    , { id = 861
      , nationalDexNumber = 527
      , name = "Woobat"
      , form = Original
      , typing = Double Psychic Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 862
      , nationalDexNumber = 528
      , name = "Swoobat"
      , form = Original
      , typing = Double Psychic Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 861 "Level with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 863
      , nationalDexNumber = 529
      , name = "Drilbur"
      , form = Original
      , typing = Single Ground
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 864
      , nationalDexNumber = 530
      , name = "Excadrill"
      , form = Original
      , typing = Double Ground Steel
      , ability = Nothing
      , evolutionData = EvolvesFrom 863 "Level 31"
      , transformationData = DoesNotTransform
      }
    , { id = 865
      , nationalDexNumber = 531
      , name = "Audino"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1563
      , nationalDexNumber = 531
      , name = "Audino"
      , form = Mega
      , typing = Double Normal Fairy
      , ability = Nothing
      , evolutionData = EvolvesFrom 865 "Holding Audinite"
      , transformationData = DoesNotTransform
      }
    , { id = 866
      , nationalDexNumber = 532
      , name = "Timburr"
      , form = Original
      , typing = Single Fighting
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 867
      , nationalDexNumber = 533
      , name = "Gurdurr"
      , form = Original
      , typing = Single Fighting
      , ability = Nothing
      , evolutionData = EvolvesFrom 866 "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 868
      , nationalDexNumber = 534
      , name = "Conkeldurr"
      , form = Original
      , typing = Single Fighting
      , ability = Nothing
      , evolutionData = EvolvesFrom 867 "Trade"
      , transformationData = DoesNotTransform
      }
    , { id = 869
      , nationalDexNumber = 535
      , name = "Tympole"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 870
      , nationalDexNumber = 536
      , name = "Palpitoad"
      , form = Original
      , typing = Double Water Ground
      , ability = Nothing
      , evolutionData = EvolvesFrom 869 "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 871
      , nationalDexNumber = 537
      , name = "Seismitoad"
      , form = Original
      , typing = Double Water Ground
      , ability = Nothing
      , evolutionData = EvolvesFrom 870 "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 872
      , nationalDexNumber = 538
      , name = "Throh"
      , form = Original
      , typing = Single Fighting
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 873
      , nationalDexNumber = 539
      , name = "Sawk"
      , form = Original
      , typing = Single Fighting
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 874
      , nationalDexNumber = 540
      , name = "Sewaddle"
      , form = Original
      , typing = Double Bug Grass
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 875
      , nationalDexNumber = 541
      , name = "Swadloon"
      , form = Original
      , typing = Double Bug Grass
      , ability = Nothing
      , evolutionData = EvolvesFrom 874 "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 876
      , nationalDexNumber = 542
      , name = "Leavanny"
      , form = Original
      , typing = Double Bug Grass
      , ability = Nothing
      , evolutionData = EvolvesFrom 875 "Level with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 877
      , nationalDexNumber = 543
      , name = "Venipede"
      , form = Original
      , typing = Double Bug Poison
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 878
      , nationalDexNumber = 544
      , name = "Whirlipede"
      , form = Original
      , typing = Double Bug Poison
      , ability = Nothing
      , evolutionData = EvolvesFrom 877 "Level 22"
      , transformationData = DoesNotTransform
      }
    , { id = 879
      , nationalDexNumber = 545
      , name = "Scolipede"
      , form = Original
      , typing = Double Bug Poison
      , ability = Nothing
      , evolutionData = EvolvesFrom 878 "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 880
      , nationalDexNumber = 546
      , name = "Cottonee"
      , form = Original
      , typing = Double Grass Fairy
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 881
      , nationalDexNumber = 547
      , name = "Whimsicott"
      , form = Original
      , typing = Double Grass Fairy
      , ability = Nothing
      , evolutionData = EvolvesFrom 880 "Use Sun Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 882
      , nationalDexNumber = 548
      , name = "Petilil"
      , form = Original
      , typing = Single Grass
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 883
      , nationalDexNumber = 549
      , name = "Lilligant"
      , form = Original
      , typing = Single Grass
      , ability = Nothing
      , evolutionData = EvolvesFrom 882 "Use Sun Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 1625
      , nationalDexNumber = 549
      , name = "Lilligant"
      , form = Regional 883 Hisui
      , typing = Double Grass Fighting
      , ability = Nothing
      , evolutionData = EvolvesFrom 882 "Use Sun Stone in Legends: Arceus"
      , transformationData = DoesNotTransform
      }
    , { id = 885
      , nationalDexNumber = 550
      , name = "Basculin"
      , form = Unique "Red-Striped"
      , typing = Single Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1414
      , nationalDexNumber = 550
      , name = "Basculin"
      , form = Unique "Blue-Striped"
      , typing = Single Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1608
      , nationalDexNumber = 550
      , name = "Basculin"
      , form = Unique "White-Striped"
      , typing = Single Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1612
      , nationalDexNumber = 902
      , name = "Basculegion"
      , form = Original
      , typing = Double Water Ghost
      , ability = Nothing
      , evolutionData = EvolvesFrom 1608 "After losing at least 294 HP from recoil damage"
      , transformationData = DoesNotTransform
      }
    , { id = 886
      , nationalDexNumber = 551
      , name = "Sandile"
      , form = Original
      , typing = Double Ground Dark
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 887
      , nationalDexNumber = 552
      , name = "Krokorok"
      , form = Original
      , typing = Double Ground Dark
      , ability = Nothing
      , evolutionData = EvolvesFrom 886 "Level 29"
      , transformationData = DoesNotTransform
      }
    , { id = 888
      , nationalDexNumber = 553
      , name = "Krookodile"
      , form = Original
      , typing = Double Ground Dark
      , ability = Nothing
      , evolutionData = EvolvesFrom 887 "Level 40"
      , transformationData = DoesNotTransform
      }
    , { id = 889
      , nationalDexNumber = 554
      , name = "Darumaka"
      , form = Original
      , typing = Single Fire
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 890
      , nationalDexNumber = 555
      , name = "Darmanitan"
      , form = Original
      , typing = Single Fire
      , ability = Nothing
      , evolutionData = EvolvesFrom 889 "Level 35"
      , transformationData = Transforms 9 "With HP above half"
      }
    , { id = 1554
      , nationalDexNumber = 555
      , name = "Darmanitan"
      , form = Unique "Zen"
      , typing = Double Fire Psychic
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 9 "With HP below half"
      }
    , { id = 1557
      , nationalDexNumber = 554
      , name = "Darumaka"
      , form = Regional 889 Galar
      , typing = Single Ice
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1555
      , nationalDexNumber = 555
      , name = "Darmanitan"
      , form = Regional 890 Galar
      , typing = Single Ice
      , ability = Nothing
      , evolutionData = EvolvesFrom 1557 "Use Ice Stone"
      , transformationData = Transforms 10 "With HP above half"
      }
    , { id = 1556
      , nationalDexNumber = 555
      , name = "Darmanitan"
      , form = UniqueRegional 890 Galar "Zen"
      , typing = Double Ice Fire
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 10 "With HP below half"
      }
    , { id = 891
      , nationalDexNumber = 556
      , name = "Maractus"
      , form = Original
      , typing = Single Grass
      , ability = Just WaterAbsorb
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 892
      , nationalDexNumber = 557
      , name = "Dwebble"
      , form = Original
      , typing = Double Bug Rock
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 893
      , nationalDexNumber = 558
      , name = "Crustle"
      , form = Original
      , typing = Double Bug Rock
      , ability = Nothing
      , evolutionData = EvolvesFrom 892 "Level 34"
      , transformationData = DoesNotTransform
      }
    , { id = 894
      , nationalDexNumber = 559
      , name = "Scraggy"
      , form = Original
      , typing = Double Dark Fighting
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 914
      , nationalDexNumber = 560
      , name = "Scrafty"
      , form = Original
      , typing = Double Dark Fighting
      , ability = Nothing
      , evolutionData = EvolvesFrom 894 "Level 39"
      , transformationData = DoesNotTransform
      }
    , { id = 915
      , nationalDexNumber = 561
      , name = "Sigilyph"
      , form = Original
      , typing = Double Psychic Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 916
      , nationalDexNumber = 562
      , name = "Yamask"
      , form = Original
      , typing = Single Ghost
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 917
      , nationalDexNumber = 563
      , name = "Cofagrigus"
      , form = Original
      , typing = Single Ghost
      , ability = Nothing
      , evolutionData = EvolvesFrom 916 "Level 34"
      , transformationData = DoesNotTransform
      }
    , { id = 1464
      , nationalDexNumber = 562
      , name = "Yamask"
      , form = Regional 916 Galar
      , typing = Double Ground Ghost
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1465
      , nationalDexNumber = 867
      , name = "Runerigus"
      , form = Original
      , typing = Double Ground Ghost
      , ability = Nothing
      , evolutionData = EvolvesFrom 1464 "Take 49+ damage and travel under the Stone Bridge in Dusty Bowl in Galar"
      , transformationData = DoesNotTransform
      }
    , { id = 918
      , nationalDexNumber = 564
      , name = "Tirtouga"
      , form = Original
      , typing = Double Water Rock
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 919
      , nationalDexNumber = 565
      , name = "Carracosta"
      , form = Original
      , typing = Double Water Rock
      , ability = Nothing
      , evolutionData = EvolvesFrom 918 "Level 37"
      , transformationData = DoesNotTransform
      }
    , { id = 920
      , nationalDexNumber = 566
      , name = "Archen"
      , form = Original
      , typing = Double Rock Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 921
      , nationalDexNumber = 567
      , name = "Archeops"
      , form = Original
      , typing = Double Rock Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 920 "Level 37"
      , transformationData = DoesNotTransform
      }
    , { id = 922
      , nationalDexNumber = 568
      , name = "Trubbish"
      , form = Original
      , typing = Single Poison
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 923
      , nationalDexNumber = 569
      , name = "Garbodor"
      , form = Original
      , typing = Single Poison
      , ability = Nothing
      , evolutionData = EvolvesFrom 922 "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 939
      , nationalDexNumber = 570
      , name = "Zorua"
      , form = Original
      , typing = Single Dark
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 940
      , nationalDexNumber = 571
      , name = "Zoroark"
      , form = Original
      , typing = Single Dark
      , ability = Nothing
      , evolutionData = EvolvesFrom 939 "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 1626
      , nationalDexNumber = 570
      , name = "Zorua"
      , form = Regional 939 Hisui
      , typing = Double Normal Ghost
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1627
      , nationalDexNumber = 571
      , name = "Zoroark"
      , form = Regional 940 Hisui
      , typing = Double Normal Ghost
      , ability = Nothing
      , evolutionData = EvolvesFrom 1626 "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 941
      , nationalDexNumber = 572
      , name = "Minccino"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 942
      , nationalDexNumber = 573
      , name = "Cinccino"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = EvolvesFrom 941 "Use Shiny Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 943
      , nationalDexNumber = 574
      , name = "Gothita"
      , form = Original
      , typing = Single Psychic
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 944
      , nationalDexNumber = 575
      , name = "Gothorita"
      , form = Original
      , typing = Single Psychic
      , ability = Nothing
      , evolutionData = EvolvesFrom 943 "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 945
      , nationalDexNumber = 576
      , name = "Gothitelle"
      , form = Original
      , typing = Single Psychic
      , ability = Nothing
      , evolutionData = EvolvesFrom 944 "Level 41"
      , transformationData = DoesNotTransform
      }
    , { id = 946
      , nationalDexNumber = 577
      , name = "Solosis"
      , form = Original
      , typing = Single Psychic
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 947
      , nationalDexNumber = 578
      , name = "Duosion"
      , form = Original
      , typing = Single Psychic
      , ability = Nothing
      , evolutionData = EvolvesFrom 946 "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 948
      , nationalDexNumber = 579
      , name = "Reuniclus"
      , form = Original
      , typing = Single Psychic
      , ability = Nothing
      , evolutionData = EvolvesFrom 947 "Level 41"
      , transformationData = DoesNotTransform
      }
    , { id = 950
      , nationalDexNumber = 580
      , name = "Ducklett"
      , form = Original
      , typing = Double Water Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 951
      , nationalDexNumber = 581
      , name = "Swanna"
      , form = Original
      , typing = Double Water Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 950 "Level 35"
      , transformationData = DoesNotTransform
      }
    , { id = 952
      , nationalDexNumber = 582
      , name = "Vanillite"
      , form = Original
      , typing = Single Ice
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 953
      , nationalDexNumber = 583
      , name = "Vanillish"
      , form = Original
      , typing = Single Ice
      , ability = Nothing
      , evolutionData = EvolvesFrom 952 "Level 35"
      , transformationData = DoesNotTransform
      }
    , { id = 954
      , nationalDexNumber = 584
      , name = "Vanilluxe"
      , form = Original
      , typing = Single Ice
      , ability = Nothing
      , evolutionData = EvolvesFrom 953 "Level 47"
      , transformationData = DoesNotTransform
      }
    , { id = 955
      , nationalDexNumber = 585
      , name = "Deerling"
      , form = Original
      , typing = Double Normal Grass
      , ability = Just SapSipper
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 956
      , nationalDexNumber = 586
      , name = "Sawsbuck"
      , form = Original
      , typing = Double Normal Grass
      , ability = Just SapSipper
      , evolutionData = EvolvesFrom 955 "Level 34"
      , transformationData = DoesNotTransform
      }
    , { id = 957
      , nationalDexNumber = 587
      , name = "Emolga"
      , form = Original
      , typing = Double Electric Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 958
      , nationalDexNumber = 588
      , name = "Karrablast"
      , form = Original
      , typing = Single Bug
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 959
      , nationalDexNumber = 589
      , name = "Escavalier"
      , form = Original
      , typing = Double Bug Steel
      , ability = Nothing
      , evolutionData = EvolvesFrom 958 "Trade for a Shelmet"
      , transformationData = DoesNotTransform
      }
    , { id = 961
      , nationalDexNumber = 590
      , name = "Foongus"
      , form = Original
      , typing = Double Grass Poison
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 962
      , nationalDexNumber = 591
      , name = "Amoonguss"
      , form = Original
      , typing = Double Grass Poison
      , ability = Nothing
      , evolutionData = EvolvesFrom 961 "Level 39"
      , transformationData = DoesNotTransform
      }
    , { id = 963
      , nationalDexNumber = 592
      , name = "Frillish"
      , form = Original
      , typing = Double Water Ghost
      , ability = Just WaterAbsorb
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 964
      , nationalDexNumber = 593
      , name = "Jellicent"
      , form = Original
      , typing = Double Water Ghost
      , ability = Just WaterAbsorb
      , evolutionData = EvolvesFrom 963 "Level 40"
      , transformationData = DoesNotTransform
      }
    , { id = 965
      , nationalDexNumber = 594
      , name = "Alomomola"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 966
      , nationalDexNumber = 595
      , name = "Joltik"
      , form = Original
      , typing = Double Bug Electric
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 967
      , nationalDexNumber = 596
      , name = "Galvantula"
      , form = Original
      , typing = Double Bug Electric
      , ability = Nothing
      , evolutionData = EvolvesFrom 966 "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 968
      , nationalDexNumber = 597
      , name = "Ferroseed"
      , form = Original
      , typing = Double Grass Steel
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 969
      , nationalDexNumber = 598
      , name = "Ferrothorn"
      , form = Original
      , typing = Double Grass Steel
      , ability = Nothing
      , evolutionData = EvolvesFrom 968 "Level 40"
      , transformationData = DoesNotTransform
      }
    , { id = 970
      , nationalDexNumber = 599
      , name = "Klink"
      , form = Original
      , typing = Single Steel
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 972
      , nationalDexNumber = 600
      , name = "Klang"
      , form = Original
      , typing = Single Steel
      , ability = Nothing
      , evolutionData = EvolvesFrom 970 "Level 38"
      , transformationData = DoesNotTransform
      }
    , { id = 973
      , nationalDexNumber = 601
      , name = "Klinklang"
      , form = Original
      , typing = Single Steel
      , ability = Nothing
      , evolutionData = EvolvesFrom 972 "Level 49"
      , transformationData = DoesNotTransform
      }
    , { id = 974
      , nationalDexNumber = 602
      , name = "Tynamo"
      , form = Original
      , typing = Single Electric
      , ability = Just Levitate
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 975
      , nationalDexNumber = 603
      , name = "Eelektrik"
      , form = Original
      , typing = Single Electric
      , ability = Just Levitate
      , evolutionData = EvolvesFrom 974 "Level 39"
      , transformationData = DoesNotTransform
      }
    , { id = 976
      , nationalDexNumber = 604
      , name = "Eelektross"
      , form = Original
      , typing = Single Electric
      , ability = Just Levitate
      , evolutionData = EvolvesFrom 975 "Use Thunder Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 977
      , nationalDexNumber = 605
      , name = "Elgyem"
      , form = Original
      , typing = Single Psychic
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 978
      , nationalDexNumber = 606
      , name = "Beheeyem"
      , form = Original
      , typing = Single Psychic
      , ability = Nothing
      , evolutionData = EvolvesFrom 977 "Level 42"
      , transformationData = DoesNotTransform
      }
    , { id = 979
      , nationalDexNumber = 607
      , name = "Litwick"
      , form = Original
      , typing = Double Ghost Fire
      , ability = Just FlashFire
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 980
      , nationalDexNumber = 608
      , name = "Lampent"
      , form = Original
      , typing = Double Ghost Fire
      , ability = Just FlashFire
      , evolutionData = EvolvesFrom 979 "Level 41"
      , transformationData = DoesNotTransform
      }
    , { id = 981
      , nationalDexNumber = 609
      , name = "Chandelure"
      , form = Original
      , typing = Double Ghost Fire
      , ability = Just FlashFire
      , evolutionData = EvolvesFrom 980 "Use Dusk Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 983
      , nationalDexNumber = 610
      , name = "Axew"
      , form = Original
      , typing = Single Dragon
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 984
      , nationalDexNumber = 611
      , name = "Fraxure"
      , form = Original
      , typing = Single Dragon
      , ability = Nothing
      , evolutionData = EvolvesFrom 983 "Level 38"
      , transformationData = DoesNotTransform
      }
    , { id = 985
      , nationalDexNumber = 612
      , name = "Haxorus"
      , form = Original
      , typing = Single Dragon
      , ability = Nothing
      , evolutionData = EvolvesFrom 984 "Level 48"
      , transformationData = DoesNotTransform
      }
    , { id = 986
      , nationalDexNumber = 613
      , name = "Cubchoo"
      , form = Original
      , typing = Single Ice
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 987
      , nationalDexNumber = 614
      , name = "Beartic"
      , form = Original
      , typing = Single Ice
      , ability = Nothing
      , evolutionData = EvolvesFrom 986 "Level 37"
      , transformationData = DoesNotTransform
      }
    , { id = 988
      , nationalDexNumber = 615
      , name = "Cryogonal"
      , form = Original
      , typing = Single Ice
      , ability = Just Levitate
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 989
      , nationalDexNumber = 616
      , name = "Shelmet"
      , form = Original
      , typing = Single Bug
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 990
      , nationalDexNumber = 617
      , name = "Accelgor"
      , form = Original
      , typing = Single Bug
      , ability = Nothing
      , evolutionData = EvolvesFrom 989 "Trade for Karrablast"
      , transformationData = DoesNotTransform
      }
    , { id = 991
      , nationalDexNumber = 618
      , name = "Stunfisk"
      , form = Original
      , typing = Double Ground Electric
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1553
      , nationalDexNumber = 618
      , name = "Stunfisk"
      , form = Regional 991 Galar
      , typing = Double Ground Steel
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 992
      , nationalDexNumber = 619
      , name = "Mienfoo"
      , form = Original
      , typing = Single Fighting
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 994
      , nationalDexNumber = 620
      , name = "Mienshao"
      , form = Original
      , typing = Single Fighting
      , ability = Nothing
      , evolutionData = EvolvesFrom 992 "Level 50"
      , transformationData = DoesNotTransform
      }
    , { id = 995
      , nationalDexNumber = 621
      , name = "Druddigon"
      , form = Original
      , typing = Single Dragon
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 996
      , nationalDexNumber = 622
      , name = "Golett"
      , form = Original
      , typing = Double Ground Ghost
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 997
      , nationalDexNumber = 623
      , name = "Golurk"
      , form = Original
      , typing = Double Ground Ghost
      , ability = Nothing
      , evolutionData = EvolvesFrom 996 "Level 43"
      , transformationData = DoesNotTransform
      }
    , { id = 998
      , nationalDexNumber = 624
      , name = "Pawniard"
      , form = Original
      , typing = Double Dark Steel
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 999
      , nationalDexNumber = 625
      , name = "Bisharp"
      , form = Original
      , typing = Double Dark Steel
      , ability = Nothing
      , evolutionData = EvolvesFrom 998 "Level 52"
      , transformationData = DoesNotTransform
      }
    , { id = 1711
      , nationalDexNumber = 983
      , name = "Kingambit"
      , form = Original
      , typing = Double Dark Steel
      , ability = Nothing
      , evolutionData = EvolvesFrom 999 "Level after defeating three Bisharp that lead a pack of Pawniard"
      , transformationData = DoesNotTransform
      }
    , { id = 1000
      , nationalDexNumber = 626
      , name = "Bouffalant"
      , form = Original
      , typing = Single Normal
      , ability = Just SapSipper
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1001
      , nationalDexNumber = 627
      , name = "Rufflet"
      , form = Original
      , typing = Double Normal Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1002
      , nationalDexNumber = 628
      , name = "Braviary"
      , form = Original
      , typing = Double Normal Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 1001 "Level 54"
      , transformationData = DoesNotTransform
      }
    , { id = 1628
      , nationalDexNumber = 628
      , name = "Braviary"
      , form = Regional 1002 Hisui
      , typing = Double Psychic Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 1001 "Level 54 in Legends: Arceus"
      , transformationData = DoesNotTransform
      }
    , { id = 1003
      , nationalDexNumber = 629
      , name = "Vullaby"
      , form = Original
      , typing = Double Dark Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1005
      , nationalDexNumber = 630
      , name = "Mandibuzz"
      , form = Original
      , typing = Double Dark Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 1003 "Level 54"
      , transformationData = DoesNotTransform
      }
    , { id = 1006
      , nationalDexNumber = 631
      , name = "Heatmor"
      , form = Original
      , typing = Single Fire
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1007
      , nationalDexNumber = 632
      , name = "Durant"
      , form = Original
      , typing = Double Bug Steel
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1008
      , nationalDexNumber = 633
      , name = "Deino"
      , form = Original
      , typing = Double Dark Dragon
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1009
      , nationalDexNumber = 634
      , name = "Zweilous"
      , form = Original
      , typing = Double Dark Dragon
      , ability = Nothing
      , evolutionData = EvolvesFrom 1008 "Level 50"
      , transformationData = DoesNotTransform
      }
    , { id = 1010
      , nationalDexNumber = 635
      , name = "Hydreigon"
      , form = Original
      , typing = Double Dark Dragon
      , ability = Just Levitate
      , evolutionData = EvolvesFrom 1009 "Level 64"
      , transformationData = DoesNotTransform
      }
    , { id = 1011
      , nationalDexNumber = 636
      , name = "Larvesta"
      , form = Original
      , typing = Double Bug Fire
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1012
      , nationalDexNumber = 637
      , name = "Volcarona"
      , form = Original
      , typing = Double Bug Fire
      , ability = Nothing
      , evolutionData = EvolvesFrom 1011 "Level 59"
      , transformationData = DoesNotTransform
      }
    , { id = 1013
      , nationalDexNumber = 638
      , name = "Cobalion"
      , form = Original
      , typing = Double Steel Fighting
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1014
      , nationalDexNumber = 639
      , name = "Terrakion"
      , form = Original
      , typing = Double Rock Fighting
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1016
      , nationalDexNumber = 640
      , name = "Virizion"
      , form = Original
      , typing = Double Grass Fighting
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1017
      , nationalDexNumber = 641
      , name = "Tornadus"
      , form = Original
      , typing = Single Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 11 "Use the Reveal Glass"
      }
    , { id = 1415
      , nationalDexNumber = 641
      , name = "Tornadus"
      , form = Unique "Therian"
      , typing = Single Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 11 "Use the Reveal Glass"
      }
    , { id = 1018
      , nationalDexNumber = 642
      , name = "Thundurus"
      , form = Original
      , typing = Double Electric Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 12 "Use the Reveal Glass"
      }
    , { id = 1416
      , nationalDexNumber = 642
      , name = "Thundurus"
      , form = Unique "Therian"
      , typing = Double Electric Flying
      , ability = Just VoltAbsorb
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 12 "Use the Reveal Glass"
      }
    , { id = 1019
      , nationalDexNumber = 643
      , name = "Reshiram"
      , form = Original
      , typing = Double Dragon Fire
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1020
      , nationalDexNumber = 644
      , name = "Zekrom"
      , form = Original
      , typing = Double Dragon Electric
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1021
      , nationalDexNumber = 645
      , name = "Landorus"
      , form = Original
      , typing = Double Ground Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 13 "Use the Reveal Glass"
      }
    , { id = 1417
      , nationalDexNumber = 645
      , name = "Landorus"
      , form = Unique "Therian"
      , typing = Double Ground Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 13 "Use the Reveal Glass"
      }
    , { id = 1022
      , nationalDexNumber = 646
      , name = "Kyurem"
      , form = Original
      , typing = Double Dragon Ice
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 14 "Undo DNA Splicing"
      }
    , { id = 1421
      , nationalDexNumber = 646
      , name = "Kyurem"
      , form = Unique "White"
      , typing = Double Dragon Ice
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 14 "DNA Splice with Reshiram"
      }
    , { id = 1423
      , nationalDexNumber = 646
      , name = "Kyurem"
      , form = Unique "Black"
      , typing = Double Dragon Ice
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 14 "DNA Splice with Zekrom"
      }
    , { id = 1023
      , nationalDexNumber = 647
      , name = "Keldeo"
      , form = Original
      , typing = Double Water Fighting
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1024
      , nationalDexNumber = 648
      , name = "Meloetta"
      , form = Original
      , typing = Double Normal Psychic
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 15 "Use the move Relic Song"
      }
    , { id = 1570
      , nationalDexNumber = 648
      , name = "Meloetta"
      , form = Unique "Pirouette"
      , typing = Double Normal Fighting
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 15 "Use the move Relic Song"
      }
    , { id = 1025
      , nationalDexNumber = 649
      , name = "Genesect"
      , form = Original
      , typing = Double Bug Steel
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1066
      , nationalDexNumber = 650
      , name = "Chespin"
      , form = Original
      , typing = Single Grass
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1067
      , nationalDexNumber = 651
      , name = "Quilladin"
      , form = Original
      , typing = Single Grass
      , ability = Nothing
      , evolutionData = EvolvesFrom 1066 "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 1068
      , nationalDexNumber = 652
      , name = "Chesnaught"
      , form = Original
      , typing = Double Grass Fighting
      , ability = Nothing
      , evolutionData = EvolvesFrom 1067 "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 1069
      , nationalDexNumber = 653
      , name = "Fennekin"
      , form = Original
      , typing = Single Fire
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1070
      , nationalDexNumber = 654
      , name = "Braixen"
      , form = Original
      , typing = Single Fire
      , ability = Nothing
      , evolutionData = EvolvesFrom 1069 "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 1071
      , nationalDexNumber = 655
      , name = "Delphox"
      , form = Original
      , typing = Double Fire Psychic
      , ability = Nothing
      , evolutionData = EvolvesFrom 1070 "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 1072
      , nationalDexNumber = 656
      , name = "Froakie"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1073
      , nationalDexNumber = 657
      , name = "Frogadier"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = EvolvesFrom 1072 "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 1074
      , nationalDexNumber = 658
      , name = "Greninja"
      , form = Original
      , typing = Double Water Dark
      , ability = Nothing
      , evolutionData = EvolvesFrom 1073 "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 1075
      , nationalDexNumber = 659
      , name = "Bunnelby"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1077
      , nationalDexNumber = 660
      , name = "Diggersby"
      , form = Original
      , typing = Double Normal Ground
      , ability = Nothing
      , evolutionData = EvolvesFrom 1075 "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 1078
      , nationalDexNumber = 661
      , name = "Fletchling"
      , form = Original
      , typing = Double Normal Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1079
      , nationalDexNumber = 662
      , name = "Fletchinder"
      , form = Original
      , typing = Double Fire Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 1078 "Level 17"
      , transformationData = DoesNotTransform
      }
    , { id = 1080
      , nationalDexNumber = 663
      , name = "Talonflame"
      , form = Original
      , typing = Double Fire Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 1079 "Level 35"
      , transformationData = DoesNotTransform
      }
    , { id = 1081
      , nationalDexNumber = 664
      , name = "Scatterbug"
      , form = Original
      , typing = Single Bug
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1082
      , nationalDexNumber = 665
      , name = "Spewpa"
      , form = Original
      , typing = Single Bug
      , ability = Nothing
      , evolutionData = EvolvesFrom 1081 "Level 9"
      , transformationData = DoesNotTransform
      }
    , { id = 1083
      , nationalDexNumber = 666
      , name = "Vivillon"
      , form = Original
      , typing = Double Bug Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 1082 "Level 12"
      , transformationData = DoesNotTransform
      }
    , { id = 1084
      , nationalDexNumber = 667
      , name = "Litleo"
      , form = Original
      , typing = Double Fire Normal
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1085
      , nationalDexNumber = 668
      , name = "Pyroar"
      , form = Original
      , typing = Double Fire Normal
      , ability = Nothing
      , evolutionData = EvolvesFrom 1084 "Level 35"
      , transformationData = DoesNotTransform
      }
    , { id = 1086
      , nationalDexNumber = 669
      , name = "Flabébé"
      , form = Original
      , typing = Single Fairy
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1088
      , nationalDexNumber = 670
      , name = "Floette"
      , form = Original
      , typing = Single Fairy
      , ability = Nothing
      , evolutionData = EvolvesFrom 1086 "Level 19"
      , transformationData = DoesNotTransform
      }
    , { id = 1089
      , nationalDexNumber = 671
      , name = "Florges"
      , form = Original
      , typing = Single Fairy
      , ability = Nothing
      , evolutionData = EvolvesFrom 1088 "Use Shiny Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 1090
      , nationalDexNumber = 672
      , name = "Skiddo"
      , form = Original
      , typing = Single Grass
      , ability = Just SapSipper
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1091
      , nationalDexNumber = 673
      , name = "Gogoat"
      , form = Original
      , typing = Single Grass
      , ability = Just SapSipper
      , evolutionData = EvolvesFrom 1090 "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 1092
      , nationalDexNumber = 674
      , name = "Pancham"
      , form = Original
      , typing = Single Fighting
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1093
      , nationalDexNumber = 675
      , name = "Pangoro"
      , form = Original
      , typing = Double Fighting Dark
      , ability = Nothing
      , evolutionData = EvolvesFrom 1092 "Level 32 With Dark-Type Pokemon In Party"
      , transformationData = DoesNotTransform
      }
    , { id = 1094
      , nationalDexNumber = 676
      , name = "Furfrou"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1095
      , nationalDexNumber = 677
      , name = "Espurr"
      , form = Original
      , typing = Single Psychic
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1096
      , nationalDexNumber = 678
      , name = "Meowstic"
      , form = Original
      , typing = Single Psychic
      , ability = Nothing
      , evolutionData = EvolvesFrom 1095 "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 25
      , nationalDexNumber = 679
      , name = "Honedge"
      , form = Original
      , typing = Double Steel Ghost
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 26
      , nationalDexNumber = 680
      , name = "Doublade"
      , form = Original
      , typing = Double Steel Ghost
      , ability = Nothing
      , evolutionData = EvolvesFrom 25 "Level 35"
      , transformationData = DoesNotTransform
      }
    , { id = 1098
      , nationalDexNumber = 681
      , name = "Aegislash"
      , form = Unique "Shield"
      , typing = Double Steel Ghost
      , ability = Nothing
      , evolutionData = EvolvesFrom 26 "Use Dusk Stone"
      , transformationData = Transforms 16 "Use the move King's Shield"
      }
    , { id = 1370
      , nationalDexNumber = 681
      , name = "Aegislash"
      , form = Unique "Blade"
      , typing = Double Steel Ghost
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 16 "Use a damaging move"
      }
    , { id = 1101
      , nationalDexNumber = 682
      , name = "Spritzee"
      , form = Original
      , typing = Single Fairy
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1102
      , nationalDexNumber = 683
      , name = "Aromatisse"
      , form = Original
      , typing = Single Fairy
      , ability = Nothing
      , evolutionData = EvolvesFrom 1101 "Trade holding Sachet"
      , transformationData = DoesNotTransform
      }
    , { id = 1103
      , nationalDexNumber = 684
      , name = "Swirlix"
      , form = Original
      , typing = Single Fairy
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1104
      , nationalDexNumber = 685
      , name = "Slurpuff"
      , form = Original
      , typing = Single Fairy
      , ability = Nothing
      , evolutionData = EvolvesFrom 1103 "Trade holding Whipped Dream"
      , transformationData = DoesNotTransform
      }
    , { id = 1105
      , nationalDexNumber = 686
      , name = "Inkay"
      , form = Original
      , typing = Double Dark Psychic
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1106
      , nationalDexNumber = 687
      , name = "Malamar"
      , form = Original
      , typing = Double Dark Psychic
      , ability = Nothing
      , evolutionData = EvolvesFrom 1105 "Level 30 With System Or Controller Upside-Down"
      , transformationData = DoesNotTransform
      }
    , { id = 1107
      , nationalDexNumber = 688
      , name = "Binacle"
      , form = Original
      , typing = Double Rock Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1108
      , nationalDexNumber = 689
      , name = "Barbaracle"
      , form = Original
      , typing = Double Rock Water
      , ability = Nothing
      , evolutionData = EvolvesFrom 1107 "Level 39"
      , transformationData = DoesNotTransform
      }
    , { id = 1110
      , nationalDexNumber = 690
      , name = "Skrelp"
      , form = Original
      , typing = Double Poison Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1111
      , nationalDexNumber = 691
      , name = "Dragalge"
      , form = Original
      , typing = Double Poison Dragon
      , ability = Nothing
      , evolutionData = EvolvesFrom 1110 "Level 48"
      , transformationData = DoesNotTransform
      }
    , { id = 1112
      , nationalDexNumber = 692
      , name = "Clauncher"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1113
      , nationalDexNumber = 693
      , name = "Clawitzer"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = EvolvesFrom 1112 "Level 37"
      , transformationData = DoesNotTransform
      }
    , { id = 1122
      , nationalDexNumber = 694
      , name = "Helioptile"
      , form = Original
      , typing = Double Electric Normal
      , ability = Just DrySkin
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1123
      , nationalDexNumber = 695
      , name = "Heliolisk"
      , form = Original
      , typing = Double Electric Normal
      , ability = Just DrySkin
      , evolutionData = EvolvesFrom 1122 "Use Sun Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 1124
      , nationalDexNumber = 696
      , name = "Tyrunt"
      , form = Original
      , typing = Double Rock Dragon
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1125
      , nationalDexNumber = 697
      , name = "Tyrantrum"
      , form = Original
      , typing = Double Rock Dragon
      , ability = Nothing
      , evolutionData = EvolvesFrom 1124 "Level 39 during the day"
      , transformationData = DoesNotTransform
      }
    , { id = 1126
      , nationalDexNumber = 698
      , name = "Amaura"
      , form = Original
      , typing = Double Rock Ice
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1127
      , nationalDexNumber = 699
      , name = "Aurorus"
      , form = Original
      , typing = Double Rock Ice
      , ability = Nothing
      , evolutionData = EvolvesFrom 1126 "Level 39 at night"
      , transformationData = DoesNotTransform
      }
    , { id = 1130
      , nationalDexNumber = 701
      , name = "Hawlucha"
      , form = Original
      , typing = Double Fighting Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1131
      , nationalDexNumber = 702
      , name = "Dedenne"
      , form = Original
      , typing = Double Electric Fairy
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1132
      , nationalDexNumber = 703
      , name = "Carbink"
      , form = Original
      , typing = Double Rock Fairy
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1133
      , nationalDexNumber = 704
      , name = "Goomy"
      , form = Original
      , typing = Single Dragon
      , ability = Just SapSipper
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1134
      , nationalDexNumber = 705
      , name = "Sliggoo"
      , form = Original
      , typing = Single Dragon
      , ability = Just SapSipper
      , evolutionData = EvolvesFrom 1133 "Level 40"
      , transformationData = DoesNotTransform
      }
    , { id = 1135
      , nationalDexNumber = 706
      , name = "Goodra"
      , form = Original
      , typing = Single Dragon
      , ability = Just SapSipper
      , evolutionData = EvolvesFrom 1134 "Level 50 When Raining or Foggy outside battle"
      , transformationData = DoesNotTransform
      }
    , { id = 1629
      , nationalDexNumber = 705
      , name = "Sliggoo"
      , form = Regional 1134 Hisui
      , typing = Double Dragon Steel
      , ability = Just SapSipper
      , evolutionData = EvolvesFrom 1133 "Level 40 in Legends: Arceus"
      , transformationData = DoesNotTransform
      }
    , { id = 1630
      , nationalDexNumber = 706
      , name = "Goodra"
      , form = Regional 1135 Hisui
      , typing = Double Dragon Steel
      , ability = Just SapSipper
      , evolutionData = EvolvesFrom 1629 "Level 50 When Raining or Foggy outside battle"
      , transformationData = DoesNotTransform
      }
    , { id = 1136
      , nationalDexNumber = 707
      , name = "Klefki"
      , form = Original
      , typing = Double Steel Fairy
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1137
      , nationalDexNumber = 708
      , name = "Phantump"
      , form = Original
      , typing = Double Ghost Grass
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1138
      , nationalDexNumber = 709
      , name = "Trevenant"
      , form = Original
      , typing = Double Ghost Grass
      , ability = Nothing
      , evolutionData = EvolvesFrom 1137 "Trade"
      , transformationData = DoesNotTransform
      }
    , { id = 1140
      , nationalDexNumber = 710
      , name = "Pumpkaboo"
      , form = Original
      , typing = Double Ghost Grass
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1141
      , nationalDexNumber = 711
      , name = "Gourgeist"
      , form = Original
      , typing = Double Ghost Grass
      , ability = Nothing
      , evolutionData = EvolvesFrom 1140 "Trade"
      , transformationData = DoesNotTransform
      }
    , { id = 1142
      , nationalDexNumber = 712
      , name = "Bergmite"
      , form = Original
      , typing = Single Ice
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1143
      , nationalDexNumber = 713
      , name = "Avalugg"
      , form = Original
      , typing = Single Ice
      , ability = Nothing
      , evolutionData = EvolvesFrom 1142 "Level 37"
      , transformationData = DoesNotTransform
      }
    , { id = 1631
      , nationalDexNumber = 713
      , name = "Avalugg"
      , form = Regional 1143 Hisui
      , typing = Double Ice Rock
      , ability = Nothing
      , evolutionData = EvolvesFrom 1142 "Level 37 in Legends: Arceus"
      , transformationData = DoesNotTransform
      }
    , { id = 1144
      , nationalDexNumber = 714
      , name = "Noibat"
      , form = Original
      , typing = Double Flying Dragon
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1145
      , nationalDexNumber = 715
      , name = "Noivern"
      , form = Original
      , typing = Double Flying Dragon
      , ability = Nothing
      , evolutionData = EvolvesFrom 1144 "Level 48"
      , transformationData = DoesNotTransform
      }
    , { id = 1146
      , nationalDexNumber = 716
      , name = "Xerneas"
      , form = Original
      , typing = Single Fairy
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1147
      , nationalDexNumber = 717
      , name = "Yveltal"
      , form = Original
      , typing = Double Dark Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1577
      , nationalDexNumber = 718
      , name = "Zygarde"
      , form = Unique "10%"
      , typing = Double Dragon Ground
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1148
      , nationalDexNumber = 718
      , name = "Zygarde"
      , form = Original
      , typing = Double Dragon Ground
      , ability = Nothing
      , evolutionData = EvolvesFrom 1577 "Collect 50% of Zygarde Cells"
      , transformationData = Transforms 17 "At the end of battle"
      }
    , { id = 1578
      , nationalDexNumber = 718
      , name = "Zygarde"
      , form = Unique "Complete"
      , typing = Double Dragon Ground
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 17 "If HP is below half"
      }
    , { id = 1149
      , nationalDexNumber = 719
      , name = "Diancie"
      , form = Original
      , typing = Double Rock Fairy
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1594
      , nationalDexNumber = 719
      , name = "Diancie"
      , form = Mega
      , typing = Double Rock Fairy
      , ability = Nothing
      , evolutionData = EvolvesFrom 1149 "Holding Diancite"
      , transformationData = DoesNotTransform
      }
    , { id = 1151
      , nationalDexNumber = 720
      , name = "Hoopa"
      , form = Original
      , typing = Double Psychic Ghost
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 18 "After 3 days"
      }
    , { id = 1579
      , nationalDexNumber = 720
      , name = "Hoopa"
      , form = Unique "Unbound"
      , typing = Double Psychic Dark
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 18 "Use Prison Bottle"
      }
    , { id = 1152
      , nationalDexNumber = 721
      , name = "Volcanion"
      , form = Original
      , typing = Double Fire Water
      , ability = Just WaterAbsorb
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1246
      , nationalDexNumber = 722
      , name = "Rowlet"
      , form = Original
      , typing = Double Grass Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1247
      , nationalDexNumber = 723
      , name = "Dartrix"
      , form = Original
      , typing = Double Grass Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 1246 "Level 17"
      , transformationData = DoesNotTransform
      }
    , { id = 1248
      , nationalDexNumber = 724
      , name = "Decidueye"
      , form = Original
      , typing = Double Grass Ghost
      , ability = Nothing
      , evolutionData = EvolvesFrom 1247 "Level 34"
      , transformationData = DoesNotTransform
      }
    , { id = 1632
      , nationalDexNumber = 724
      , name = "Decidueye"
      , form = Regional 1248 Hisui
      , typing = Double Grass Fighting
      , ability = Nothing
      , evolutionData = EvolvesFrom 1247 "Level 36 in Legends: Arceus"
      , transformationData = DoesNotTransform
      }
    , { id = 1249
      , nationalDexNumber = 725
      , name = "Litten"
      , form = Original
      , typing = Single Fire
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1250
      , nationalDexNumber = 726
      , name = "Torracat"
      , form = Original
      , typing = Single Fire
      , ability = Nothing
      , evolutionData = EvolvesFrom 1249 "Level 17"
      , transformationData = DoesNotTransform
      }
    , { id = 1251
      , nationalDexNumber = 727
      , name = "Incineroar"
      , form = Original
      , typing = Double Fire Dark
      , ability = Nothing
      , evolutionData = EvolvesFrom 1250 "Level 34"
      , transformationData = DoesNotTransform
      }
    , { id = 1252
      , nationalDexNumber = 728
      , name = "Popplio"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1253
      , nationalDexNumber = 729
      , name = "Brionne"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = EvolvesFrom 1252 "Level 17"
      , transformationData = DoesNotTransform
      }
    , { id = 1255
      , nationalDexNumber = 730
      , name = "Primarina"
      , form = Original
      , typing = Double Water Fairy
      , ability = Nothing
      , evolutionData = EvolvesFrom 1253 "Level 34"
      , transformationData = DoesNotTransform
      }
    , { id = 1256
      , nationalDexNumber = 731
      , name = "Pikipek"
      , form = Original
      , typing = Double Normal Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1257
      , nationalDexNumber = 732
      , name = "Trumbeak"
      , form = Original
      , typing = Double Normal Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 1256 "Level 14"
      , transformationData = DoesNotTransform
      }
    , { id = 1258
      , nationalDexNumber = 733
      , name = "Toucannon"
      , form = Original
      , typing = Double Normal Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 1257 "Level 28"
      , transformationData = DoesNotTransform
      }
    , { id = 1259
      , nationalDexNumber = 734
      , name = "Yungoos"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1260
      , nationalDexNumber = 735
      , name = "Gumshoos"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = EvolvesFrom 1259 "Level 20 During The Day"
      , transformationData = DoesNotTransform
      }
    , { id = 1261
      , nationalDexNumber = 736
      , name = "Grubbin"
      , form = Original
      , typing = Single Bug
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1262
      , nationalDexNumber = 737
      , name = "Charjabug"
      , form = Original
      , typing = Double Bug Electric
      , ability = Nothing
      , evolutionData = EvolvesFrom 1261 "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 1263
      , nationalDexNumber = 738
      , name = "Vikavolt"
      , form = Original
      , typing = Double Bug Electric
      , ability = Just Levitate
      , evolutionData = EvolvesFrom 1262 "Use Thunder Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 1264
      , nationalDexNumber = 739
      , name = "Crabrawler"
      , form = Original
      , typing = Single Fighting
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1266
      , nationalDexNumber = 740
      , name = "Crabominable"
      , form = Original
      , typing = Double Fighting Ice
      , ability = Nothing
      , evolutionData = EvolvesFrom 1264 "Level at Mount Lanakila in Alola"
      , transformationData = DoesNotTransform
      }
    , { id = 1267
      , nationalDexNumber = 741
      , name = "Oricorio"
      , form = Unique "Baile"
      , typing = Double Fire Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 19 "Use Red Nectar"
      }
    , { id = 1582
      , nationalDexNumber = 741
      , name = "Oricorio"
      , form = Unique "Pom-Pom"
      , typing = Double Electric Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 19 "Use Yellow Nectar"
      }
    , { id = 1583
      , nationalDexNumber = 741
      , name = "Oricorio"
      , form = Unique "Pa'u"
      , typing = Double Psychic Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 19 "Use Pink Nectar"
      }
    , { id = 1584
      , nationalDexNumber = 741
      , name = "Oricorio"
      , form = Unique "Sensu"
      , typing = Double Ghost Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 19 "Use Purple Nectar"
      }
    , { id = 1268
      , nationalDexNumber = 742
      , name = "Cutiefly"
      , form = Original
      , typing = Double Bug Fairy
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1269
      , nationalDexNumber = 743
      , name = "Ribombee"
      , form = Original
      , typing = Double Bug Fairy
      , ability = Nothing
      , evolutionData = EvolvesFrom 1268 "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 1270
      , nationalDexNumber = 744
      , name = "Rockruff"
      , form = Original
      , typing = Single Rock
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1271
      , nationalDexNumber = 745
      , name = "Lycanroc"
      , form = Original
      , typing = Single Rock
      , ability = Nothing
      , evolutionData = EvolvesFrom 1270 "Level 25 during the day"
      , transformationData = DoesNotTransform
      }
    , { id = 1403
      , nationalDexNumber = 745
      , name = "Lycanroc"
      , form = Unique "Dusk"
      , typing = Single Rock
      , ability = Nothing
      , evolutionData = EvolvesFrom 1270 "Level 25 with Own Tempo between 5-6pm"
      , transformationData = DoesNotTransform
      }
    , { id = 1402
      , nationalDexNumber = 745
      , name = "Lycanroc"
      , form = Unique "Midnight"
      , typing = Single Rock
      , ability = Nothing
      , evolutionData = EvolvesFrom 1270 "Level 25 at night"
      , transformationData = DoesNotTransform
      }
    , { id = 1272
      , nationalDexNumber = 746
      , name = "Wishiwashi"
      , form = Unique "Solo"
      , typing = Single Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 20 "If HP below 25%"
      }
    , { id = 1581
      , nationalDexNumber = 746
      , name = "Wishiwashi"
      , form = Unique "School"
      , typing = Single Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 20 "If HP above 25% and level 20+"
      }
    , { id = 1273
      , nationalDexNumber = 747
      , name = "Mareanie"
      , form = Original
      , typing = Double Poison Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1274
      , nationalDexNumber = 748
      , name = "Toxapex"
      , form = Original
      , typing = Double Poison Water
      , ability = Nothing
      , evolutionData = EvolvesFrom 1273 "Level 38"
      , transformationData = DoesNotTransform
      }
    , { id = 1275
      , nationalDexNumber = 749
      , name = "Mudbray"
      , form = Original
      , typing = Single Ground
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1277
      , nationalDexNumber = 750
      , name = "Mudsdale"
      , form = Original
      , typing = Single Ground
      , ability = Nothing
      , evolutionData = EvolvesFrom 1275 "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 1278
      , nationalDexNumber = 751
      , name = "Dewpider"
      , form = Original
      , typing = Double Water Bug
      , ability = Just WaterBubble
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1279
      , nationalDexNumber = 752
      , name = "Araquanid"
      , form = Original
      , typing = Double Water Bug
      , ability = Just WaterBubble
      , evolutionData = EvolvesFrom 1278 "Level 22"
      , transformationData = DoesNotTransform
      }
    , { id = 1280
      , nationalDexNumber = 753
      , name = "Fomantis"
      , form = Original
      , typing = Single Grass
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1281
      , nationalDexNumber = 754
      , name = "Lurantis"
      , form = Original
      , typing = Single Grass
      , ability = Nothing
      , evolutionData = EvolvesFrom 1280 "Level 34 During The Day"
      , transformationData = DoesNotTransform
      }
    , { id = 1282
      , nationalDexNumber = 755
      , name = "Morelull"
      , form = Original
      , typing = Double Grass Fairy
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1283
      , nationalDexNumber = 756
      , name = "Shiinotic"
      , form = Original
      , typing = Double Grass Fairy
      , ability = Nothing
      , evolutionData = EvolvesFrom 1282 "Level 24"
      , transformationData = DoesNotTransform
      }
    , { id = 1284
      , nationalDexNumber = 757
      , name = "Salandit"
      , form = Original
      , typing = Double Poison Fire
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1285
      , nationalDexNumber = 758
      , name = "Salazzle"
      , form = Original
      , typing = Double Poison Fire
      , ability = Nothing
      , evolutionData = EvolvesFrom 1284 "Level 33 When Female"
      , transformationData = DoesNotTransform
      }
    , { id = 1286
      , nationalDexNumber = 759
      , name = "Stufful"
      , form = Original
      , typing = Double Normal Fighting
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1288
      , nationalDexNumber = 760
      , name = "Bewear"
      , form = Original
      , typing = Double Normal Fighting
      , ability = Nothing
      , evolutionData = EvolvesFrom 1286 "Level 27"
      , transformationData = DoesNotTransform
      }
    , { id = 1289
      , nationalDexNumber = 761
      , name = "Bounsweet"
      , form = Original
      , typing = Single Grass
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1290
      , nationalDexNumber = 762
      , name = "Steenee"
      , form = Original
      , typing = Single Grass
      , ability = Nothing
      , evolutionData = EvolvesFrom 1289 "Level 18"
      , transformationData = DoesNotTransform
      }
    , { id = 1291
      , nationalDexNumber = 763
      , name = "Tsareena"
      , form = Original
      , typing = Single Grass
      , ability = Nothing
      , evolutionData = EvolvesFrom 1290 "Level while knowing Stomp"
      , transformationData = DoesNotTransform
      }
    , { id = 1292
      , nationalDexNumber = 764
      , name = "Comfey"
      , form = Original
      , typing = Single Fairy
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 743
      , nationalDexNumber = 765
      , name = "Oranguru"
      , form = Original
      , typing = Double Normal Psychic
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1293
      , nationalDexNumber = 766
      , name = "Passimian"
      , form = Original
      , typing = Single Fighting
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1294
      , nationalDexNumber = 767
      , name = "Wimpod"
      , form = Original
      , typing = Double Bug Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1295
      , nationalDexNumber = 768
      , name = "Golisopod"
      , form = Original
      , typing = Double Bug Water
      , ability = Nothing
      , evolutionData = EvolvesFrom 1294 "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 1296
      , nationalDexNumber = 769
      , name = "Sandygast"
      , form = Original
      , typing = Double Ghost Ground
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1298
      , nationalDexNumber = 770
      , name = "Palossand"
      , form = Original
      , typing = Double Ghost Ground
      , ability = Nothing
      , evolutionData = EvolvesFrom 1296 "Level 42"
      , transformationData = DoesNotTransform
      }
    , { id = 1299
      , nationalDexNumber = 771
      , name = "Pyukumuku"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1300
      , nationalDexNumber = 772
      , name = "Type: Null"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1301
      , nationalDexNumber = 773
      , name = "Silvally"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = EvolvesFrom 1300 "Level with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 1302
      , nationalDexNumber = 774
      , name = "Minior"
      , form = Original
      , typing = Double Rock Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 21 "If HP is above half"
      }
    , { id = 1564
      , nationalDexNumber = 774
      , name = "Minior"
      , form = Unique "Core"
      , typing = Double Rock Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 21 "If HP is below half"
      }
    , { id = 1303
      , nationalDexNumber = 775
      , name = "Komala"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1304
      , nationalDexNumber = 776
      , name = "Turtonator"
      , form = Original
      , typing = Double Fire Dragon
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1305
      , nationalDexNumber = 777
      , name = "Togedemaru"
      , form = Original
      , typing = Double Electric Steel
      , ability = Just LightningRod
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1306
      , nationalDexNumber = 778
      , name = "Mimikyu"
      , form = Original
      , typing = Double Ghost Fairy
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1307
      , nationalDexNumber = 779
      , name = "Bruxish"
      , form = Original
      , typing = Double Water Psychic
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1309
      , nationalDexNumber = 780
      , name = "Drampa"
      , form = Original
      , typing = Double Normal Dragon
      , ability = Just SapSipper
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1310
      , nationalDexNumber = 781
      , name = "Dhelmise"
      , form = Original
      , typing = Double Ghost Grass
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1311
      , nationalDexNumber = 782
      , name = "Jangmo-o"
      , form = Original
      , typing = Single Dragon
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1312
      , nationalDexNumber = 783
      , name = "Hakamo-o"
      , form = Original
      , typing = Double Dragon Fighting
      , ability = Nothing
      , evolutionData = EvolvesFrom 1311 "Level 35"
      , transformationData = DoesNotTransform
      }
    , { id = 1313
      , nationalDexNumber = 784
      , name = "Kommo-o"
      , form = Original
      , typing = Double Dragon Fighting
      , ability = Nothing
      , evolutionData = EvolvesFrom 1312 "Level 45"
      , transformationData = DoesNotTransform
      }
    , { id = 1314
      , nationalDexNumber = 785
      , name = "Tapu Koko"
      , form = Original
      , typing = Double Electric Fairy
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1315
      , nationalDexNumber = 786
      , name = "Tapu Lele"
      , form = Original
      , typing = Double Psychic Fairy
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1316
      , nationalDexNumber = 787
      , name = "Tapu Bulu"
      , form = Original
      , typing = Double Grass Fairy
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1317
      , nationalDexNumber = 788
      , name = "Tapu Fini"
      , form = Original
      , typing = Double Water Fairy
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1318
      , nationalDexNumber = 789
      , name = "Cosmog"
      , form = Original
      , typing = Single Psychic
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1320
      , nationalDexNumber = 790
      , name = "Cosmoem"
      , form = Original
      , typing = Single Psychic
      , ability = Nothing
      , evolutionData = EvolvesFrom 1318 "Level 43"
      , transformationData = DoesNotTransform
      }
    , { id = 1321
      , nationalDexNumber = 791
      , name = "Solgaleo"
      , form = Original
      , typing = Double Psychic Steel
      , ability = Nothing
      , evolutionData = EvolvesFrom 1320 "Level 53 In Sun Or Ultra Sun"
      , transformationData = DoesNotTransform
      }
    , { id = 1322
      , nationalDexNumber = 792
      , name = "Lunala"
      , form = Original
      , typing = Double Psychic Ghost
      , ability = Nothing
      , evolutionData = EvolvesFrom 1320 "Level 53 In Moon Or Ultra Moon"
      , transformationData = DoesNotTransform
      }
    , { id = 1323
      , nationalDexNumber = 793
      , name = "Nihilego"
      , form = Original
      , typing = Double Rock Poison
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1324
      , nationalDexNumber = 794
      , name = "Buzzwole"
      , form = Original
      , typing = Double Bug Fighting
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1325
      , nationalDexNumber = 795
      , name = "Pheromosa"
      , form = Original
      , typing = Double Bug Fighting
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1326
      , nationalDexNumber = 796
      , name = "Xurkitree"
      , form = Original
      , typing = Single Electric
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1327
      , nationalDexNumber = 797
      , name = "Celesteela"
      , form = Original
      , typing = Double Steel Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1328
      , nationalDexNumber = 798
      , name = "Kartana"
      , form = Original
      , typing = Double Grass Steel
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1329
      , nationalDexNumber = 799
      , name = "Guzzlord"
      , form = Original
      , typing = Double Dark Dragon
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1331
      , nationalDexNumber = 800
      , name = "Necrozma"
      , form = Original
      , typing = Single Psychic
      , ability = Just PrismArmor
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 22 "Undo fusion"
      }
    , { id = 1404
      , nationalDexNumber = 800
      , name = "Necrozma"
      , form = Unique "Dusk Mane"
      , typing = Double Psychic Steel
      , ability = Just PrismArmor
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 22 "Fuze with Solgaleo"
      }
    , { id = 1405
      , nationalDexNumber = 800
      , name = "Necrozma"
      , form = Unique "Dawn Wings"
      , typing = Double Psychic Ghost
      , ability = Just PrismArmor
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 22 "Fuze with Lunala"
      }
    , { id = 1406
      , nationalDexNumber = 800
      , name = "Necrozma"
      , form = Unique "Ultra"
      , typing = Double Psychic Dragon
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 22 "Use Ultra Burst"
      }
    , { id = 1332
      , nationalDexNumber = 801
      , name = "Magearna"
      , form = Original
      , typing = Double Steel Fairy
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1333
      , nationalDexNumber = 802
      , name = "Marshadow"
      , form = Original
      , typing = Double Fighting Ghost
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1334
      , nationalDexNumber = 803
      , name = "Poipole"
      , form = Original
      , typing = Single Poison
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1335
      , nationalDexNumber = 804
      , name = "Naganadel"
      , form = Original
      , typing = Double Poison Dragon
      , ability = Nothing
      , evolutionData = EvolvesFrom 1334 "Level while knowing Dragon Pulse"
      , transformationData = DoesNotTransform
      }
    , { id = 1336
      , nationalDexNumber = 805
      , name = "Stakataka"
      , form = Original
      , typing = Double Rock Steel
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1337
      , nationalDexNumber = 806
      , name = "Blacephalon"
      , form = Original
      , typing = Double Fire Ghost
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1338
      , nationalDexNumber = 807
      , name = "Zeraora"
      , form = Original
      , typing = Single Electric
      , ability = Just VoltAbsorb
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1339
      , nationalDexNumber = 808
      , name = "Meltan"
      , form = Original
      , typing = Single Steel
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1340
      , nationalDexNumber = 809
      , name = "Melmetal"
      , form = Original
      , typing = Single Steel
      , ability = Nothing
      , evolutionData = EvolvesFrom 1339 "400 Meltan Candy (Pokemon GO only)"
      , transformationData = DoesNotTransform
      }
    , { id = 1439
      , nationalDexNumber = 810
      , name = "Grookey"
      , form = Original
      , typing = Single Grass
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1440
      , nationalDexNumber = 811
      , name = "Thwackey"
      , form = Original
      , typing = Single Grass
      , ability = Nothing
      , evolutionData = EvolvesFrom 1439 "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 1441
      , nationalDexNumber = 812
      , name = "Rillaboom"
      , form = Original
      , typing = Single Grass
      , ability = Nothing
      , evolutionData = EvolvesFrom 1440 "Level 35"
      , transformationData = DoesNotTransform
      }
    , { id = 1442
      , nationalDexNumber = 813
      , name = "Scorbunny"
      , form = Original
      , typing = Single Fire
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1443
      , nationalDexNumber = 814
      , name = "Raboot"
      , form = Original
      , typing = Single Fire
      , ability = Nothing
      , evolutionData = EvolvesFrom 1442 "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 1444
      , nationalDexNumber = 815
      , name = "Cinderace"
      , form = Original
      , typing = Single Fire
      , ability = Nothing
      , evolutionData = EvolvesFrom 1443 "Level 35"
      , transformationData = DoesNotTransform
      }
    , { id = 1445
      , nationalDexNumber = 816
      , name = "Sobble"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1446
      , nationalDexNumber = 817
      , name = "Drizzile"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = EvolvesFrom 1445 "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 1447
      , nationalDexNumber = 818
      , name = "Inteleon"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = EvolvesFrom 1446 "Level 35"
      , transformationData = DoesNotTransform
      }
    , { id = 1462
      , nationalDexNumber = 819
      , name = "Skwovet"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1463
      , nationalDexNumber = 820
      , name = "Greedent"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = EvolvesFrom 1462 "Level 24"
      , transformationData = DoesNotTransform
      }
    , { id = 1469
      , nationalDexNumber = 821
      , name = "Rookidee"
      , form = Original
      , typing = Single Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1470
      , nationalDexNumber = 822
      , name = "Corvisquire"
      , form = Original
      , typing = Single Flying
      , ability = Nothing
      , evolutionData = EvolvesFrom 1469 "Level 18"
      , transformationData = DoesNotTransform
      }
    , { id = 1471
      , nationalDexNumber = 823
      , name = "Corviknight"
      , form = Original
      , typing = Double Flying Steel
      , ability = Nothing
      , evolutionData = EvolvesFrom 1470 "Level 38"
      , transformationData = DoesNotTransform
      }
    , { id = 1485
      , nationalDexNumber = 824
      , name = "Blipbug"
      , form = Original
      , typing = Single Bug
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1486
      , nationalDexNumber = 825
      , name = "Dottler"
      , form = Original
      , typing = Double Bug Psychic
      , ability = Nothing
      , evolutionData = EvolvesFrom 1485 "Level 10"
      , transformationData = DoesNotTransform
      }
    , { id = 1487
      , nationalDexNumber = 826
      , name = "Orbeetle"
      , form = Original
      , typing = Double Bug Psychic
      , ability = Nothing
      , evolutionData = EvolvesFrom 1486 "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 1488
      , nationalDexNumber = 827
      , name = "Nickit"
      , form = Original
      , typing = Single Dark
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1489
      , nationalDexNumber = 828
      , name = "Thievul"
      , form = Original
      , typing = Single Dark
      , ability = Nothing
      , evolutionData = EvolvesFrom 1488 "Level 18"
      , transformationData = DoesNotTransform
      }
    , { id = 1490
      , nationalDexNumber = 829
      , name = "Gossifleur"
      , form = Original
      , typing = Single Grass
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1491
      , nationalDexNumber = 830
      , name = "Eldegoss"
      , form = Original
      , typing = Single Grass
      , ability = Nothing
      , evolutionData = EvolvesFrom 1490 "Level 20"
      , transformationData = DoesNotTransform
      }
    , { id = 1492
      , nationalDexNumber = 831
      , name = "Wooloo"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1493
      , nationalDexNumber = 832
      , name = "Dubwool"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = EvolvesFrom 1492 "Level 24"
      , transformationData = DoesNotTransform
      }
    , { id = 1494
      , nationalDexNumber = 833
      , name = "Chewtle"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1495
      , nationalDexNumber = 834
      , name = "Drednaw"
      , form = Original
      , typing = Double Water Rock
      , ability = Nothing
      , evolutionData = EvolvesFrom 1494 "Level 22"
      , transformationData = DoesNotTransform
      }
    , { id = 1496
      , nationalDexNumber = 835
      , name = "Yamper"
      , form = Original
      , typing = Single Electric
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1497
      , nationalDexNumber = 836
      , name = "Boltund"
      , form = Original
      , typing = Single Electric
      , ability = Nothing
      , evolutionData = EvolvesFrom 1496 "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 1498
      , nationalDexNumber = 837
      , name = "Rolycoly"
      , form = Original
      , typing = Single Rock
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1499
      , nationalDexNumber = 838
      , name = "Carkol"
      , form = Original
      , typing = Double Rock Fire
      , ability = Nothing
      , evolutionData = EvolvesFrom 1498 "Level 18"
      , transformationData = DoesNotTransform
      }
    , { id = 1500
      , nationalDexNumber = 839
      , name = "Coalossal"
      , form = Original
      , typing = Double Rock Fire
      , ability = Nothing
      , evolutionData = EvolvesFrom 1499 "Level 34"
      , transformationData = DoesNotTransform
      }
    , { id = 1501
      , nationalDexNumber = 840
      , name = "Applin"
      , form = Original
      , typing = Double Grass Dragon
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1502
      , nationalDexNumber = 841
      , name = "Flapple"
      , form = Original
      , typing = Double Grass Dragon
      , ability = Nothing
      , evolutionData = EvolvesFrom 1501 "Use Tart Apple"
      , transformationData = DoesNotTransform
      }
    , { id = 1503
      , nationalDexNumber = 842
      , name = "Appletun"
      , form = Original
      , typing = Double Grass Dragon
      , ability = Nothing
      , evolutionData = EvolvesFrom 1501 "Use Sweet Apple"
      , transformationData = DoesNotTransform
      }
    , { id = 1504
      , nationalDexNumber = 843
      , name = "Silicobra"
      , form = Original
      , typing = Single Ground
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1505
      , nationalDexNumber = 844
      , name = "Sandaconda"
      , form = Original
      , typing = Single Ground
      , ability = Nothing
      , evolutionData = EvolvesFrom 1504 "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 1506
      , nationalDexNumber = 845
      , name = "Cramorant"
      , form = Original
      , typing = Double Flying Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1507
      , nationalDexNumber = 846
      , name = "Arrokuda"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1508
      , nationalDexNumber = 847
      , name = "Barraskewda"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = EvolvesFrom 1507 "Level 26"
      , transformationData = DoesNotTransform
      }
    , { id = 14
      , nationalDexNumber = 848
      , name = "Toxel"
      , form = Original
      , typing = Double Electric Poison
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 15
      , nationalDexNumber = 849
      , name = "Toxtricity"
      , form = Unique "Amped"
      , typing = Double Electric Poison
      , ability = Nothing
      , evolutionData = EvolvesFrom 14 "Level 30 With An Adamant, Brave, Docile, Hardy, Hasty, Impish, Jolly, Lax, Naive, Naughty, Quirky, Rash, or Sassy Nature"
      , transformationData = DoesNotTransform
      }
    , { id = 16
      , nationalDexNumber = 849
      , name = "Toxtricity"
      , form = Unique "Low Key"
      , typing = Double Electric Poison
      , ability = Nothing
      , evolutionData = EvolvesFrom 14 "Level 30 With A Bashful, Bold, Calm, Careful, Gentle, Lonely, Mild, Modest, Quiet, Relaxed, Serious, or Timid Nature"
      , transformationData = DoesNotTransform
      }
    , { id = 1509
      , nationalDexNumber = 850
      , name = "Sizzlipede"
      , form = Original
      , typing = Double Fire Bug
      , ability = Just FlashFire
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1510
      , nationalDexNumber = 851
      , name = "Centiskorch"
      , form = Original
      , typing = Double Fire Bug
      , ability = Just FlashFire
      , evolutionData = EvolvesFrom 1509 "Level 28"
      , transformationData = DoesNotTransform
      }
    , { id = 1511
      , nationalDexNumber = 852
      , name = "Clobbopus"
      , form = Original
      , typing = Single Fighting
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1512
      , nationalDexNumber = 853
      , name = "Grapploct"
      , form = Original
      , typing = Single Fighting
      , ability = Nothing
      , evolutionData = EvolvesFrom 1511 "Level while knowing Taunt"
      , transformationData = DoesNotTransform
      }
    , { id = 1513
      , nationalDexNumber = 854
      , name = "Sinistea"
      , form = Original
      , typing = Single Ghost
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1514
      , nationalDexNumber = 855
      , name = "Polteageist"
      , form = Original
      , typing = Single Ghost
      , ability = Nothing
      , evolutionData = EvolvesFrom 1513 "Use Cracked Pot when Phony or Chipped Pot when Authentic"
      , transformationData = DoesNotTransform
      }
    , { id = 1515
      , nationalDexNumber = 856
      , name = "Hatenna"
      , form = Original
      , typing = Single Psychic
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1516
      , nationalDexNumber = 857
      , name = "Hattrem"
      , form = Original
      , typing = Single Psychic
      , ability = Nothing
      , evolutionData = EvolvesFrom 1515 "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 1517
      , nationalDexNumber = 858
      , name = "Hatterene"
      , form = Original
      , typing = Double Psychic Fairy
      , ability = Nothing
      , evolutionData = EvolvesFrom 1516 "Level 42"
      , transformationData = DoesNotTransform
      }
    , { id = 1518
      , nationalDexNumber = 859
      , name = "Impidimp"
      , form = Original
      , typing = Double Dark Fairy
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1519
      , nationalDexNumber = 860
      , name = "Morgrem"
      , form = Original
      , typing = Double Dark Fairy
      , ability = Nothing
      , evolutionData = EvolvesFrom 1518 "Level 32"
      , transformationData = DoesNotTransform
      }
    , { id = 1520
      , nationalDexNumber = 861
      , name = "Grimmsnarl"
      , form = Original
      , typing = Double Dark Fairy
      , ability = Nothing
      , evolutionData = EvolvesFrom 1519 "Level 42"
      , transformationData = DoesNotTransform
      }
    , { id = 1483
      , nationalDexNumber = 868
      , name = "Milcery"
      , form = Original
      , typing = Single Fairy
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1484
      , nationalDexNumber = 869
      , name = "Alcremie"
      , form = Original
      , typing = Single Fairy
      , ability = Nothing
      , evolutionData = EvolvesFrom 1483 "While holding a Sweet and its Trainer spins"
      , transformationData = DoesNotTransform
      }
    , { id = 1378
      , nationalDexNumber = 870
      , name = "Falinks"
      , form = Original
      , typing = Single Fighting
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1425
      , nationalDexNumber = 871
      , name = "Pincurchin"
      , form = Original
      , typing = Single Electric
      , ability = Just LightningRod
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1481
      , nationalDexNumber = 872
      , name = "Snom"
      , form = Original
      , typing = Double Ice Bug
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1482
      , nationalDexNumber = 873
      , name = "Frosmoth"
      , form = Original
      , typing = Double Ice Bug
      , ability = Nothing
      , evolutionData = EvolvesFrom 1481 "Level during the night with high friendship"
      , transformationData = DoesNotTransform
      }
    , { id = 1412
      , nationalDexNumber = 874
      , name = "Stonjourner"
      , form = Original
      , typing = Single Rock
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1479
      , nationalDexNumber = 875
      , name = "Eiscue"
      , form = Original
      , typing = Single Ice
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 23 "When a hailstorm starts"
      }
    , { id = 1480
      , nationalDexNumber = 875
      , name = "Eiscue"
      , form = Unique "Noice"
      , typing = Single Ice
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 23 "When hit by a physical move"
      }
    , { id = 1477
      , nationalDexNumber = 876
      , name = "Indeedee"
      , form = Original
      , typing = Double Psychic Normal
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1426
      , nationalDexNumber = 877
      , name = "Morpeko"
      , form = Unique "Full Belly"
      , typing = Double Electric Dark
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 29 "At the end of a turn"
      }
    , { id = 1742
      , nationalDexNumber = 877
      , name = "Morpeko"
      , form = Unique "Hangry"
      , typing = Double Electric Dark
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 29 "At the end of a turn"
      }
    , { id = 1475
      , nationalDexNumber = 878
      , name = "Cufant"
      , form = Original
      , typing = Single Steel
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1476
      , nationalDexNumber = 879
      , name = "Copperajah"
      , form = Original
      , typing = Single Steel
      , ability = Nothing
      , evolutionData = EvolvesFrom 1475 "Level 34"
      , transformationData = DoesNotTransform
      }
    , { id = 1418
      , nationalDexNumber = 880
      , name = "Dracozolt"
      , form = Original
      , typing = Double Electric Dragon
      , ability = Just VoltAbsorb
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1420
      , nationalDexNumber = 881
      , name = "Arctozolt"
      , form = Original
      , typing = Double Electric Ice
      , ability = Just VoltAbsorb
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1419
      , nationalDexNumber = 882
      , name = "Dracovish"
      , form = Original
      , typing = Double Water Dragon
      , ability = Just WaterAbsorb
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1422
      , nationalDexNumber = 883
      , name = "Arctovish"
      , form = Original
      , typing = Double Water Ice
      , ability = Just WaterAbsorb
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1424
      , nationalDexNumber = 884
      , name = "Duraludon"
      , form = Original
      , typing = Double Steel Dragon
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1472
      , nationalDexNumber = 885
      , name = "Dreepy"
      , form = Original
      , typing = Double Dragon Ghost
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1473
      , nationalDexNumber = 886
      , name = "Drakloak"
      , form = Original
      , typing = Double Dragon Ghost
      , ability = Nothing
      , evolutionData = EvolvesFrom 1472 "Level 50"
      , transformationData = DoesNotTransform
      }
    , { id = 1474
      , nationalDexNumber = 887
      , name = "Dragapult"
      , form = Original
      , typing = Double Dragon Ghost
      , ability = Nothing
      , evolutionData = EvolvesFrom 1473 "Level 60"
      , transformationData = DoesNotTransform
      }
    , { id = 1448
      , nationalDexNumber = 888
      , name = "Zacian"
      , form = Original
      , typing = Single Fairy
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 24 "If not holding Rusted Sword"
      }
    , { id = 1449
      , nationalDexNumber = 888
      , name = "Zacian"
      , form = Unique "Crowned"
      , typing = Double Fairy Steel
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 24 "While holding Rusted Sword"
      }
    , { id = 1450
      , nationalDexNumber = 889
      , name = "Zamazenta"
      , form = Original
      , typing = Single Fighting
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 25 "If not holding Rusted Shield"
      }
    , { id = 1451
      , nationalDexNumber = 889
      , name = "Zamazenta"
      , form = Unique "Crowned"
      , typing = Double Fighting Steel
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 25 "While holding Rusted Shield"
      }
    , { id = 1452
      , nationalDexNumber = 890
      , name = "Eternatus"
      , form = Original
      , typing = Double Poison Dragon
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 26 "When owned by the player"
      }
    , { id = 1461
      , nationalDexNumber = 890
      , name = "Eternatus"
      , form = Unique "Eternamax"
      , typing = Double Poison Dragon
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 26 "During the final battle in Pokémon Sword and Shield"
      }
    , { id = 38
      , nationalDexNumber = 891
      , name = "Kubfu"
      , form = Original
      , typing = Single Fighting
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 39
      , nationalDexNumber = 892
      , name = "Urshifu"
      , form = Unique "Single Strike"
      , typing = Double Fighting Dark
      , ability = Nothing
      , evolutionData = EvolvesFrom 38 "Conquer the Tower of Darkness in Galar's Isle of Armor"
      , transformationData = DoesNotTransform
      }
    , { id = 40
      , nationalDexNumber = 892
      , name = "Urshifu"
      , form = Unique "Rapid Strike"
      , typing = Double Fighting Water
      , ability = Nothing
      , evolutionData = EvolvesFrom 38 "Conquer the Tower of Waters in Galar's Isle of Armor"
      , transformationData = DoesNotTransform
      }
    , { id = 1597
      , nationalDexNumber = 893
      , name = "Zarude"
      , form = Original
      , typing = Double Dark Grass
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1598
      , nationalDexNumber = 894
      , name = "Regieleki"
      , form = Original
      , typing = Single Electric
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1599
      , nationalDexNumber = 895
      , name = "Regidrago"
      , form = Original
      , typing = Single Dragon
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1602
      , nationalDexNumber = 896
      , name = "Glastrier"
      , form = Original
      , typing = Single Ice
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1603
      , nationalDexNumber = 897
      , name = "Spectrier"
      , form = Original
      , typing = Single Ghost
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 41
      , nationalDexNumber = 898
      , name = "Calyrex"
      , form = Original
      , typing = Double Psychic Grass
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 27 "Undo fusion"
      }
    , { id = 1604
      , nationalDexNumber = 898
      , name = "Calyrex"
      , form = Unique "Ice Rider"
      , typing = Double Psychic Ice
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 27 "Fuse with Glastrier"
      }
    , { id = 1605
      , nationalDexNumber = 898
      , name = "Calyrex"
      , form = Unique "Shadow Rider"
      , typing = Double Psychic Ghost
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 27 "Fuse with Spectrier"
      }
    , { id = 1617
      , nationalDexNumber = 905
      , name = "Enamorus"
      , form = Original
      , typing = Double Fairy Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 28 "Use the Reveal Glass"
      }
    , { id = 1618
      , nationalDexNumber = 905
      , name = "Enamorus"
      , form = Unique "Therian"
      , typing = Double Fairy Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = Transforms 28 "Use the Reveal Glass"
      }
    , { id = 1633
      , nationalDexNumber = 906
      , name = "Sprigatito"
      , form = Original
      , typing = Single Grass
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1634
      , nationalDexNumber = 907
      , name = "Floragato"
      , form = Original
      , typing = Single Grass
      , ability = Nothing
      , evolutionData = EvolvesFrom 1633 "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 1635
      , nationalDexNumber = 908
      , name = "Meowscarada"
      , form = Original
      , typing = Double Grass Dark
      , ability = Nothing
      , evolutionData = EvolvesFrom 1634 "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 1636
      , nationalDexNumber = 909
      , name = "Fuecoco"
      , form = Original
      , typing = Single Fire
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1637
      , nationalDexNumber = 910
      , name = "Crocalor"
      , form = Original
      , typing = Single Fire
      , ability = Nothing
      , evolutionData = EvolvesFrom 1636 "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 1638
      , nationalDexNumber = 911
      , name = "Skeledirge"
      , form = Original
      , typing = Double Fire Ghost
      , ability = Nothing
      , evolutionData = EvolvesFrom 1637 "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 1639
      , nationalDexNumber = 912
      , name = "Quaxly"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1640
      , nationalDexNumber = 913
      , name = "Quaxwell"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = EvolvesFrom 1639 "Level 16"
      , transformationData = DoesNotTransform
      }
    , { id = 1641
      , nationalDexNumber = 914
      , name = "Quaquaval"
      , form = Original
      , typing = Double Water Fighting
      , ability = Nothing
      , evolutionData = EvolvesFrom 1640 "Level 36"
      , transformationData = DoesNotTransform
      }
    , { id = 1642
      , nationalDexNumber = 915
      , name = "Lechonk"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1643
      , nationalDexNumber = 916
      , name = "Oinkologne"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = EvolvesFrom 1642 "Level 18"
      , transformationData = DoesNotTransform
      }
    , { id = 1644
      , nationalDexNumber = 917
      , name = "Tarountula"
      , form = Original
      , typing = Single Bug
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1645
      , nationalDexNumber = 918
      , name = "Spidops"
      , form = Original
      , typing = Single Bug
      , ability = Nothing
      , evolutionData = EvolvesFrom 1644 "Level 15"
      , transformationData = DoesNotTransform
      }
    , { id = 1646
      , nationalDexNumber = 919
      , name = "Nymble"
      , form = Original
      , typing = Single Bug
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1647
      , nationalDexNumber = 920
      , name = "Lokix"
      , form = Original
      , typing = Double Bug Dark
      , ability = Nothing
      , evolutionData = EvolvesFrom 1646 "Level 24"
      , transformationData = DoesNotTransform
      }
    , { id = 1648
      , nationalDexNumber = 921
      , name = "Pawmi"
      , form = Original
      , typing = Single Electric
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1649
      , nationalDexNumber = 922
      , name = "Pawmo"
      , form = Original
      , typing = Double Electric Fighting
      , ability = Just VoltAbsorb
      , evolutionData = EvolvesFrom 1648 "Level 18"
      , transformationData = DoesNotTransform
      }
    , { id = 1650
      , nationalDexNumber = 923
      , name = "Pawmot"
      , form = Original
      , typing = Double Electric Fighting
      , ability = Just VoltAbsorb
      , evolutionData = EvolvesFrom 1649 "While outside of its Poké Ball after walking 1000 steps using the Let's Go feature"
      , transformationData = DoesNotTransform
      }
    , { id = 1653
      , nationalDexNumber = 924
      , name = "Tandemaus"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1654
      , nationalDexNumber = 925
      , name = "Maushold"
      , form = Original
      , typing = Single Normal
      , ability = Nothing
      , evolutionData = EvolvesFrom 1653 "Level 25 while battling"
      , transformationData = DoesNotTransform
      }
    , { id = 1655
      , nationalDexNumber = 926
      , name = "Fidough"
      , form = Original
      , typing = Single Fairy
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1656
      , nationalDexNumber = 927
      , name = "Dachsbun"
      , form = Original
      , typing = Single Fairy
      , ability = Nothing
      , evolutionData = EvolvesFrom 1655 "Level 26"
      , transformationData = DoesNotTransform
      }
    , { id = 1657
      , nationalDexNumber = 928
      , name = "Smoliv"
      , form = Original
      , typing = Double Grass Normal
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1658
      , nationalDexNumber = 929
      , name = "Dolliv"
      , form = Original
      , typing = Double Grass Normal
      , ability = Nothing
      , evolutionData = EvolvesFrom 1657 "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 1659
      , nationalDexNumber = 930
      , name = "Arboliva"
      , form = Original
      , typing = Double Grass Normal
      , ability = Nothing
      , evolutionData = EvolvesFrom 1658 "Level 35"
      , transformationData = DoesNotTransform
      }
    , { id = 1660
      , nationalDexNumber = 931
      , name = "Squawkabilly"
      , form = Original
      , typing = Double Normal Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1661
      , nationalDexNumber = 932
      , name = "Nacli"
      , form = Original
      , typing = Single Rock
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1662
      , nationalDexNumber = 933
      , name = "Naclstack"
      , form = Original
      , typing = Single Rock
      , ability = Nothing
      , evolutionData = EvolvesFrom 1661 "Level 24"
      , transformationData = DoesNotTransform
      }
    , { id = 1663
      , nationalDexNumber = 934
      , name = "Garganacl"
      , form = Original
      , typing = Single Rock
      , ability = Nothing
      , evolutionData = EvolvesFrom 1662 "Level 38"
      , transformationData = DoesNotTransform
      }
    , { id = 1665
      , nationalDexNumber = 935
      , name = "Charcadet"
      , form = Original
      , typing = Single Fire
      , ability = Just FlashFire
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1666
      , nationalDexNumber = 936
      , name = "Armarouge"
      , form = Original
      , typing = Double Fire Psychic
      , ability = Just FlashFire
      , evolutionData = EvolvesFrom 1665 "Use Auspicious Armor"
      , transformationData = DoesNotTransform
      }
    , { id = 1667
      , nationalDexNumber = 937
      , name = "Ceruledge"
      , form = Original
      , typing = Double Fire Ghost
      , ability = Just FlashFire
      , evolutionData = EvolvesFrom 1665 "Use Malicious Armor"
      , transformationData = DoesNotTransform
      }
    , { id = 1668
      , nationalDexNumber = 938
      , name = "Tadbulb"
      , form = Original
      , typing = Single Electric
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1669
      , nationalDexNumber = 939
      , name = "Bellibolt"
      , form = Original
      , typing = Single Electric
      , ability = Nothing
      , evolutionData = EvolvesFrom 1668 "Use Thunder Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 1670
      , nationalDexNumber = 940
      , name = "Wattrel"
      , form = Original
      , typing = Double Electric Flying
      , ability = Just VoltAbsorb
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1671
      , nationalDexNumber = 941
      , name = "Kilowattrel"
      , form = Original
      , typing = Double Electric Flying
      , ability = Just VoltAbsorb
      , evolutionData = EvolvesFrom 1670 "Level 25"
      , transformationData = DoesNotTransform
      }
    , { id = 1674
      , nationalDexNumber = 942
      , name = "Maschiff"
      , form = Original
      , typing = Single Dark
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1675
      , nationalDexNumber = 943
      , name = "Mabosstiff"
      , form = Original
      , typing = Single Dark
      , ability = Nothing
      , evolutionData = EvolvesFrom 1674 "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 1676
      , nationalDexNumber = 944
      , name = "Shroodle"
      , form = Original
      , typing = Double Poison Normal
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1677
      , nationalDexNumber = 945
      , name = "Grafaiai"
      , form = Original
      , typing = Double Poison Normal
      , ability = Nothing
      , evolutionData = EvolvesFrom 1676 "Level 28"
      , transformationData = DoesNotTransform
      }
    , { id = 1681
      , nationalDexNumber = 946
      , name = "Bramblin"
      , form = Original
      , typing = Double Grass Ghost
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1682
      , nationalDexNumber = 947
      , name = "Brambleghast"
      , form = Original
      , typing = Double Grass Ghost
      , ability = Nothing
      , evolutionData = EvolvesFrom 1681 "While outside of its Poké Ball after walking 1000 steps using the Let's Go feature"
      , transformationData = DoesNotTransform
      }
    , { id = 1683
      , nationalDexNumber = 948
      , name = "Toedscool"
      , form = Original
      , typing = Double Ground Grass
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1684
      , nationalDexNumber = 949
      , name = "Toedscruel"
      , form = Original
      , typing = Double Ground Grass
      , ability = Nothing
      , evolutionData = EvolvesFrom 1683 "Level 30"
      , transformationData = DoesNotTransform
      }
    , { id = 1685
      , nationalDexNumber = 950
      , name = "Klawf"
      , form = Original
      , typing = Single Rock
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1686
      , nationalDexNumber = 951
      , name = "Capsakid"
      , form = Original
      , typing = Single Grass
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1687
      , nationalDexNumber = 952
      , name = "Scovillain"
      , form = Original
      , typing = Double Grass Fire
      , ability = Nothing
      , evolutionData = EvolvesFrom 1686 "Use Fire Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 1688
      , nationalDexNumber = 953
      , name = "Rellor"
      , form = Original
      , typing = Single Bug
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1689
      , nationalDexNumber = 954
      , name = "Rabsca"
      , form = Original
      , typing = Double Bug Psychic
      , ability = Nothing
      , evolutionData = EvolvesFrom 1688 "While outside of its Poké Ball after walking 1000 steps using the Let's Go feature"
      , transformationData = DoesNotTransform
      }
    , { id = 1690
      , nationalDexNumber = 955
      , name = "Flittle"
      , form = Original
      , typing = Single Psychic
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1691
      , nationalDexNumber = 956
      , name = "Espathra"
      , form = Original
      , typing = Single Psychic
      , ability = Nothing
      , evolutionData = EvolvesFrom 1690 "Level 35"
      , transformationData = DoesNotTransform
      }
    , { id = 1692
      , nationalDexNumber = 957
      , name = "Tinkatink"
      , form = Original
      , typing = Double Fairy Steel
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1693
      , nationalDexNumber = 958
      , name = "Tinkatuff"
      , form = Original
      , typing = Double Fairy Steel
      , ability = Nothing
      , evolutionData = EvolvesFrom 1692 "Level 24"
      , transformationData = DoesNotTransform
      }
    , { id = 1694
      , nationalDexNumber = 959
      , name = "Tinkaton"
      , form = Original
      , typing = Double Fairy Steel
      , ability = Nothing
      , evolutionData = EvolvesFrom 1693 "Level 38"
      , transformationData = DoesNotTransform
      }
    , { id = 1695
      , nationalDexNumber = 960
      , name = "Wiglett"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1696
      , nationalDexNumber = 961
      , name = "Wugtrio"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = EvolvesFrom 1695 "Level 26"
      , transformationData = DoesNotTransform
      }
    , { id = 1697
      , nationalDexNumber = 962
      , name = "Bombirdier"
      , form = Original
      , typing = Double Flying Dark
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1698
      , nationalDexNumber = 963
      , name = "Finizen"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1699
      , nationalDexNumber = 964
      , name = "Palafin"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = EvolvesFrom 1698 "Level 38 while in the Union Circle with another player"
      , transformationData = DoesNotTransform
      }
    , { id = 1700
      , nationalDexNumber = 965
      , name = "Varoom"
      , form = Original
      , typing = Double Steel Poison
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1701
      , nationalDexNumber = 966
      , name = "Revavroom"
      , form = Original
      , typing = Double Steel Poison
      , ability = Nothing
      , evolutionData = EvolvesFrom 1700 "Level 40"
      , transformationData = DoesNotTransform
      }
    , { id = 1702
      , nationalDexNumber = 967
      , name = "Cyclizar"
      , form = Original
      , typing = Double Dragon Normal
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1703
      , nationalDexNumber = 968
      , name = "Orthworm"
      , form = Original
      , typing = Single Steel
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1704
      , nationalDexNumber = 969
      , name = "Glimmet"
      , form = Original
      , typing = Double Rock Poison
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1705
      , nationalDexNumber = 970
      , name = "Glimmora"
      , form = Original
      , typing = Double Rock Poison
      , ability = Nothing
      , evolutionData = EvolvesFrom 1704 "Level 35"
      , transformationData = DoesNotTransform
      }
    , { id = 1706
      , nationalDexNumber = 971
      , name = "Greavard"
      , form = Original
      , typing = Single Ghost
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1707
      , nationalDexNumber = 972
      , name = "Houndstone"
      , form = Original
      , typing = Single Ghost
      , ability = Nothing
      , evolutionData = EvolvesFrom 1706 "Level 30 at night"
      , transformationData = DoesNotTransform
      }
    , { id = 1708
      , nationalDexNumber = 973
      , name = "Flamigo"
      , form = Original
      , typing = Double Flying Fighting
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1709
      , nationalDexNumber = 974
      , name = "Cetoddle"
      , form = Original
      , typing = Single Ice
      , ability = Just ThickFat
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1710
      , nationalDexNumber = 975
      , name = "Cetitan"
      , form = Original
      , typing = Single Ice
      , ability = Just ThickFat
      , evolutionData = EvolvesFrom 1709 "Use Ice Stone"
      , transformationData = DoesNotTransform
      }
    , { id = 1712
      , nationalDexNumber = 976
      , name = "Veluza"
      , form = Original
      , typing = Double Water Psychic
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1713
      , nationalDexNumber = 977
      , name = "Dondozo"
      , form = Original
      , typing = Single Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1714
      , nationalDexNumber = 978
      , name = "Tatsugiri"
      , form = Original
      , typing = Double Dragon Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1715
      , nationalDexNumber = 984
      , name = "Great Tusk"
      , form = Original
      , typing = Double Ground Fighting
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1716
      , nationalDexNumber = 985
      , name = "Scream Tail"
      , form = Original
      , typing = Double Fairy Psychic
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1717
      , nationalDexNumber = 986
      , name = "Brute Bonnet"
      , form = Original
      , typing = Double Grass Dark
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1718
      , nationalDexNumber = 987
      , name = "Flutter Mane"
      , form = Original
      , typing = Double Ghost Fairy
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1719
      , nationalDexNumber = 988
      , name = "Slither Wing"
      , form = Original
      , typing = Double Bug Fighting
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1720
      , nationalDexNumber = 989
      , name = "Sandy Shocks"
      , form = Original
      , typing = Double Electric Ground
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1721
      , nationalDexNumber = 990
      , name = "Iron Treads"
      , form = Original
      , typing = Double Ground Steel
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1722
      , nationalDexNumber = 991
      , name = "Iron Bundle"
      , form = Original
      , typing = Double Ice Water
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1723
      , nationalDexNumber = 992
      , name = "Iron Hands"
      , form = Original
      , typing = Double Fighting Electric
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1724
      , nationalDexNumber = 993
      , name = "Iron Jugulis"
      , form = Original
      , typing = Double Dark Flying
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1725
      , nationalDexNumber = 994
      , name = "Iron Moth"
      , form = Original
      , typing = Double Fire Poison
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1726
      , nationalDexNumber = 995
      , name = "Iron Thorns"
      , form = Original
      , typing = Double Rock Electric
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1727
      , nationalDexNumber = 996
      , name = "Frigibax"
      , form = Original
      , typing = Double Dragon Ice
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1728
      , nationalDexNumber = 997
      , name = "Arctibax"
      , form = Original
      , typing = Double Dragon Ice
      , ability = Nothing
      , evolutionData = EvolvesFrom 1727 "Level 35"
      , transformationData = DoesNotTransform
      }
    , { id = 1729
      , nationalDexNumber = 998
      , name = "Baxcalibur"
      , form = Original
      , typing = Double Dragon Ice
      , ability = Nothing
      , evolutionData = EvolvesFrom 1728 "Level 54"
      , transformationData = DoesNotTransform
      }
    , { id = 1730
      , nationalDexNumber = 999
      , name = "Gimmighoul"
      , form = Original
      , typing = Single Ghost
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1731
      , nationalDexNumber = 1000
      , name = "Gholdengo"
      , form = Original
      , typing = Double Ghost Steel
      , ability = Nothing
      , evolutionData = EvolvesFrom 1730 "Level while having 999 Ghimmighoul Coins"
      , transformationData = DoesNotTransform
      }
    , { id = 1732
      , nationalDexNumber = 1001
      , name = "Wo-Chien"
      , form = Original
      , typing = Double Dark Grass
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1733
      , nationalDexNumber = 1002
      , name = "Chien-Pao"
      , form = Original
      , typing = Double Dark Ice
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1734
      , nationalDexNumber = 1003
      , name = "Ting-Lu"
      , form = Original
      , typing = Double Dark Ground
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1735
      , nationalDexNumber = 1004
      , name = "Chi-Yu"
      , form = Original
      , typing = Double Dark Fire
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1736
      , nationalDexNumber = 1005
      , name = "Roaring Moon"
      , form = Original
      , typing = Double Dragon Dark
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1737
      , nationalDexNumber = 1006
      , name = "Iron Valiant"
      , form = Original
      , typing = Double Fighting Fairy
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1738
      , nationalDexNumber = 1007
      , name = "Koraidon"
      , form = Original
      , typing = Double Fighting Dragon
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    , { id = 1739
      , nationalDexNumber = 1008
      , name = "Miraidon"
      , form = Original
      , typing = Double Electric Dragon
      , ability = Nothing
      , evolutionData = IsNotEvolved
      , transformationData = DoesNotTransform
      }
    ]
